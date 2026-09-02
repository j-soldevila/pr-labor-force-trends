# 00_project_setup.R

# 1. Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, DBI, RPostgres, scales, ipumsr,readxl,usmap,
               gghighlight,styler,Hmisc)

# 2. Database Connection
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'pr_labor_proj',
  host = "localhost",
  port = 5432,
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# Load base tables
data_prcs <- tbl(con, "data_prcs")
data_acs <- tbl(con, "data_acs")
data_hist <- tbl(con, "data_acs_decennial")

# 3. Custom Plotting Theme
my_theme <- theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, color = "grey30"),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.caption = element_text(size = 10, color = "grey50", hjust = 1)
  )

# Common vertical lines for important years
important_years_lines <- geom_vline(
  xintercept = c(2006, 2008, 2015, 2017, 2020), 
  linetype = "dotted", 
  color = "red"
)

# 4. Reusable Function for LFPR Calculation
calculate_lfpr <- function(db_tbl, pr_filter = FALSE, grp_vars = NULL) {
  
  # Base filtering (includes GQ != 3 to remove institutionalized pop)
  q <- db_tbl %>%
    filter(AGE >= 16, EMPSTAT != 0, !(EMPSTATD %in% 13:15), GQ != 3)
  
  # Toggle PR filter
  if (pr_filter) {
    q <- q %>% filter(HISPAN == 2 | HISPAND == 200 | ANCESTR1 == 261)
  }
  
  # Apply dynamic grouping if demographic variables are provided
  if (!is.null(grp_vars)) {
    q <- q %>% group_by(across(all_of(c("YEAR", grp_vars))))
  } else {
    q <- q %>% group_by(YEAR)
  }
  
  # Calculate metrics
  q %>%
    summarise(
      total_pop = sum(ifelse(EMPSTAT %in% c(1, 2, 3), PERWT, 0), na.rm = TRUE),
      in_labfor = sum(ifelse(EMPSTAT %in% c(1, 2), PERWT, 0), na.rm = TRUE),
      out_labfor = sum(ifelse(EMPSTAT == 3, PERWT, 0), na.rm = TRUE),
      unemp = sum(ifelse(EMPSTAT == 2, PERWT, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    collect() %>%
    mutate(
      labfor_rate = (in_labfor / total_pop) * 100,
      out_labfor_pct = (out_labfor / total_pop) * 100,
      unemp_rate = (unemp / in_labfor) * 100,
      etop_ratio = (in_labfor - unemp) / total_pop * 100
    ) 
}

# Define age standard weights (You will need to compute these based on your total data_acs pool)
# Example weights based on a hypothetical standard population distribution:
standard_weights <- tibble(
  age_bucket = c("16-24", "25-54", "55+"),
  std_weight = c(0.15, 0.55, 0.30) 
)

calculate_age_adjusted_lfpr <- function(db_tbl, ...) {
  db_tbl %>%
    # Use the same base filters as your original function
    filter(..., AGE >= 16, EMPSTAT != 0, !(EMPSTATD %in% 13:15), GQ != 3) %>%
    mutate(
      age_bucket = case_when(
        AGE >= 16 & AGE <= 24 ~ "16-24",
        AGE >= 25 & AGE <= 54 ~ "25-54",
        AGE >= 55 ~ "55+"
      )
    ) %>%
    # Group by YEAR and age_bucket
    group_by(YEAR, age_bucket) %>%
    summarise(
      total_pop = sum(ifelse(EMPSTAT %in% c(1, 2, 3), PERWT, 0), na.rm = TRUE),
      in_labfor = sum(ifelse(EMPSTAT %in% c(1, 2), PERWT, 0), na.rm = TRUE),
      out_labfor = sum(ifelse(EMPSTAT == 3, PERWT, 0), na.rm = TRUE),
      unemp = sum(ifelse(EMPSTAT == 2, PERWT, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    collect() %>%
    
    # Perform the age adjustment locally
    mutate(age_specific_lfpr = in_labfor / total_pop,
           age_specific_etop = (in_labfor - unemp) / total_pop
           ) %>%
    
    left_join(standard_weights, by = "age_bucket") %>%
    
    group_by(YEAR) %>%
    summarise(
      crude_lfpr = (sum(in_labfor) / sum(total_pop)) * 100,
      crude_etop = (sum(in_labfor - unemp) / sum(total_pop)) * 100,
      crude_unemp = (sum(unemp) / sum(in_labfor)) * 100,
      
      adjusted_lfpr = sum(age_specific_lfpr * std_weight) * 100,
      adjusted_etop = sum(age_specific_etop * std_weight) * 100,
      .groups = "drop"
    )
}

# Custom Reusable Function for Poverty Calculation
calculate_poverty_rate <- function(db_tbl, pr_filter = FALSE, grp_vars = NULL) {
  
  # Base filtering: Remove 0, which represents N/A or institutionalized pop not in the poverty universe in IPUMS
  q <- db_tbl %>%
    filter(POVERTY != 0)
  
  # Toggle PR filter[cite: 1]
  if (pr_filter) {
    q <- q %>% filter(HISPAN == 2 | HISPAND == 200 | ANCESTR1 == 261)
  }
  
  # Apply dynamic grouping if demographic variables are provided[cite: 1]
  if (!is.null(grp_vars)) {
    q <- q %>% group_by(across(all_of(c("YEAR", grp_vars))))
  } else {
    q <- q %>% group_by(YEAR)
  }
  
  # Calculate metrics using person weight (PERWT)[cite: 1]
  q %>%
    summarise(
      total_pop_poverty_universe = sum(PERWT, na.rm = TRUE),
      # In IPUMS, POVERTY < 100 means the person's family income is below the poverty threshold
      in_poverty = sum(ifelse(POVERTY < 100, PERWT, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      poverty_rate = (in_poverty / total_pop_poverty_universe) * 100
    ) %>%
    collect()
}
