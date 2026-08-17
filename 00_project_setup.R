# 00_project_setup.R

# 1. Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, DBI, RPostgres, scales, ipumsr,readxl,usmap,gghighlight,styler)

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
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey30", size = 11),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Common vertical lines for important years
important_years_lines <- geom_vline(
  xintercept = c(2006, 2008, 2015, 2017, 2020), 
  linetype = "dotted", 
  color = "red"
)

# 4. Reusable Function for LFPR Calculation
# Uses ifelse() instead of subsetting [ ] to ensure perfect translation to SQL
calculate_lfpr <- function(db_tbl, ...) {
  db_tbl %>%
    filter(..., AGE >= 16, EMPSTAT != 0, !(EMPSTATD %in% 13:15), GQ != 3) %>%
    group_by(YEAR) %>%
    summarise(
      total_pop = sum(ifelse(EMPSTAT %in% c(1, 2, 3), PERWT, 0), na.rm = TRUE),
      in_labfor = sum(ifelse(EMPSTAT %in% c(1, 2), PERWT, 0), na.rm = TRUE),
      out_labfor = sum(ifelse(EMPSTAT == 3, PERWT, 0), na.rm = TRUE),
      unemp = sum(ifelse(EMPSTAT == 2, PERWT, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      labfor_rate = (in_labfor / total_pop) * 100,
      out_labfor_pct = (out_labfor / total_pop) * 100,
      unemp_rate = (unemp / in_labfor) * 100,
      etop_ratio = (in_labfor-unemp)/total_pop * 100
    ) %>%
    collect()
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