02_LFPR_analysis
================
Jorge R. Soldevila Irizarry
2026-07-15

``` r
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, DBI, RPostgres, scales)
```

``` r
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'pr_labor_proj',
  host = "localhost",
  port = 5432,
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

prcs_06_24 <- tbl(con, "data_prcs")
acs_01_24  <- tbl(con, "data_acs")
```

# Labor Force Participation Rate for Puerto Ricans in the U.S.

``` r
#To perform age group analysis we can update our data base tables using SQL syntax within the RStudio environment. 

#First we need to create a new variable to host the recoded age variable. 
#dbExecute(con, 'ALTER TABLE "data_acs" ADD COLUMN age_grp VARCHAR(20);')

#Using SQL Update
#dbExecute(con,r"(
#          UPDATE "data_acs"
#          SET age_grp = CASE
#            WHEN "AGE" < 16 THEN 'minor'
#            WHEN "AGE" BETWEEN 16 AND 24 THEN '16 to 24'
#            WHEN "AGE" BETWEEN 25 AND 54 THEN '25 to 54'
#            ELSE '55+'
#          END;
#          )"
#          )

#Because we updated the table in our data base, we need to establish a new connection with the updated table. We'll give it the same name and re-write the old connection.  
#acs_01_24 <- tbl(con, "data_acs")
```

## To understand the participation of Puerto Rican in the Labor Force in the U.S. we first evaluate their participation rate and compare to the overall population.

``` r
pr_lf_rate_us <- acs_01_24 %>%
  filter(HISPAN == 2 | HISPAND == 200 | ANCESTR1 == 261,
         AGE >= 16,
         EMPSTAT != 0,
         !(EMPSTATD %in% 13:15),
         GQ != 3) %>%
  group_by(YEAR) %>%
  summarise(total_pop = sum(PERWT[EMPSTAT %in% c(1, 2, 3)], na.rm = TRUE),
            in_labfor = sum(PERWT[EMPSTAT %in% c(1, 2)], na.rm = TRUE),
            out_labfor = sum(PERWT[EMPSTAT == 3], na.rm = TRUE),
            unemp = sum(PERWT[EMPSTAT == 2], na.rm = TRUE))%>%
  mutate(labfor_rate = in_labfor/total_pop*100,
         out_labfor_pct = out_labfor/total_pop*100,
         unemp_rate = unemp/in_labfor*100) %>%
  collect()
```

## We can also calculate the participation rate for the total population of the U.S. for comparison analysis.

``` r
lf_rate_us <- acs_01_24 %>%
  filter(AGE >= 16,
         EMPSTAT != 0,
         !(EMPSTATD %in% 13:15),
         GQ != 3) %>%
  group_by(YEAR) %>%
  summarise(total_pop = sum(PERWT[EMPSTAT %in% c(1, 2, 3)], na.rm = TRUE),
            in_labfor = sum(PERWT[EMPSTAT %in% c(1, 2)], na.rm = TRUE),
            out_labfor = sum(PERWT[EMPSTAT == 3], na.rm = TRUE),
            unemp = sum(PERWT[EMPSTAT == 2], na.rm = TRUE))%>%
  mutate(labfor_rate = in_labfor/total_pop*100,
         out_labfor_pct = out_labfor/total_pop*100,
         unemp_rate = unemp/in_labfor*100) %>%
  collect()
```

## We can also calculate the participation rate for the total population in

## Puerto Rico.

``` r
lf_rate_pr <- prcs_06_24 %>%
  filter(AGE >= 16,
         EMPSTAT != 0,
         !(EMPSTATD %in% 13:15),
         GQ != 3) %>%
  group_by(YEAR) %>%
  summarise(total_pop = sum(PERWT[EMPSTAT %in% c(1, 2, 3)], na.rm = TRUE),
            in_labfor = sum(PERWT[EMPSTAT %in% c(1, 2)], na.rm = TRUE),
            out_labfor = sum(PERWT[EMPSTAT == 3], na.rm = TRUE),
            unemp = sum(PERWT[EMPSTAT == 2], na.rm = TRUE))%>%
  mutate(labfor_rate = in_labfor/total_pop*100,
         out_labfor_pct = out_labfor/total_pop*100,
         unemp_rate = unemp/in_labfor*100) %>%
  collect()
```

## We can visualize the trends in Labor Force Participation Rate and Unemployment rate for Puerto Ricans in the U.S. to better understand it’s behavior.

``` r
pr_lf_us_graph <- ggplot() +
  geom_line(data = pr_lf_rate_us, aes(x = YEAR, y = labfor_rate),
            color = "blue") +
  geom_point(data = pr_lf_rate_us, aes(x = YEAR, y = labfor_rate),
            color = "blue") +
  geom_text(data = subset(pr_lf_rate_us, YEAR %in% selected_years),
            aes(x = YEAR, y = labfor_rate, label = sprintf("%.1f",labfor_rate), 
                vjust = -1)) +
  geom_line(data = lf_rate_us, aes(x = YEAR, y = labfor_rate),
            color = "orange") +
  geom_point(data = lf_rate_us, aes(x = YEAR, y = labfor_rate),
            color = "orange") +
  geom_line(data = lf_rate_pr, aes(x = YEAR, y = labfor_rate),
            color = "lightblue") +
  geom_point(data = lf_rate_pr, aes(x = YEAR, y = labfor_rate),
            color = "lightblue",
            linetype = 2) +
  geom_text(data = subset(lf_rate_pr, YEAR %in% selected_years),
            aes(x = YEAR, y = labfor_rate, label = sprintf("%.1f",labfor_rate), 
                vjust = -1)) +
  geom_vline(xintercept = c(2006,2008,2015,2017,2020),linetype = "dotted",
             color = "red") +
  theme_bw() +
  labs(title = "Trends in Puerto Rican Labor Force Participation in the U.S. 2001-2024",
       x = "Year",
       y = "Rate")
print(pr_lf_us_graph)
```

![](labfor_analysis_files/figure-gfm/PR%20in%20Us%20Labor%20Force%20Visualization-1.png)<!-- -->

``` r
ggplot() +
  geom_line(data = pr_lf_rate_us, aes(x = YEAR, y = unemp_rate),
            color = "blue") +
  geom_point(data = pr_lf_rate_us, aes(x = YEAR, y = unemp_rate),
            color = "blue") +
  geom_text(data = subset(pr_lf_rate_us, YEAR %in% selected_years),
            aes(x = YEAR, y = unemp_rate, label = sprintf("%.1f",unemp_rate), 
                vjust = -1)) +
  geom_line(data = lf_rate_us, aes(x = YEAR, y = unemp_rate),
            color = "orange") +
  geom_point(data = lf_rate_us, aes(x = YEAR, y = unemp_rate),
            color = "orange") +
  geom_line(data = lf_rate_pr, aes(x = YEAR, y = unemp_rate),
            color = "lightblue") +
  geom_point(data = lf_rate_pr, aes(x = YEAR, y = unemp_rate),
            color = "lightblue",
            linetype = 2) +
  geom_text(data = subset(lf_rate_pr, YEAR %in% selected_years),
            aes(x = YEAR, y = unemp_rate, label = sprintf("%.1f",unemp_rate), 
                vjust = -1)) +
  geom_vline(xintercept = c(2006,2008,2015,2017,2020),linetype = "dotted",
             color = "red") +
  theme_bw() +
  labs(title = "Trends in Puerto Rican Unemployment Rate in the U.S. 2001-2024",
       x = "Year",
       y = "Rate")
```

![](labfor_analysis_files/figure-gfm/PR%20in%20Us%20Unemployment%20Visualization-1.png)<!-- -->

# Labor Force Participation Rate by Age Groups

``` r
#To perform age group analysis we can update our data base tables using SQL syntax within the RStudio environment. 

#First we need to create a new variable to host the recoded age variable. 
#dbExecute(con, 'ALTER TABLE "data_acs" ADD COLUMN age_grp VARCHAR(20);')

#Using SQL Update
#dbExecute(con,r"(
#          UPDATE "data_acs"
#          SET age_grp = CASE
#            WHEN "AGE" < 16 THEN 'minor'
#            WHEN "AGE" BETWEEN 16 AND 24 THEN '16 to 24'
#            WHEN "AGE" BETWEEN 25 AND 54 THEN '25 to 54'
#            ELSE '55+'
#          END;
#          )"
#          )

#Because we updated the table in our data base, we need to establish a new connection with the updated table. We'll give it the same name and re-write the old connection.  
#acs_01_24 <- tbl(con, "data_acs")
```

### Having created a new variable that groups the working age population into three major groups, we can perform a similar analysis as before to create comparisons between age groups among the Puerto Rican population and vis-a-vis the total population.

``` r
pr_age_lf_rate_us <- acs_01_24 %>%
  filter(HISPAN == 2 | HISPAND == 200 | ANCESTR1 == 261,
         AGE >= 16,
         EMPSTAT != 0,
         !(EMPSTATD %in% 13:15),
         GQ != 3) %>%
  group_by(YEAR,age_grp) %>%
  summarise(total_pop = sum(PERWT[EMPSTAT %in% c(1, 2, 3)], na.rm = TRUE),
            in_labfor = sum(PERWT[EMPSTAT %in% c(1, 2)], na.rm = TRUE),
            unemp = sum(PERWT[EMPSTAT == 2], na.rm = TRUE))%>%
  mutate(labfor_rate = in_labfor/total_pop*100,
         unemp_rate = unemp/in_labfor*100) %>%
  collect()
```

``` r
age_lf_rate_us <- acs_01_24 %>%
  filter(AGE >= 16,
         EMPSTAT != 0,
         !(EMPSTATD %in% 13:15),
         GQ != 3) %>%
  group_by(YEAR,age_grp) %>%
  summarise(total_pop = sum(PERWT[EMPSTAT %in% c(1, 2, 3)], na.rm = TRUE),
            in_labfor = sum(PERWT[EMPSTAT %in% c(1, 2)], na.rm = TRUE),
            unemp = sum(PERWT[EMPSTAT == 2], na.rm = TRUE))%>%
  mutate(labfor_rate = in_labfor/total_pop*100,
         unemp_rate = unemp/in_labfor*100) %>%
  collect()
```

``` r
age_lf_rate_pr <- prcs_06_24 %>%
  filter(AGE >= 16,
         EMPSTAT != 0,
         !(EMPSTATD %in% 13:15),
         GQ != 3) %>%
  group_by(YEAR,age_grp) %>%
  summarise(total_pop = sum(PERWT[EMPSTAT %in% c(1, 2, 3)], na.rm = TRUE),
            in_labfor = sum(PERWT[EMPSTAT %in% c(1, 2)], na.rm = TRUE),
            unemp = sum(PERWT[EMPSTAT == 2], na.rm = TRUE))%>%
  mutate(labfor_rate = in_labfor/total_pop*100,
         unemp_rate = unemp/in_labfor*100) %>%
  collect()
```

``` r
ggplot(age_lf_rate_us, aes(x = YEAR, y = labfor_rate, color = age_grp)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Labor Force Participation Rate for Total US Pop by Age Group",
    x = "Year",
    y = "Participation Rate (%)",
    color = "Age Group") +
  geom_text(data = subset(age_lf_rate_us, YEAR %in% selected_years),
            aes(x = YEAR, y = labfor_rate, label = sprintf("%.1f",labfor_rate), 
                vjust = -1)) +
  geom_vline(xintercept = c(2006,2008,2015,2017,2020),linetype = "dotted",
             color = "red") +
  theme_bw()
```

![](labfor_analysis_files/figure-gfm/Total%20Age%20Group%20Visualizaing-1.png)<!-- -->

``` r
ggplot(pr_age_lf_rate_us, aes(x = YEAR, y = labfor_rate, color = age_grp)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Labor Force Participation Rate for Puerto Ricans in the US by Age Group",
    x = "Year",
    y = "Participation Rate (%)",
    color = "Age Group") +
  geom_text(data = subset(pr_age_lf_rate_us, YEAR %in% selected_years),
            aes(x = YEAR, y = labfor_rate, label = sprintf("%.1f",labfor_rate), 
                vjust = -1)) +
  geom_vline(xintercept = c(2006,2008,2015,2017,2020),linetype = "dotted",
             color = "red") +
  theme_bw()
```

![](labfor_analysis_files/figure-gfm/PR%20Age%20Group%20Visualizaing%202-1.png)<!-- -->

``` r
ggplot(age_lf_rate_pr, aes(x = YEAR, y = labfor_rate, color = age_grp)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Labor Force Participation Rate in Puerto Rico by Age Group",
    x = "Year",
    y = "Participation Rate (%)",
    color = "Age Group") +
  geom_text(data = subset(age_lf_rate_pr, YEAR %in% selected_years),
            aes(x = YEAR, y = labfor_rate, label = sprintf("%.1f",labfor_rate), 
                vjust = -1)) +
  geom_vline(xintercept = c(2006,2008,2015,2017,2020),linetype = "dotted",
             color = "red") +
  theme_bw()
```

![](labfor_analysis_files/figure-gfm/PR%20in%20PR%20Age%20Group%20Visualizaing-1.png)<!-- -->

``` r
ggplot(pr_age_lf_rate_us, aes(x = YEAR, y = unemp_rate, color = age_grp)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Labor Force Participation Rate for Puerto Ricans in the US by Age Group",
    x = "Year",
    y = "Participation Rate (%)",
    color = "Age Group") +
  geom_text(data = subset(pr_age_lf_rate_us, YEAR %in% selected_years),
            aes(x = YEAR, y = unemp_rate, label = sprintf("%.1f",unemp_rate), 
                vjust = -1)) +
  geom_vline(xintercept = c(2006,2008,2015,2017,2020),linetype = "dotted",
             color = "red") +
  theme_bw()
```

![](labfor_analysis_files/figure-gfm/PR%20Age%20Group%20Unemp%20Visualizaing-1.png)<!-- -->

``` r
ggplot(age_lf_rate_us, aes(x = YEAR, y = unemp_rate, color = age_grp)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Labor Force Participation Rate for Puerto Ricans in the US by Age Group",
    x = "Year",
    y = "Participation Rate (%)",
    color = "Age Group") +
  geom_text(data = subset(age_lf_rate_us, YEAR %in% selected_years),
            aes(x = YEAR, y = unemp_rate, label = sprintf("%.1f",unemp_rate), 
                vjust = -1)) +
  geom_vline(xintercept = c(2006,2008,2015,2017,2020),linetype = "dotted",
             color = "red") +
  theme_bw()
```

![](labfor_analysis_files/figure-gfm/Age%20Group%20Unemp%20Visualizaing-1.png)<!-- -->

``` r
ggplot(age_lf_rate_pr, aes(x = YEAR, y = unemp_rate, color = age_grp)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Labor Force Participation Rate for Puerto Ricans in the US by Age Group",
    x = "Year",
    y = "Participation Rate (%)",
    color = "Age Group") +
  geom_text(data = subset(age_lf_rate_pr, YEAR %in% selected_years),
            aes(x = YEAR, y = unemp_rate, label = sprintf("%.1f",unemp_rate), 
                vjust = -1)) +
  geom_vline(xintercept = c(2006,2008,2015,2017,2020),linetype = "dotted",
             color = "red") +
  theme_bw()
```

![](labfor_analysis_files/figure-gfm/PR%20Age%20Group%20Unemp%20Visualizaing%20in%20PR-1.png)<!-- -->
