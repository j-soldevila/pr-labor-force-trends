01_data_wrangling
================
Jorge R. Soldevila Irizarry
2026-07-15

# This Project is the beginning of larger work that explores the potential impact of AI on the Puerto Rican work force. Here we perform initial assessment to gather insights into the behavior of Puerto Ricans in the workforce over the past 20 years.

## Read in the libraries

``` r
# Install pacman if not already installed on the system. This will allow us to install desired libraries. 
if (!require("pacman")) install.packages("pacman")

# Load libraries using p_load(), which installs missing packages and loads
#libraries.
p_load(tidyverse,      # For data manipulation.
       here,           # For dynamic file path *probably will not need
       DBI,            # For interacting with database
       odbc,
       RPostgres,      # For connecting to Postgresql database.
       kableExtra,     # For creating tables
       scales,           # For formating y-axis in graphs
       ipumsr
       ) 
```

## Because of the size of our data sets we are hosting the data sets in a Postgrsql data base. Here we show steps for connecting to the data base. This method allows us to establish a connection and use native R language functions and syntax to interact with our data.

``` r
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'pr_labor_proj',
  host = "localhost",
  port = 5432,
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)
```

## Inside the database we have two tables, one contains data from the PRCS for the years 2006 to 2024, and another contains data from the ACS for the years 2001 to 2024.

## Let’s call our data by referring to the tables in the database.

``` r
#Get PRCS data
prcs_06_24 <- tbl(con, "data_prcs")

#Get ACS data
acs_01_24 <- tbl(con, "data_acs")
```
