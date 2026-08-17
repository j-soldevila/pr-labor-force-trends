# Update data base

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, DBI, RPostgres,viridis)


# 1. Fixed connection variable (was 'on', changed to 'con')
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'pr_labor_proj',
  host = "localhost",
  port = 5432,
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# 2. Read and upload the data
mini_data <- read_csv("usa_00127.csv")

dbWriteTable(
  con,
  name = "temp_data",
  value = mini_data,
  overwrite = TRUE,
  row.names = FALSE
)

# 3. Create an index on the temporary table (Crucial for speed)
dbExecute(con, 'CREATE INDEX temp_match_idx ON temp_data ("YEAR", "SAMPLE", "SERIAL", "PERNUM");')

# 4. Combine ALTER TABLE statements
# dbExecute(con, '
#   ALTER TABLE data_acs 
#   ADD COLUMN "HHINCOME" INTEGER,
#   ADD COLUMN "EDUC" INTEGER,
#   ADD COLUMN "SPEAKENG" INTEGER,
#   ADD COLUMN "UHRSWORK" INTEGER;
# ')

# dbExecute(con,'
#           ALTER TABLE data_acs
#           ADD COLUMN "DEGFIELD" INTEGER,
#           ADD COLUMN "SCHOOL" INTEGER;
#           ')

# dbExecute(con,'
#           ALTER TABLE data_acs_decennial
#           ADD COLUMN "INCWAGE" INTEGER,
#           ADD COLUMN "IND1990" INTEGER;
#           ')

dbExecute(con,'
          ALTER TABLE data_acs_decennial
          ADD COLUMN "CPI99" INTEGER;
          ')

# 5. Combine all updates into a single query
# update_query <- '
#   UPDATE data_acs AS main
#   SET 
#     "HHINCOME" = temp."HHINCOME",
#     "EDUC" = temp."EDUC",
#     "SPEAKENG" = temp."SPEAKENG",
#     "UHRSWORK" = temp."UHRSWORK"
#   FROM temp_data AS temp
#   WHERE main."YEAR" = temp."YEAR"
#     AND main."SAMPLE" = temp."SAMPLE"
#     AND main."SERIAL" = temp."SERIAL"
#     AND main."PERNUM" = temp."PERNUM";
# '
# dbExecute(con, update_query)

# update_query <- '
#   UPDATE data_acs AS main
#   SET
#   "DEGFIELD" = temp."DEGFIELD",
#   "SCHOOL" = temp."SCHOOL"
#   FROM temp_data AS temp
#   WHERE main."YEAR" = temp."YEAR"
#      AND main."SAMPLE" = temp."SAMPLE"
#      AND main."SERIAL" = temp."SERIAL"
#      AND main."PERNUM" = temp."PERNUM";
# '
# dbExecute(con, update_query)

# update_query <- '
#   UPDATE data_acs_decennial AS main
#   SET
#   "INCWAGE" = temp."INCWAGE",
#   "IND1990" = temp."IND1990"
#   FROM temp_data AS temp
#   WHERE main."YEAR" = temp."YEAR"
#      AND main."SAMPLE" = temp."SAMPLE"
#      AND main."SERIAL" = temp."SERIAL"
#      AND main."PERNUM" = temp."PERNUM";
# '
# dbExecute(con, update_query)

update_query <- '
  UPDATE data_acs_decennial AS main
  SET
  "CPI99" = temp."CPI99"
  FROM temp_data AS temp
  WHERE main."YEAR" = temp."YEAR"
     AND main."SAMPLE" = temp."SAMPLE"
     AND main."SERIAL" = temp."SERIAL"
     AND main."PERNUM" = temp."PERNUM";
'
dbExecute(con, update_query)

# 6. Clean up with the correct table name and disconnect
dbExecute(con, "DROP TABLE temp_data;")
dbDisconnect(con)
