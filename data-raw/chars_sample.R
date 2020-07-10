# This script is used to create a sample of addchars + characteristics data for
# testing chars_ functions
library(dplyr)
library(DBI)
library(glue)

current_year <- year(Sys.time())

# Connect to main CCAO DB
CCAODATA <- dbConnect(
  odbc(),
  .connection_string = Sys.getenv("DB_CONFIG_CCAODATA"),
  dbname = "CCAODATA"
)

# Gather a full universe of PINs
universe <- dbGetQuery(
  CCAODATA, 
  glue_sql(
    "
    SELECT * 
    FROM VW_RES_UNIVERSE
    WHERE TAX_YEAR BETWEEN {min_year} AND {max_year}
    ",
    .con = CCAODATA,
    min_year = current_year - 7,
    max_year = current_year
  )
) 

# If current tax year is same also a  reassessment year for target_township,
# years prior should be 4. If current tax year is not a reassessment year for
# target_township, make sure to keep in consideration for which years 288s 
# are valid when defining years_prior, i.e. a 288 can last up to 6 years 
# depending on when it was filed
addchars <- dbGetQuery(
  CCAODATA,
  glue_sql(
    "
    SELECT *
    FROM ADDCHARS
    WHERE QU_HOME_IMPROVEMENT = 1 
    AND TAX_YEAR BETWEEN {min_year} AND {max_year}
    ",
    .con = CCAODATA,
    min_year = current_year - 8,
    max_year = current_year
  )
)

# Generate a useful subsample of addchars that has both additive and replacement
# characteristics
sample_pins <- addchars %>%
  group_by(QU_PIN) %>%
  filter(n() > 1) %>%
  group_by(QU_PIN, TAX_YEAR) %>%
  filter(n() == 1) %>%
  filter(TAX_YEAR <= 2014) %>%
  filter(QU_SQFT_BLD != 0 & (QU_ROOF != 0 | QU_BASEMENT_FINISH != 0)) %>%
  pull(QU_PIN)
  
chars_sample_addchars <- addchars %>%
  filter(QU_PIN %in% sample_pins)

# Gather only PINs from universe that are in the addchars sample
chars_sample_universe <- universe %>%
  filter(PIN %in% sample_pins)

# Save both datasets to data/
usethis::use_data(
  chars_sample_addchars,
  chars_sample_universe,
  overwrite = TRUE,
  compress = "xz"
)
