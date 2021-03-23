equalization_factors <- readr::read_csv(
  file = "data-raw/equalization_factors.csv",
  col_types = cols(year = col_integer(), equalization_factor = col_double())
)

usethis::use_data(equalization_factors, overwrite = TRUE)
