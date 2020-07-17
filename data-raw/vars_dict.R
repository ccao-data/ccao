
vars_dict <- readr::read_csv(
  file = "data-raw/vars_dict.csv",
  col_types = readr::cols(var_code = readr::col_character())
)

usethis::use_data(vars_dict, overwrite = TRUE)
