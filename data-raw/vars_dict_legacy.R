vars_dict_legacy <- readr::read_csv(
  file = "data-raw/vars_dict_legacy.csv",
  col_types = readr::cols(var_code = readr::col_character())
)

usethis::use_data(vars_dict_legacy, overwrite = TRUE)
