class_dict <- readr::read_csv(
  file = "data-raw/class_dict.csv",
  col_types = readr::cols(class_code = readr::col_character())
)

usethis::use_data(class_dict, overwrite = TRUE)
