coe_dict <- readr::read_csv(
  file = "data-raw/coe_dict.csv",
  col_types = readr::cols(
    coe_code = readr::col_integer(),
    coe_reason = readr::col_character()
  )
)

usethis::use_data(coe_dict, overwrite = TRUE)
