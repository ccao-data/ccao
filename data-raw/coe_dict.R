coe_dict <- readr::read_csv(
  file = "data-raw/coe_dict.csv",
  col_types = cols(coe_code = col_integer(), coe_reason = col_character())
)

usethis::use_data(coe_dict, overwrite = TRUE)
