coe_codes <- readr::read_csv(
  file = "data-raw/coe_codes.csv",
  col_types = cols(coe_code = col_integer(), coe_reason = col_character())
)

usethis::use_data(coe_codes, overwrite = TRUE)
