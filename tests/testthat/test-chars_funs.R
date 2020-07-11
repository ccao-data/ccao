context("test chars_288_active()")

##### TEST chars_288_active() #####

chars_288_out <- chars_288_active(2016, "77")
chars_288_out_long <- chars_288_active(
  c(2017, 2013),
  c("Evanston", "New Trier")
)

test_that("functions return list", {
  expect_type(chars_288_out, "list")
  expect_length(chars_288_out, 1)
  expect_type(chars_288_out_long, "list")
  expect_length(chars_288_out_long, 2)
})

test_that("output is as expected", {
  expect_equal(chars_288_out[[1]], 2016:2020)
  expect_equal(chars_288_out_long, list(2017:2021, 2013:2018))
  expect_equal(
    chars_288_active(2010, c("25", NA)),
    list(2010:2015, NA_character_)
  )
  expect_equal(
    chars_288_active(c(2010, NA), c("25", "10")),
    list(2010:2015, NA_real_)
  )
})

test_that("bad input data stops execution", {
  expect_condition(chars_288_active(c(2013, 2010), c("25", "10", "25")))
  expect_condition(chars_288_active(1980, c("25", "10", "25")))
})


##### TEST chars_sparsify() #####

context("test chars_sparsify()")

# Load chars sample data
data("chars_sample_addchars")

chars_sparsify_out <- chars_sample_addchars %>%
  chars_sparsify(
    pin_col = QU_PIN,
    year_col = TAX_YEAR,
    town_col = as.character(QU_TOWN),
    upload_date_col = QU_UPLOAD_DATE,
    additive_source = any_of(chars_cols$add_source),
    replacement_source = any_of(chars_cols$rep_source)
  )

test_that("data is identical to known good output", {
  expect_known_hash(chars_sparsify_out, hash = "d2dcef875f")
})

##### TEST chars_update() #####

context("test chars_update()")

data("chars_sample_universe")

merged <- chars_sample_universe %>%
  dplyr::left_join(
    chars_sparsify_out,
    by = c("PIN" = "QU_PIN", "TAX_YEAR" = "YEAR")
  )

updated_chars <- merged %>%
  chars_update(
    additive_target = any_of(ccao::chars_cols$add_target),
    replacement_target = any_of(ccao::chars_cols$rep_target)
  )

# Test that output is identical to previous output
test_that("data is identical to known good output", {
  expect_known_hash(updated_chars, hash = "b4d2bd7089")
})
