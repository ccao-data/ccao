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

# Create simple test case using fake data
chars_sparsify_simple_data <- dplyr::tibble(
  QU_PIN = c("123456", "123456", "123456"),
  TAX_YEAR = c(2013, 2015, 2020),
  QU_CLASS = c("206", "206", "206"),
  QU_TOWN = c("25", "25", "25"),
  QU_UPLOAD_DATE = c(3, 4, 3),
  QU_BEDS = c(1, 0, 2),
  QU_SQFT_BLD = c(300, 100, 200),
  QU_GARAGE_SIZE = c(0, 3, 0)
)

# Create the correct, expected output
chars_sparsify_simple_correct <- dplyr::tibble(
  QU_PIN = rep("123456", 11),
  YEAR = c(2013:2018, 2020:2024),
  QU_CLASS = rep("206", 11),
  QU_BEDS = c(rep(1, 6), rep(2, 5)),
  QU_SQFT_BLD = c(rep(300, 2), rep(400, 4), rep(200, 5)),
  QU_GARAGE_SIZE = c(0, 0, rep(3, 4), rep(0, 5)),
  NUM_288S_ACTIVE = c(1, 1, rep(2, 4), rep(1, 5))
)

# Create results for simple case
chars_sparsify_simple_results <- chars_sparsify_simple_data %>%
  chars_sparsify(
    pin_col = QU_PIN,
    year_col = TAX_YEAR,
    class_col = QU_CLASS,
    town_col = as.character(QU_TOWN),
    upload_date_col = QU_UPLOAD_DATE,
    additive_source = any_of(chars_cols$add_source),
    replacement_source = any_of(chars_cols$rep_source)
  )

# Create complex case using sample dataset
chars_sparsify_complex <- chars_sample_addchars %>%
  chars_sparsify(
    pin_col = QU_PIN,
    year_col = TAX_YEAR,
    class_col = QU_CLASS,
    town_col = as.character(QU_TOWN),
    upload_date_col = QU_UPLOAD_DATE,
    additive_source = any_of(chars_cols$add_source),
    replacement_source = any_of(chars_cols$rep_source)
  )

# Test dataset equivalence
test_that("data is identical to known good output", {
  expect_equivalent(
    chars_sparsify_simple_results,
    chars_sparsify_simple_correct
  )
  expect_known_hash(chars_sparsify_complex, hash = "55d47a6b89")
})

##### TEST chars_update() #####

context("test chars_update()")

chars_fake_universe <- dplyr::tibble(
  PIN = rep("123456", 11),
  TAX_YEAR = c(2013:2018, 2020:2024),
  CLASS = rep("206", 11),
  TOWN = rep("25", 11),
  AGE = c(rep(80, 3), rep(83, 3), rep(86, 2), rep(89, 3)),
  BEDS = c(rep(2, 6), rep(3, 5)),
  BLDG_SF = c(600, 600, rep(700, 4), rep(1100, 5)),
  GAR1_SIZE = c(rep(0, 6), rep(3, 5))
)

chars_fake_updated <- chars_fake_universe %>%
  dplyr::left_join(
    chars_sparsify_simple_results,
    by = c("PIN" = "QU_PIN", "TAX_YEAR" = "YEAR", "CLASS" = "QU_CLASS")
  ) %>%
  chars_update(
    additive_target = any_of(ccao::chars_cols$add_target),
    replacement_target = any_of(ccao::chars_cols$rep_target)
  )

# Join to fake universe data
updated_chars <- chars_sample_universe %>%
  dplyr::left_join(
    chars_sparsify_complex %>% dplyr::mutate(QU_CLASS = as.character(QU_CLASS)),
    by = c("PIN" = "QU_PIN", "TAX_YEAR" = "YEAR", "CLASS" = "QU_CLASS")
  ) %>%
  chars_update(
    additive_target = any_of(ccao::chars_cols$add_target),
    replacement_target = any_of(ccao::chars_cols$rep_target)
  )

# Test with groups applied
updated_chars_groups <- chars_sample_universe %>%
  dplyr::left_join(
    chars_sparsify_complex %>% dplyr::mutate(QU_CLASS = as.character(QU_CLASS)),
    by = c("PIN" = "QU_PIN", "TAX_YEAR" = "YEAR", "CLASS" = "QU_CLASS")
  ) %>%
  dplyr::group_by(TAX_YEAR) %>%
  chars_update(
    additive_target = any_of(ccao::chars_cols$add_target),
    replacement_target = any_of(ccao::chars_cols$rep_target)
  )

# Test that output is identical to previous output
test_that("data is identical to known good output", {
  expect_known_hash(chars_fake_updated, hash = "64a26a3b64")
  expect_known_hash(updated_chars, hash = "10246b9174")
  expect_known_hash(updated_chars_groups, hash = "0ee1cf5b06")
})


##### TEST chars_fix_age() #####

context("test chars_fix_age()")

age_test <- dplyr::tibble(
  age = c(120, 120, 123),
  year = c(2014, 2015, 2016),
  town = rep("25", 3)
)

test_that("output is as expected", {
  expect_equal(
    chars_fix_age(age_test$age, age_test$year, age_test$town),
    c(121, 122, 123)
  )
  expect_equal(
    chars_fix_age(
      chars_fake_universe$AGE,
      chars_fake_universe$TAX_YEAR,
      chars_fake_universe$TOWN
    ),
    c(80:85, 87:91)
  )
})

test_that("bad input data throws errors", {
  expect_condition(chars_fix_age(80, 1990, "cat"))
  expect_condition(chars_fix_age(80:90, 2010:2015, "25"))
  expect_equal(chars_fix_age(80, 2010, "cat"), NA_real_)
})

test_that("missing data inputs returns NA outputs", {
  expect_equal(chars_fix_age(NA_real_, 2010, "25"), NA_real_)
  expect_equal(chars_fix_age(120, NA_real_, "25"), NA_real_)
  expect_equal(chars_fix_age(120, 2010, NA_character_), NA_character_)
})
