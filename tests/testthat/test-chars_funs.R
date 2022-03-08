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
    list(2010:2015, NA_real_)
  )
  expect_equal(
    chars_288_active(c(2010, NA), c("25", "10")),
    list(2010:2015, NA_real_)
  )
  expect_equal(
    chars_288_active(NA_real_, c("25", "10")),
    list(NA_real_, NA_real_)
  )
  expect_equal(
    chars_fix_age(80, 2015, c("Evanston", "Niles")),
    c(82, 82)
  )
})

test_that("bad input data stops execution", {
  expect_error(chars_288_active(c(2013, 2010), c("25", "10", "25")))
  expect_error(chars_288_active(1980, c("25", "10", "25")))
  expect_error(chars_288_active(2010:2013, c("25", "10", "10")))
  expect_error(chars_fix_age(81:82, 2015, c("Evanston", "Niles", "10")))
  expect_error(chars_fix_age(81:83, 2015, c("Evanston", "Niles")))
})


##### TEST chars_sparsify() #####

context("test chars_sparsify()")

# Create simple test case using fake data
chars_sparsify_simple_data <- dplyr::tibble(
  pin = c("123456", "123456", "123456"),
  year = c(2013, 2015, 2020),
  qu_class = c("206", "206", "206"),
  qu_town = c("25", "25", "25"),
  qu_upload_date = c(3, 4, 3),
  qu_beds = c(1, 0, 2),
  qu_sqft_bld = c(300, 100, 200),
  qu_garage_size = c(0, 3, 0)
)

# Create the correct, expected output
chars_sparsify_simple_correct <- dplyr::tibble(
  pin = rep("123456", 11),
  year = c(2013:2018, 2020:2024),
  qu_beds = c(rep(1, 6), rep(2, 5)),
  qu_sqft_bld = c(rep(300, 2), rep(400, 4), rep(200, 5)),
  qu_garage_size = c(0, 0, rep(3, 4), rep(0, 5)),
  qu_class = rep("206", 11),
  hie_num_active = c(1, 1, rep(2, 4), rep(1, 5))
)

# Create results for simple case
chars_sparsify_simple_test <- chars_sparsify_simple_data %>%
  chars_sparsify(
    pin_col = pin,
    year_col = year,
    town_col = as.character(qu_town),
    upload_date_col = qu_upload_date,
    additive_source = any_of(chars_cols$add$source),
    replacement_source = any_of(chars_cols$replace$source)
  )

# Create complex case using sample dataset
chars_sample_hie_sparse <- chars_sample_hie %>%
  chars_sparsify(
    pin_col = pin,
    year_col = year,
    town_col = qu_town,
    upload_date_col = qu_upload_date,
    additive_source = any_of(chars_cols$add$source),
    replacement_source = any_of(chars_cols$replace$source)
  )

# Test dataset equivalence
test_that("data is identical to known good output", {
  expect_equivalent(
    chars_sparsify_simple_test,
    chars_sparsify_simple_correct
  )
  expect_known_hash(chars_sample_hie_sparse, hash = "0276a27ad5")
})


##### TEST chars_update() #####

context("test chars_update()")

# Test mock dataset with only addchars columns
chars_fake_universe <- dplyr::tibble(
  pin = rep("123456", 11),
  year = c(2013:2018, 2020:2024),
  town = rep("25", 11),
  char_age = c(rep(80, 3), rep(83, 3), rep(86, 2), rep(89, 3)),
  char_beds = c(rep(2, 6), rep(3, 5)),
  char_bldg_sf = c(600, 600, rep(700, 4), rep(1100, 5)),
  char_gar1_size = c(rep(0, 6), rep(3, 5)),
  meta_class = rep("206", 11)
)

chars_fake_updated <- chars_fake_universe %>%
  dplyr::left_join(
    chars_sparsify_simple_test,
    by = c("pin", "year")
  ) %>%
  chars_update(
    additive_target = any_of(ccao::chars_cols$add$target),
    replacement_target = any_of(ccao::chars_cols$replace$target)
  )

# Test that output is identical to previous output
test_that("data is identical to known good output", {
  expect_known_hash(chars_fake_updated, hash = "a4b0543b65")
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
      chars_fake_universe$char_age,
      chars_fake_universe$year,
      chars_fake_universe$town
    ),
    c(80:85, 87:91)
  )
})

test_that("bad input data throws errors", {
  expect_error(chars_fix_age(80, 1990, "cat"))
  expect_error(chars_fix_age(80:90, 2010:2015, "25"))
  expect_error(chars_fix_age(80:82, 2010:2011, "25"))
  expect_equal(chars_fix_age(80, 2010, "cat"), NA_real_)
})

test_that("missing data inputs returns NA outputs", {
  expect_equal(chars_fix_age(NA_real_, 2010, "25"), NA_real_)
  expect_equal(chars_fix_age(120, NA_real_, "25"), NA_real_)
  expect_equal(chars_fix_age(120, 2010, NA_character_), NA_real_)
})
