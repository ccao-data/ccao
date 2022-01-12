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


##### TEST chars_fix_age() #####

context("test chars_fix_age()")

# Test mock dataset with only addchars columns
chars_fake_universe <- dplyr::tibble(
  PIN = rep("123456", 11),
  TAX_YEAR = c(2013:2018, 2020:2024),
  TOWN = rep("25", 11),
  AGE = c(rep(80, 3), rep(83, 3), rep(86, 2), rep(89, 3)),
  BEDS = c(rep(2, 6), rep(3, 5)),
  BLDG_SF = c(600, 600, rep(700, 4), rep(1100, 5)),
  GAR1_SIZE = c(rep(0, 6), rep(3, 5)),
  CLASS = rep("206", 11)
)

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
