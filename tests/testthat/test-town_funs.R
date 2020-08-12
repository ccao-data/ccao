context("test convert_town()")

##### TEST town_convert() #####

towns <- c("Evanston", "Lyons", "25", "10")

test_that("output is correct", {
  expect_equal(
    town_convert(towns),
    c("17", "21", "Northfield", "Barrington")
  )
  expect_equal(
    town_convert(town_dict$township_name),
    as.character(town_dict$township_code)
  )
  expect_equal(
    town_convert(as.character(town_dict$township_code)),
    town_dict$township_name
  )
})

test_that("Missing values are handled correctly", {
  expect_equal(
    town_convert(c(towns, NA, NULL, NaN, "02")),
    c("17", "21", "Northfield", "Barrington", NA, NA, NA)
  )
})

test_that("incorrect inputs throw errors", {
  expect_condition(town_convert(data.frame(towns)))
  expect_condition(town_convert(list(towns)))
  expect_condition(town_convert(c(02, 11, 10)))
})


##### TEST town_get_triad() #####

context("test town_get_triad()")

towns <- c("Evanston", "Lyons", "25", "10", "77", "West Chicago")

test_that("output is correct", {
  expect_equal(
    town_get_triad(towns),
    c("2", "3", "2", "2", "1", "1")
  )
  expect_equal(
    town_get_triad(towns, name = TRUE),
    c("North", "South", "North", "North", "City", "City")
  )
})

test_that("Missing values are handled correctly", {
  expect_equal(
    town_get_triad(c(towns, NA, NULL, NaN, "02")),
    c("2", "3", "2", "2", "1", "1", NA, NA, NA)
  )
})

test_that("incorrect inputs throw errors", {
  expect_condition(town_get_triad(data.frame(towns)))
  expect_condition(town_get_triad(list(towns)))
  expect_condition(town_get_triad(c(02, 11, 10)))
})


##### TEST town_get_assmnt_year() #####

context("test town_get_assmnt_year()")

towns <- c("Evanston", "Lyons", "25", "10", "77", "West Chicago")

test_that("output is correct", {
  expect_equal(
    town_get_assmnt_year(towns, 2020, round_type = "floor"),
    c(2019, 2020, 2019, 2019, 2018, 2018)
  )
  expect_equal(
    town_get_assmnt_year(towns, 1995),
    c(1995, 1996, 1995, 1995, 1994, 1994)
  )
  expect_equal(
    town_get_assmnt_year(towns, 1995, round_type = "ceiling"),
    c(1995, 1996, 1995, 1995, 1997, 1997)
  )
  expect_equal(
    town_get_assmnt_year(towns[1:3], c(2008, 2003, 2020), round_type = "floor"),
    c(2007, 2002, 2019)
  )
  expect_equal(
    town_get_assmnt_year(towns, 2001:2006, round_type = "ceiling"),
    c(2001, 2002, 2004, 2004, 2006, 2006)
  )
})

test_that("Missing values are handled correctly", {
  expect_equal(
    town_get_assmnt_year(c(towns, NA, NULL, NaN, "02"), round_type = "floor"),
    c(2019, 2020, 2019, 2019, 2018, 2018, NA, NA, NA)
  )
  expect_equal(town_get_assmnt_year(NA_character_, 2010:2012), rep(NA_real_, 3))
  expect_equal(town_get_assmnt_year(c("25", NA), 2014:2015), c(2013, NA))
})

test_that("incorrect inputs throw errors", {
  expect_error(town_get_assmnt_year(data.frame(towns)))
  expect_error(town_get_assmnt_year(list(towns)))
  expect_error(town_get_assmnt_year(c(02, 11, 10)))
  expect_error(town_get_assmnt_year(c("10", "77"), 2010:2014))
})
