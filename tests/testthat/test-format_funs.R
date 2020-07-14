context("test format_as400()")

##### TEST format_as400() #####

# Create a sample dataframe of fake data
sdf <- data.frame(
  town = c("10", "10", "10"),
  pin = c("01013130100000", "01121050080000", "01014100020000"),
  val = c(335830, 472950, 324420),
  stringsAsFactors = FALSE
)

# Create tempfile to save to
tmpf <- tempfile(fileext = ".txt")
tmpf_png <- tempfile(fileext = ".png")

test_that("bad input data stops execution", {
  expect_condition(format_as400(sdf$town[1:2], sdf$pin, sdf$val, tmpf))
  expect_condition(format_as400(sdf$town, as.numeric(sdf$pin), sdf$val, tmpf))
  expect_condition(format_as400("02", "0101410002a000", 222, tmpf))
  expect_condition(format_as400("02", "1014100020000", 222, tmpf))
  expect_condition(format_as400(
    c("02", "01"),
    c("01014100020000", "01014100020000"),
    c(222, 333),
    tmpf
  ))
  expect_condition(format_as400(sdf$town, sdf$pin, sdf$val, tmpf, "resident"))
  expect_condition(format_as400(sdf$town, sdf$pin, sdf$val, tmpf_png))
})

test_that("output is as expected", {
  expect_null(format_as400(sdf$town, sdf$pin, sdf$val, tmpf))
  expect_known_hash(read.table(tmpf), hash = "103ef62271")
  expect_null(format_as400(sdf$town, sdf$pin, sdf$val, tmpf, type = "condo"))
  expect_known_hash(read.table(tmpf), hash = "3991febe4d")
})
