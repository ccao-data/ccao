context("test val_limit_ratios()")

##### TEST val_limit_ratios() #####

sales <- c(20000, 15000, 30000, NA, 55000, 40000, NA, 100000)
estimates <- c(30000, 50000, 31000, 10000, 200000, 30000, 20000, 10000)

# Test for expected outputs
test_that("output is as expected", {
  expect_equal(
    val_limit_ratios(sales, estimates, 0.7, 2.0),
    c(30000, 30000, 31000, 10000, 110000, 30000, 20000, 70000)
  )
  expect_equal(
    val_limit_ratios(c(sales, 10000), c(estimates, NA), 0.7, 2.0),
    c(30000, 30000, 31000, 10000, 110000, 30000, 20000, 70000, NA)
  )
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(val_limit_ratios(c(sales, 10), estimates, 0.7, 2.0))
  expect_condition(val_limit_ratios(sales, estimates, -0.7, 2.0))
  expect_condition(val_limit_ratios(c(sales, "b"), c(estimates, "a"), 0.7, 2.0))
  expect_condition(val_limit_ratios(list(sales), estimates, 0.7, 2.0))
})


context("test val_round_fmv()")

##### TEST val_round_fmv() #####

# Extract the components of the ratios data frame as vectors
estimates <- c(31249, 809451, 1039404, 58001, 2501, 1021, 0)

# Test for expected outputs
test_that("output is as expected", {
  expect_equal(
    val_round_fmv(estimates, type = "floor"),
    c(30000, 800000, 1030000, 55000, 0, 0, 0)
  )
  expect_equal(
    val_round_fmv(estimates, type = "normal"),
    c(30000, 810000, 1040000, 60000, 5000, 0, 0)
  )
  expect_equal(
    val_round_fmv(estimates, type = "ceiling"),
    c(35000, 810000, 1040000, 60000, 5000, 5000, 0)
  )
})


# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(val_round_fmv(estimates, breaks = TRUE))
  expect_condition(val_round_fmv(estimates, breaks = NULL))
  expect_condition(val_round_fmv(estimates, breaks = 0))
  expect_condition(val_round_fmv(estimates, type = "round"))
  expect_condition(val_round_fmv(estimates, type = 0))
  expect_condition(val_round_fmv(estimates, round_to = c(100, 1000, 10000)))
  expect_condition(val_round_fmv(estimates, round_to = c("x", "y")))
})

# Weird values handled correctly
test_that("NA, Inf, NaN handled correctly", {
  expect_equal(
    val_round_fmv(c(estimates, NA, Inf, NaN), type = "floor"),
    c(30000, 800000, 1030000, 55000, 0, 0, 0, NA, NA, NaN)
  )
  expect_equal(
    val_round_fmv(c(estimates, NA, Inf, NaN), type = "normal"),
    c(30000, 810000, 1040000, 60000, 5000, 0, 0, NA, NA, NaN)
  )
  expect_equal(
    val_round_fmv(c(estimates, NA, Inf, NaN), type = "ceiling"),
    c(35000, 810000, 1040000, 60000, 5000, 5000, 0, NA, NA, NaN)
  )
})
