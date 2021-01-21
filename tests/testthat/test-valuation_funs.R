context("test val_med_pct_adj()")

##### TEST val_med_pct_adj() #####

sales <- c(20000, 15000, 30000, NA, 55000, 40000, NA)
estimates <- c(30000, 20000, 31000, 10000, 100000, 30000, 20000)

# Test for expected outputs
test_that("output is as expected", {
  expect_equal(
    val_med_pct_adj(sales, estimates, min_n = 3, max_abs_adj = 0.4),
    -0.25
  )
  expect_equal(
    val_med_pct_adj(sales, estimates, 3, 0.4, na.rm = FALSE),
    NA
  )
  expect_equal(
    val_med_pct_adj(1000:2000, 1100:2100, 3, 0.4, na.rm = FALSE),
    -0.0625
  )
  expect_equal(val_med_pct_adj(1:10, 11:20, 20, 0.4), NA_real_)
  expect_equal(val_med_pct_adj(101:110, 11:20, 3, 0.4), 0.4)
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(val_med_pct_adj(1:10, 1:20, 3, 0.4))
  expect_condition(val_med_pct_adj(1:10, 11:20, "3", 0.4))
  expect_condition(val_med_pct_adj(1:10, 11:20, 3, 0.4, na.rm = "true"))
})


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


context("test val_create_ntiles()")

##### TEST val_create_ntiles() #####

# Test for expected outputs
test_that("output is as expected", {
  expect_equal(
    val_create_ntiles(0:100, probs = c(0.25, 0.75)),
    list(c(-Inf, 25, 75, Inf))
  )
  expect_equal(
    val_create_ntiles(c(0:100, NA), probs = c(0.2, 0.4, 0.6, 0.8)),
    list(c(-Inf, 20, 40, 60, 80, Inf))
  )
  expect_equal(
    val_create_ntiles(c(NA_real_, NA, NA), probs = c(0.2, 0.4, 0.6, 0.8)),
    list(NA_real_)
  )
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(val_create_ntiles(c(sales, "10"), probs = c(0.25, 0.75)))
  expect_condition(val_create_ntiles(sales, probs = c(2, 0.75)))
  expect_condition(val_create_ntiles(sales), probs = c("0.25", 0.75))
})


context("test val_assign_ntile()")

##### TEST val_assign_ntile() #####

ntiles <- val_create_ntiles(0:100, probs = c(0.25, 0.75))
ntiles_long <- c(ntiles, val_create_ntiles(0:100, probs = c(0.2, 0.6, 0.8)))

# Test for expected outputs
test_that("output is as expected", {
  expect_equal(val_assign_ntile(40, ntiles), "(25,75]")
  expect_equal(val_assign_ntile(1000, ntiles), "(75, Inf]")
  expect_equal(val_assign_ntile(-10, ntiles), "(-Inf,25]")
  expect_equal(
    val_assign_ntile(c(-10, 10, 1000), ntiles),
    c("(-Inf,25]", "(-Inf,25]", "(75, Inf]")
  )
  expect_equal(val_assign_ntile(10, ntiles_long), c("(-Inf,25]"))
  expect_equal(
    val_assign_ntile(c(10, 10), ntiles_long),
    c("(-Inf,25]", "(-Inf,20]")
  )
  expect_equal(val_assign_ntile(NA_real_, ntiles), NA_character_)
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(val_assign_ntile(c(40, "10"), ntiles))
  expect_condition(val_assign_ntile(40, ntiles[[1]]))
})
