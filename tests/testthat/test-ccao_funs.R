context("load data")

# Load the ratios sample dataset for testing
library(assessr)
data("ratios_sample")

# Extract the components of the dataframe as vectors
ratio <- ratios_sample$ratio
sale_price <- ratios_sample$sale_price
assessed <- ratios_sample$assessed


##### TEST ccao_cod() #####

context("test ccao_cod()")

# Calculate COD
cod_out <- ccao_cod(ratio)
cod_out_w_outliers <- ccao_cod(c(ratio, rep(1.35, 100), rep(0.2, 50)))

test_that("functions return named list", {
  expect_type(cod_out, "list")
  expect_named(cod_out)
})

test_that("output within in expected range", {
  expect_gt(cod_out$COD, 12)
  expect_lt(cod_out$COD, 13)
  expect_gt(cod_out_w_outliers$COD, 15)
  expect_lt(cod_out_w_outliers$COD, 16)
  expect_equal(ccao_cod(c(ratio, rep(NA, 20)), na.rm = TRUE)$COD_N, 881)
})

test_that("overlapping CI returns true for CI_MET", {
  expect_true(cod_out_w_outliers$COD_CI_MET)
})

test_that("bad input data stops execution", {
  expect_condition(ccao_cod(data.frame(ratio)))
  expect_condition(ccao_cod(c(ratio, NaN)))
  expect_condition(ccao_cod(c(ratio, "2")))
})

test_that("incomplete data stops execution unless suppressed", {
  expect_condition(ccao_cod(runif(29)))
  expect_silent(ccao_cod(runif(29), suppress = TRUE))
  expect_equal(
    unname(ccao_cod(runif(29), suppress = TRUE)),
    list(NA, NA, NA, NA, 25)
  )
  expect_equal(
    unname(ccao_cod(c(ratio[1:29], rep(NA, 10)), suppress = TRUE)),
    list(NA, NA, NA, NA, 25)
  )
  expect_equal(
    unname(ccao_cod(rep(NA_real_, 40), suppress = TRUE)),
    list(NA, NA, NA, NA, 0)
  )
})


##### TEST ccao_prd() #####

context("test ccao_prd()")

# Calculate PRD from sample
prd_out <- ccao_prd(assessed, sale_price)
prd_out_w_outliers <- ccao_prd(
  c(assessed, rep(1e4, 80)),
  c(sale_price, rep(1.5e5, 80))
)

test_that("functions return named list", {
  expect_type(prd_out, "list")
  expect_named(prd_out)
})

test_that("output within expected range", {
  expect_gt(prd_out$PRD, 1.01)
  expect_lt(prd_out$PRD, 1.03)
  expect_gt(prd_out_w_outliers$PRD, 0.97)
  expect_lt(prd_out_w_outliers$PRD, 0.98)
  expect_equal(ccao_prd(
    c(assessed, rep(NA, 80)),
    c(sale_price, rep(NA, 80)),
    na.rm = TRUE
  )$PRD_N, 881)
})

test_that("overlapping CI returns true for CI_MET", {
  expect_true(prd_out_w_outliers$PRD_CI_MET)
})

test_that("bad input data stops execution", {
  expect_condition(ccao_prd(data.frame(assessed), sale_price))
  expect_condition(ccao_prd(c(assessed, NaN), c(sale_price, 10e5)))
  expect_condition(ccao_prd(c(assessed, "2"), c(sale_price, 10e5)))
  expect_condition(ccao_prd(assessed))
  expect_condition(ccao_prd(assessed, c(sale_price, NA)))
  expect_condition(ccao_prd(assessed, c(sale_price, 10000)))
})

test_that("incomplete data stops execution unless suppressed", {
  expect_condition(ccao_prd(runif(29), runif(29)))
  expect_silent(ccao_prd(runif(29), runif(29), suppress = TRUE))
  expect_equal(
    unname(ccao_prd(runif(29), runif(29), suppress = TRUE)),
    list(NA, NA, NA, NA, 25)
  )
  expect_equal(
    unname(ccao_prd(
      c(assessed[1:29], rep(NA, 10)),
      c(sale_price[1:29], rep(NA, 10)),
      suppress = TRUE
    )),
    list(NA, NA, NA, NA, 25)
  )
  expect_equal(
    unname(ccao_prd(
      rep(NA_real_, 40),
      rep(NA_real_, 40),
      suppress = TRUE
    )),
    list(NA, NA, NA, NA, 0)
  )
})


##### TEST ccao_prb() #####

context("test ccao_prb()")

# Create a vector of sales the same length as ratio
prb_out <- ccao_prb(assessed, sale_price)
prb_out_w_outliers <- ccao_prb(
  c(assessed, rep(4.5e4, 60)),
  c(sale_price, rep(1e5, 60))
)

test_that("functions return named list", {
  expect_type(prb_out, "list")
  expect_named(prb_out)
})

test_that("output within expected range", {
  expect_gt(prb_out$PRB, -0.01)
  expect_lt(prb_out$PRB, 0.01)
  expect_gt(prb_out_w_outliers$PRB, 0.05)
  expect_lt(prb_out_w_outliers$PRB, 0.06)
  expect_equal(ccao_prb(
    c(assessed, rep(NA, 80)),
    c(sale_price, rep(NA, 80)),
    na.rm = TRUE
  )$PRB_N, 881)
})

test_that("overlapping CI returns true for CI_MET", {
  expect_true(prb_out_w_outliers$PRB_CI_MET)
})

test_that("bad input data stops execution", {
  expect_condition(ccao_prd(data.frame(assessed), sale_price))
  expect_condition(ccao_prb(c(assessed, NaN), c(sale_price, 10e5)))
  expect_condition(ccao_prb(assessed))
  expect_condition(ccao_prb(assessed, c(sale_price, NA)))
  expect_condition(ccao_prb(assessed, c(sale_price, 10000)))
})

test_that("incomplete data stops execution unless suppressed", {
  expect_condition(ccao_prb(runif(29), runif(29)))
  expect_silent(ccao_prb(runif(29), runif(29), suppress = TRUE))
  expect_equal(
    unname(ccao_prb(runif(29), runif(29), suppress = TRUE)),
    list(NA, NA, NA, NA, 25)
  )
  expect_equal(
    unname(ccao_prb(
      c(assessed[1:29], rep(NA, 10)),
      c(sale_price[1:29], rep(NA, 10)),
      suppress = TRUE
    )),
    list(NA, NA, NA, NA, 25)
  )
  expect_equal(
    unname(ccao_prb(
      rep(NA_real_, 40),
      rep(NA_real_, 40),
      suppress = TRUE
    )),
    list(NA, NA, NA, NA, 0)
  )
})
