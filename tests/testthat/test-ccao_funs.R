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

test_that("functions return named list", {
  expect_type(cod_out, "list")
  expect_named(cod_out)
})

test_that("output within in expected range", {
  expect_gt(cod_out$COD, 11)
  expect_lt(cod_out$COD, 13)
})

test_that("bad input data stops execution", {
  expect_condition(ccao_cod(data.frame(ratio)))
  expect_condition(ccao_cod(c(ratio, NA)))
  expect_condition(ccao_cod(c(ratio, NaN)))
  expect_condition(ccao_cod(c(ratio, "2")))
})

test_that("incomplete data stops execution unless suppressed", {
  expect_condition(ccao_cod(runif(29)))
  expect_silent(ccao_cod(runif(29), suppress = TRUE))
  expect_equal(
    unname(ccao_cod(runif(29), suppress = TRUE)),
    list(NA, NA, NA, 25)
  )
})


##### TEST ccao_prd() #####

context("test ccao_prd()")

# Calculate PRD from sample
prd_out <- ccao_prd(assessed, sale_price)

test_that("functions return named list", {
  expect_type(prd_out, "list")
  expect_named(prd_out)
})

test_that("output within expected range", {
  expect_gt(prd_out$PRD, 0.98)
  expect_lt(prd_out$PRD, 1.03)
})

test_that("bad input data stops execution", {
  expect_condition(ccao_prd(data.frame(assessed), sale_price))
  expect_condition(ccao_prd(c(assessed, NA), c(sale_price, 10e5)))
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
    list(NA, NA, NA, 25)
  )
})


##### TEST ccao_prb() #####

context("test ccao_prb()")

# Create a vector of sales the same length as ratio
prb_out <- ccao_prb(assessed, sale_price)

test_that("functions return named list", {
  expect_type(prb_out, "list")
  expect_named(prb_out)
})

test_that("output within expected range", {
  expect_gt(prb_out$PRB, -0.03)
  expect_lt(prb_out$PRB, 0.03)
})

test_that("bad input data stops execution", {
  expect_condition(ccao_prd(data.frame(assessed), sale_price))
  expect_condition(ccao_prb(c(assessed, NA), c(sale_price, 10e5)))
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
    list(NA, NA, NA, 25)
  )
})
