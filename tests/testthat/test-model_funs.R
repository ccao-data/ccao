context("test rm_intermediate()")

##### TEST rm_intermediate() #####

x <- "50"
y <- 10

# Test for expected outputs
test_that("output is as expected", {
  expect_silent(rm_intermediate(x))
  expect_silent(rm_intermediate("lgbm"))
  expect_silent(rm_intermediate("x"))
  expect_silent(rm_intermediate("x", keep = "y"))
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(rm_intermediate(50))
  expect_condition(rm_intermediate(c("800", "202")))
  expect_condition(rm_intermediate(list("800", "202")))
})


context("test model_get_env()")

##### TEST model_get_env() #####

Sys.setenv("R_TEST_VAR" = "99")

# Test for expected outputs
test_that("output is as expected", {
  expect_equal(model_get_env("R_TEST_VAR", "56"), "99")
  expect_equal(model_get_env("", "56"), "56")
  expect_equal(model_get_env("", 56), 56)
  expect_equal(model_get_env("", NA), NA)
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(model_get_env(9, "56"))
  expect_condition(model_get_env(c("TEST_VAR", "R_TEST_VAR"), "56"))
  expect_condition(model_get_env(""))
})


context("test model_axe_tune_data()")

##### TEST model_axe_tune_data() #####

axe_test_data <- dplyr::tibble(
  PIN = rep("12345", 4),
  EXT_WALL = c("1", "2", "0", NA),
  splits = c("1", "3", "4", "5")
)

# Test for expected outputs
test_that("output is as expected", {
  expect_equivalent(model_axe_tune_data(axe_test_data), axe_test_data[, 1:2])
  expect_equivalent(
    model_axe_tune_data(chars_sample_universe),
    chars_sample_universe
  )
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(model_axe_tune_data("cat"))
  expect_condition(model_axe_tune_data(8))
})


context("test model_axe_recipe()")

##### TEST model_axe_recipe() #####

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(model_axe_recipe("cat"))
  expect_condition(model_axe_recipe(8))
})


context("test model_lgbm_cap_num_leaves()")

##### TEST model_lgbm_cap_num_leaves() #####

params_test_data <- data.frame(
  num_leaves = c(60, 600),
  tree_depth = c(7, 7),
  af = c("a", "b")
)

# Test for expected outputs
test_that("output is as expected", {
  expect_equivalent(
    model_lgbm_cap_num_leaves(params_test_data),
    data.frame(num_leaves = c(60, 127), tree_depth = c(7, 7), af = c("a", "b"))
  )
  expect_silent(model_lgbm_cap_num_leaves(data.frame()))
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(model_lgbm_cap_num_leaves("cat"))
  expect_condition(model_lgbm_cap_num_leaves(8))
})
