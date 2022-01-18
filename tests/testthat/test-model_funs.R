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
