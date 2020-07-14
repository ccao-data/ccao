context("test check_class()")

##### TEST check_class() #####

# Test for expected outputs
test_that("output is as expected", {
  expect_equal(check_class(50, 800, "202"), TRUE)
  expect_equal(check_class(50, 800, c("202", "299")), c(TRUE, TRUE))
  expect_equal(check_class(50, 800, c("202", "203")), c(TRUE, FALSE))
  expect_equal(check_class(62, 3000, c("206", "210")), c(FALSE, FALSE))
  expect_equal(check_class(c(62, 63), 800, "210"), c(FALSE, TRUE))
  expect_equal(check_class(-10, 200, "206"), NA)
})

test_that("invalid matches return false", {
  expect_false(check_class(50, 800, "190"), FALSE)
  expect_false(check_class(60, 1000, "202"))
  expect_false(check_class(62, 1500, "205"))
})


# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(check_class("50", 800, 202))
  expect_condition(check_class(50, "800", "202"))
  expect_condition(check_class(50, 800, 202))
})

test_that("invalid vector lengths stop process", {
  expect_condition(check_class(50, c(800, 1000, 2000), c("203", "204")))
  expect_condition(check_class(c(50, 80), c(800, 1000, 2000), "204"))
  expect_condition(check_class(5, c(800, 1000), c("203", "204", "297")))
})
