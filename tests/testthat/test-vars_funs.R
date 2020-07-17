context("test vars_check_class()")

##### TEST vars_check_class() #####

# Test for expected outputs
test_that("output is as expected", {
  expect_equal(vars_check_class(50, 800, "202"), TRUE)
  expect_equal(vars_check_class(50, 800, c("202", "299")), c(TRUE, TRUE))
  expect_equal(vars_check_class(50, 800, c("202", "203")), c(TRUE, FALSE))
  expect_equal(vars_check_class(62, 3000, c("206", "210")), c(FALSE, FALSE))
  expect_equal(vars_check_class(c(62, 63), 800, "210"), c(FALSE, TRUE))
  expect_equal(vars_check_class(-10, 200, "206"), NA)
})

test_that("invalid matches return false", {
  expect_false(vars_check_class(50, 800, "190"), FALSE)
  expect_false(vars_check_class(60, 1000, "202"))
  expect_false(vars_check_class(62, 1500, "205"))
})


# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(vars_check_class("50", 800, 202))
  expect_condition(vars_check_class(50, "800", "202"))
  expect_condition(vars_check_class(50, 800, 202))
})

test_that("invalid vector lengths stop process", {
  expect_condition(vars_check_class(50, c(800, 1000, 2000), c("203", "204")))
  expect_condition(vars_check_class(c(50, 80), c(800, 1000, 2000), "204"))
  expect_condition(vars_check_class(5, c(800, 1000), c("203", "204", "297")))
})


context("test vars_rename()")

##### TEST vars_rename() #####

# Test for expected outputs
test_that("output is as expected", {
  expect_equal(
    names(vars_rename(chars_sample_universe[, 17:28])),
    c(
      "char_apts", "char_ext_wall", "char_roof_cnst", "char_rooms", "char_beds",
      "char_bsmt", "char_bsmt_fin", "char_heat", "char_oheat", "char_air",
      "char_frpl", "char_attic_type"
    )
  )
  expect_equal(
    names(vars_rename(
      chars_sample_addchars[, 5:9],
      names_from = "addchars", names_to = "standard"
    )),
    c(
      "QU_HOME_IMPROVEMENT", "char_use", "char_ext_wall",
      "char_roof_cnst", "char_bsmt"
    )
  )
  expect_equal(
    names(vars_rename(cdu_dict, names_to = "pretty")),
    c("cdu_code", "cdu_type", "cdu_desc", "cdu_desc_short")
  )
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(vars_rename("cat"))
  expect_condition(vars_rename(chars_sample_universe, names_to = "HEADT"))
  expect_condition(vars_rename(chars_sample_universe, names_from = "OPEN"))
})
