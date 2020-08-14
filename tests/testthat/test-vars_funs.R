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
  expect_equal(
    vars_rename(chars_sample_universe[, 14:19], type = "vector"),
    c(
      "meta_key_pin", "CONDO_STRATA_10", "CONDO_STRATA_100",
      "char_apts", "char_ext_wall", "char_roof_cnst"
    )
  )
  expect_equal(
    vars_rename(
      c("apts", "condition_desirability_and_utility", "per_ass"),
      names_from = "socrata"
    ),
    c("char_apts", "meta_cdu", "meta_per_ass")
  )
  expect_equal(
    vars_rename(c("APTS", "EXT_WALL", "BEDS")),
    c("char_apts", "char_ext_wall", "char_beds")
  )
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(vars_rename(1))
  expect_condition(vars_rename(chars_sample_universe, names_to = "HEADT"))
  expect_condition(vars_rename(chars_sample_universe, names_from = "OPEN"))
  expect_condition(vars_rename(chars_sample_universe, type = "list"))
})


context("test vars_recode()")

##### TEST vars_recode() #####

recode_test_data <- dplyr::tibble(
  PIN = rep("12345", 4),
  EXT_WALL = c("1", "2", "0", NA),
  BSMT = c("1", "3", "4", "5"),
  value = 1000:1003,
  roof_cnst = c("1", "2", "4", "3")
)

recode_correct <- dplyr::tibble(
  PIN = rep("12345", 4),
  EXT_WALL = factor(
    c("Frame", "Masonry", NA, NA),
    levels = c("Frame", "Masonry", "Frame + Masonry", "Stucco")
  ),
  BSMT = factor(
    c("Full", "Partial", "Crawl", NA),
    levels = c("Full", "Slab", "Partial", "Crawl")
  ),
  value = 1000:1003,
  roof_cnst = factor(
    c("Shingle + Asphalt", "Tar + Gravel", "Shake", "Slate"),
    levels = c("Shingle + Asphalt","Tar + Gravel", "Slate", "Shake", "Tile", "Other")
  )
)

# Test for expected outputs
test_that("output is as expected", {
  expect_known_hash(vars_recode(chars_sample_universe), hash = "dbc001b6f4")
  expect_equivalent(vars_recode(recode_test_data), recode_correct)
  expect_equivalent(
    vars_recode(recode_test_data, as_factor = FALSE),
    recode_correct %>%
      dplyr::mutate(dplyr::across(where(is.factor), as.character))
  )
  expect_known_hash(
    vars_recode(chars_sample_universe, type = "short"),
    hash = "c255ed2f9c"
  )
  expect_known_hash(
    vars_recode(chars_sample_universe, type = "short", as_factor = FALSE),
    hash = "b97f45a917"
  )
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(vars_recode("cat"))
  expect_condition(vars_recode(chars_sample_universe, type = "HEADT"))
})
