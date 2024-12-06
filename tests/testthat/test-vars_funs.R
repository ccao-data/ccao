context("test vars_rename()")

##### TEST vars_rename() #####

# Test for expected outputs
test_that("output is as expected", {
  expect_equal(
    names(vars_rename(
      data = chars_sample_universe[, 21:32],
      names_from = "sql",
      names_to = "standard",
      dictionary = ccao::vars_dict_legacy
    )),
    c(
      "char_apts", "char_ext_wall", "char_roof_cnst", "char_rooms", "char_beds",
      "char_bsmt", "char_bsmt_fin", "char_heat", "char_oheat", "char_air",
      "char_frpl", "char_attic_type"
    )
  )
  expect_equal(
    names(vars_rename(
      cdu_dict,
      names_from = "sql",
      names_to = "pretty",
      dictionary = ccao::vars_dict_legacy
    )),
    c("cdu_code", "cdu_type", "cdu_desc", "cdu_desc_short")
  )
  expect_equal(
    vars_rename(
      data = chars_sample_universe[, 14:19],
      names_from = "sql",
      names_to = "standard",
      output_type = "vector",
      dictionary = ccao::vars_dict_legacy
    ),
    c(
      "meta_certified_est_land", "meta_modeling_group", "char_age",
      "meta_multi_code", "meta_per_ass", "meta_cdu"
    )
  )
  expect_equal(
    names(vars_rename(
      data = chars_sample_athena[, 14:19],
      names_from = "athena",
      names_to = "pretty",
      dictionary = ccao::vars_dict
    )),
    c(
      "Apartments", "Cathedral Ceiling", "Attic Finish",
      "Garage 1 Attached", "Garage 1 Area Included", "Garage 1 Size"
    )
  )
  expect_equal(
    vars_rename(
      data = c("apts", "condition_desirability_and_utility", "per_ass"),
      names_from = "socrata",
      names_to = "standard",
      dictionary = ccao::vars_dict_legacy
    ),
    c("char_apts", "meta_cdu", "meta_per_ass")
  )
  expect_equal(
    vars_rename(
      data = c("APTS", "EXT_WALL", "BEDS"),
      names_from = "sql",
      names_to = "standard",
      dictionary = ccao::vars_dict_legacy
    ),
    c("char_apts", "char_ext_wall", "char_beds")
  )
  expect_equal(
    vars_rename(
      data = c("char_apts", "char_ext_wall", "char_beds"),
      names_from = "athena",
      names_to = "iasworld",
      dictionary = ccao::vars_dict
    ),
    c("user14", "extwall", "rmbed")
  )
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(vars_rename(1))
  expect_error(
    vars_rename(
      data = chars_sample_universe,
      names_to = "HEADT",
      names_from = "sql",
      dictionary = ccao::vars_dict_legacy
    )
  )
  expect_error(
    vars_rename(
      data = chars_sample_universe,
      names_from = "OPEN",
      names_to = "sql",
      dictionary = ccao::vars_dict_legacy
    )
  )
  expect_error(
    vars_rename(
      data = chars_sample_universe,
      names_from = "sql",
      names_to = "pretty",
      output_type = "list",
      dictionary = ccao::vars_dict_legacy
    )
  )
  expect_error(
    vars_rename(
      data = chars_sample_universe,
      names_from = "sql",
      names_to = NULL,
      dictionary = ccao::vars_dict_legacy
    )
  )
  expect_error(
    vars_rename(
      data = chars_sample_universe,
      names_from = NULL,
      names_to = "sql",
      dictionary = ccao::vars_dict_legacy
    )
  )
  expect_error(
    vars_rename(
      data = chars_sample_universe,
      names_from = NULL,
      names_to = "sql",
      dictionary = c("sql" = "char")
    )
  )
  expect_error(
    vars_rename(
      data = chars_sample_universe,
      names_from = NULL,
      names_to = "sql",
      dictionary = ccao::vars_dict_legacy[, 5:10]
    )
  )
  expect_error(
    vars_rename(
      data = chars_sample_universe,
      names_from = "sql",
      names_to = "pretty",
      dictionary = ccao::vars_dict
    )
  )
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
    levels = c(
      "Shingle + Asphalt", "Tar + Gravel", "Slate", "Shake", "Tile", "Other"
    )
  )
)

recode_test_data_athena <- dplyr::tibble(
  pin = rep("12345", 4),
  char_ext_wall = c("1", "2", "0", NA),
  char_bsmt = c("1", "3", "4", "5"),
  value = 1000:1003,
  roof_cnst = c("1", "2", "4", "3")
)

recode_correct_athena <- dplyr::tibble(
  PIN = rep("12345", 4),
  EXT_WALL = factor(
    c("FRAM", "MASR", NA, NA),
    levels = c("FRAM", "MASR", "FRMA", "STUC")
  ),
  BSMT = factor(
    c("FL", "PT", "CR", NA),
    levels = c("FL", "SL", "PT", "CR")
  ),
  value = 1000:1003,
  roof_cnst = c("1", "2", "4", "3")
)

# Test for expected outputs
test_that("output is as expected", {
  expect_known_hash(
    vars_recode(
      data = chars_sample_universe,
      dictionary = ccao::vars_dict_legacy
    ),
    hash = "8c41990e86"
  )
  expect_known_hash(
    vars_recode(data = chars_sample_athena, code_type = "long"),
    hash = "d3f8b1e3cd"
  )
  expect_equivalent(
    vars_recode(
      data = recode_test_data,
      dictionary = ccao::vars_dict_legacy
    ),
    recode_correct
  )
  expect_equivalent(
    vars_recode(data = recode_test_data_athena, code_type = "short"),
    recode_correct_athena
  )
  expect_equivalent(
    vars_recode(
      data = recode_test_data,
      as_factor = FALSE,
      dictionary = ccao::vars_dict_legacy
    ),
    recode_correct %>%
      dplyr::mutate(dplyr::across(where(is.factor), as.character))
  )
  expect_known_hash(
    vars_recode(
      data = chars_sample_universe,
      code_type = "short",
      dictionary = ccao::vars_dict_legacy
    ),
    hash = "ecd0d79b5d"
  )
  expect_known_hash(
    vars_recode(
      data = chars_sample_universe,
      code_type = "short",
      as_factor = FALSE,
      dictionary = ccao::vars_dict_legacy
    ),
    hash = "aed980d873"
  )
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_error(
    vars_recode(
      data = "cat",
      dictionary = ccao::vars_dict_legacy
    )
  )
  expect_error(
    vars_recode(
      data = chars_sample_universe,
      code_type = "HEADT",
      dictionary = ccao::vars_dict_legacy
    )
  )
  expect_error(
    vars_recode(
      data = chars_sample_universe,
      dictionary = ccao::vars_dict_legacy[, 5:10]
    )
  )
  expect_error(
    vars_recode(
      data = chars_sample_universe,
      dictionary = ccao::vars_dict_legacy[, 6:14]
    )
  )
})
