context("test recp_clean_keep_dict_vars()")

##### TEST recp_clean_keep_dict_vars() #####

# List of cols usually removed from raw SQL data
cols_usually_removed <- c(
  "GAR2_SIZE", "GAR2_CNST", "GAR2_ATT", "GAR2_AREA",
  "VOLUME", "NCU", "TAX_CD"
)

# Test for expected outputs
test_that("output is as expected", {
  expect_equal(
    chars_sample_universe %>% dplyr::select(-any_of(cols_usually_removed)),
    recp_clean_keep_dict_vars(chars_sample_universe)
  )
  expect_equal(
    chars_sample_universe %>%
      dplyr::select(-any_of(c(cols_usually_removed, "GAR1_SIZE"))),
    recp_clean_keep_dict_vars(chars_sample_universe, cols_to_rm = "GAR1_SIZE")
  )
  expect_equal(
    chars_sample_universe %>%
      dplyr::select(-any_of(c(cols_usually_removed, "GAR3_SIZE"))),
    recp_clean_keep_dict_vars(chars_sample_universe, cols_to_rm = "GAR3_SIZE")
  )
  expect_equal(recp_clean_keep_dict_vars(data.frame(), "cat"), data.frame())
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(recp_clean_keep_dict_vars(9))
  expect_condition(recp_clean_keep_dict_vars(chars_sample_universe, 85))
  expect_condition(recp_clean_keep_dict_vars(chars_sample_universe, list("te")))
})


context("test recp_clean_rename()")

##### TEST recp_clean_rename() #####

# Test for expected outputs
test_that("output is as expected", {
  expect_equal(
    names(recp_clean_rename(chars_sample_universe[, 21:32])),
    c(
      "char_apts", "char_ext_wall", "char_roof_cnst", "char_rooms", "char_beds",
      "char_bsmt", "char_bsmt_fin", "char_heat", "char_oheat", "char_air",
      "char_frpl", "char_attic_type"
    )
  )
  expect_equal(
    names(recp_clean_rename(
      chars_sample_addchars[, 5:9],
      names_from = "addchars", names_to = "standard"
    )),
    c(
      "QU_HOME_IMPROVEMENT", "char_use", "char_ext_wall",
      "char_roof_cnst", "char_bsmt"
    )
  )
  expect_equal(
    names(recp_clean_rename(cdu_dict, names_to = "pretty")),
    c("cdu_code", "cdu_type", "cdu_desc", "cdu_desc_short")
  )
  expect_equal(
    recp_clean_rename(chars_sample_universe[, 14:19], type = "vector"),
    c(
      "meta_certified_est_land", "meta_modeling_group", "char_age",
      "meta_multi_code", "meta_per_ass", "meta_cdu"
    )
  )
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(recp_clean_rename(1))
  expect_condition(recp_clean_rename(chars_sample_universe, names_to = "HEADT"))
  expect_condition(recp_clean_rename(chars_sample_universe, names_from = "OPE"))
  expect_condition(recp_clean_rename(chars_sample_universe, type = "list"))
})


context("test recp_clean_recode()")

##### TEST recp_clean_recode() #####

recode_sample_data <- recp_clean_rename(
  chars_sample_universe[1:10, c(1, 2, 4, 16, 21:32)]
)

# Test for expected outputs
test_that("output is as expected", {
  expect_known_hash(recp_clean_recode(recode_sample_data), hash = "041a6fbead")
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(recp_clean_recode("cat"))
  expect_condition(recp_clean_recode(recode_sample_data[, 5:10]))
})


context("test recp_clean_relocate()")

##### TEST recp_clean_relocate() #####

# Test for expected outputs
test_that("output is as expected", {
  expect_equal(
    names(recp_clean_relocate(recode_sample_data[, c(10, 2, 6, 11)])),
    c("meta_year", "char_bsmt", "char_ext_wall", "char_bsmt_fin")
  )
  expect_known_hash(
    recp_clean_relocate(chars_sample_universe),
    hash = "b8889038e8"
  )
  expect_known_hash(
    recp_clean_relocate(recp_clean_rename(chars_sample_universe)),
    hash = "19de18af2e"
  )
})

# Test that invalid inputs throw errors
test_that("invalid data types stop process", {
  expect_condition(recp_clean_relocate("cat"))
  expect_condition(recp_clean_relocate(data.frame()))
})
