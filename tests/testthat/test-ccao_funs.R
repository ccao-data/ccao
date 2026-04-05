# Load the ratios sample dataset for testing
library(assessr)
data("ratios_sample")

# Extract the components of the dataframe as vectors
ratio <- ratios_sample$ratio
sale_price <- ratios_sample$sale_price
assessed <- ratios_sample$assessed


##### TEST ccao_cod() #####

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
  expect_condition(ccao_cod(runif(19)))
  expect_silent(ccao_cod(runif(19), suppress = TRUE))
  expect_equal(
    unname(ccao_cod(runif(19), suppress = TRUE)),
    list(NA, NA, NA, NA, 17)
  )
  expect_equal(
    unname(ccao_cod(c(ratio[1:19], rep(NA, 10)), suppress = TRUE)),
    list(NA, NA, NA, NA, 17)
  )
  expect_equal(
    unname(ccao_cod(rep(NA_real_, 40), suppress = TRUE)),
    list(NA, NA, NA, NA, 0)
  )
})


##### TEST ccao_prd() #####

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
  expect_condition(ccao_prd(runif(19), runif(19)))
  expect_silent(ccao_prd(runif(19), runif(19), suppress = TRUE))
  expect_equal(
    unname(ccao_prd(runif(19), runif(19), suppress = TRUE)),
    list(NA, NA, NA, NA, 17)
  )
  expect_equal(
    unname(ccao_prd(
      c(assessed[1:19], rep(NA, 10)),
      c(sale_price[1:19], rep(NA, 10)),
      suppress = TRUE
    )),
    list(NA, NA, NA, NA, 17)
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
  expect_condition(ccao_prb(runif(19), runif(19)))
  expect_silent(ccao_prb(runif(19), runif(19), suppress = TRUE))
  expect_equal(
    unname(ccao_prb(runif(19), runif(19), suppress = TRUE)),
    list(NA, NA, NA, NA, 17)
  )
  expect_equal(
    unname(ccao_prb(
      c(assessed[1:19], rep(NA, 10)),
      c(sale_price[1:19], rep(NA, 10)),
      suppress = TRUE
    )),
    list(NA, NA, NA, NA, 17)
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


##### TEST ccao_cod() #####

test_that("functions return character vector of length n", {
  expect_type(ccao_generate_id(), "character")
  expect_length(ccao_generate_id(), 1)
  expect_length(ccao_generate_id(10), 10)
})

test_that("no hanging dashes", {
  expect_false(all(grepl("[ -]+$", ccao_generate_id(1, prefix = ""))))
  expect_false(all(grepl("[ -]+$", ccao_generate_id(1, prefix = " "))))
  expect_false(all(grepl("[ -]+$", ccao_generate_id(1, prefix = NULL))))
  expect_false(all(grepl("[ -]+$", ccao_generate_id(2, prefix = c(NULL, "")))))
})

test_that("bad input data stops execution", {
  expect_condition(ccao_generate_id("10"))
  expect_condition(ccao_generate_id(2, 2))
  expect_condition(ccao_generate_id(3, c("cat", "dog")))
  expect_condition(ccao_generate_id(1, c("cat", "dog")))
  expect_condition(ccao_generate_id(0))
  expect_condition(ccao_generate_id(-1))
})

##### TEST ccao_download_model_input_data() #####

# This uses mock paths since we cannot connect to Athena during our tests.
# It solely checks if we return objects of the correct type / length
# and create the correct paths

test_ccao_download_model_input_data <- function(
  test_name,
  assessment_group,
  expected_path_regexes,
  expected_called_paths = NULL,
  succeed_on = "group_folder", # "group_folder", "root_md5", "root", or "none"
  run_id = "2025-01-11-gallant-rina",
  file_keys = c("complex_id", "land_nbhd_rate", "hie")
) {
  test_that(test_name, {
    called_paths <- character(0)

    mock_con <- structure(list(), class = "MockAthenaConnection")

    mock_dbConnect <- mockery::mock(mock_con, cycle = TRUE)
    mock_dbDisconnect <- mockery::mock(invisible(TRUE), cycle = TRUE)

    mock_dbGetQuery <- mockery::mock(
      data.frame(
        assessment_year = 2026L,
        assessment_group = as.character(assessment_group),
        dvc_md5_assessment_data = NA_character_,
        dvc_md5_complex_id_data = paste0(rep("a", 32), collapse = ""),
        dvc_md5_land_nbhd_rate_data = paste0(rep("b", 32), collapse = ""),
        dvc_md5_land_site_rate_data = NA_character_,
        dvc_md5_training_data = NA_character_,
        dvc_md5_char_data = NA_character_,
        dvc_md5_hie_data = paste0(rep("c", 32), collapse = ""),
        dvc_md5_condo_strata_data = NA_character_,
        stringsAsFactors = FALSE
      ),
      cycle = TRUE
    )

    expected_folder <- if (assessment_group == "condo") {
      "model-condo-avm"
    } else {
      "model-res-avm"
    }

    mock_read_parquet <- function(path, ...) {
      called_paths <<- c(called_paths, path)
      ok <- switch(succeed_on,
        group_folder = grepl(
          expected_folder,
          path
        ),
        root_md5 = !grepl(
          expected_folder,
          path
        ) && grepl("/files/md5/", path),
        root_flat = !grepl(
          expected_folder,
          path
        ) && !grepl("/files/md5/", path),
        none = FALSE
      )
      if (!ok) stop("Mock file not found")
      data.frame(.mock = TRUE, stringsAsFactors = FALSE)
    }

    mockery::stub(
      ccao_download_model_input_data,
      "DBI::dbConnect", mock_dbConnect
    )
    mockery::stub(
      ccao_download_model_input_data,
      "DBI::dbDisconnect", mock_dbDisconnect
    )
    mockery::stub(
      ccao_download_model_input_data,
      "DBI::dbGetQuery", mock_dbGetQuery
    )
    mockery::stub(
      ccao_download_model_input_data,
      "arrow::read_parquet", mock_read_parquet
    )

    if (succeed_on == "none") {
      expect_error(
        ccao_download_model_input_data(run_id, file_keys[1]),
        regexp = "Could not find",
        ignore.case = TRUE
      )
      if (!is.null(expected_called_paths)) {
        expect_equal(length(called_paths), expected_called_paths)
      }
    } else {
      # Multiple files, returns a list
      data <- ccao_download_model_input_data(run_id, file_keys)

      expect_type(data, "list")
      expect_length(data, length(file_keys))
      expect_setequal(names(data), file_keys)

      for (rx in expected_path_regexes) {
        expect_true(any(grepl(rx, called_paths)))
      }

      if (!is.null(expected_called_paths)) {
        expect_equal(length(called_paths), expected_called_paths)
      }

      # Single file returns the object directly, succeeds on first path tried
      called_paths <- character(0)
      single_data <- ccao_download_model_input_data(run_id, file_keys[1])
      expect_true(is.data.frame(single_data))
      expect_equal(
        length(called_paths),
        switch(succeed_on,
          group_folder = 1L,
          root_md5 = 2L,
          root_flat = 3L
        )
      )
    }

    # Missing / empty DVC hash should error without reading parquet
    called_paths <- character(0)

    mock_dbGetQuery_missing <- mockery::mock(
      data.frame(
        assessment_year = 2026L,
        assessment_group = as.character(assessment_group),
        dvc_md5_assessment_data = NA_character_,
        dvc_md5_complex_id_data = NA_character_,
        dvc_md5_land_nbhd_rate_data = NA_character_,
        dvc_md5_land_site_rate_data = NA_character_,
        dvc_md5_training_data = NA_character_,
        dvc_md5_char_data = NA_character_,
        dvc_md5_hie_data = NA_character_,
        dvc_md5_condo_strata_data = NA_character_,
        stringsAsFactors = FALSE
      ),
      cycle = TRUE
    )

    mockery::stub(
      ccao_download_model_input_data,
      "DBI::dbGetQuery", mock_dbGetQuery_missing
    )

    expect_error(
      ccao_download_model_input_data(run_id, "complex_id"),
      regexp = "Missing/empty.*run_id",
      ignore.case = TRUE
    )
    expect_equal(length(called_paths), 0)

    # Invalid file key should error without reading parquet
    called_paths <- character(0)
    expect_error(
      ccao_download_model_input_data(run_id, "bad_file_key"),
      regexp = "Invalid file key",
      ignore.case = TRUE
    )
    expect_equal(length(called_paths), 0)
  })
}

# --- res ---

test_ccao_download_model_input_data(
  test_name = "res succeeds on group folder (path 1)",
  assessment_group = "res",
  succeed_on = "group_folder",
  expected_path_regexes = c(
    "model-res-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
    "model-res-avm/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb$",
    "model-res-avm/files/md5/cc/cccccccccccccccccccccccccccccc$"
  ),
  expected_called_paths = 3
)

test_ccao_download_model_input_data(
  test_name = "res falls back to root md5 (path 2)",
  assessment_group = "res",
  succeed_on = "root_md5",
  expected_path_regexes = c(
    "model-res-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
    "/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
    "/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb$",
    "/files/md5/cc/cccccccccccccccccccccccccccccc$"
  ),
  expected_called_paths = 6
)

test_ccao_download_model_input_data(
  test_name = "res falls back to root flat (path 3)",
  assessment_group = "res",
  succeed_on = "root_flat",
  expected_path_regexes = c(
    "model-res-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
    "/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
    "/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
    "/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb$",
    "/cc/cccccccccccccccccccccccccccccc$"
  ),
  expected_called_paths = 9
)

test_ccao_download_model_input_data(
  test_name = "res errors when all paths fail",
  assessment_group = "res",
  succeed_on = "none",
  expected_path_regexes = c(),
  expected_called_paths = 3
)

# --- condo ---

test_ccao_download_model_input_data(
  test_name = "condo succeeds on group folder (path 1)",
  assessment_group = "condo",
  succeed_on = "group_folder",
  expected_path_regexes = c(
    "model-condo-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
    "model-condo-avm/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb$",
    "model-condo-avm/files/md5/cc/cccccccccccccccccccccccccccccc$"
  ),
  expected_called_paths = 3
)

test_ccao_download_model_input_data(
  test_name = "condo falls back to root md5 (path 2)",
  assessment_group = "condo",
  succeed_on = "root_md5",
  expected_path_regexes = c(
    "model-condo-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
    "/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
    "/files/md5/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb$",
    "/files/md5/cc/cccccccccccccccccccccccccccccc$"
  ),
  expected_called_paths = 6
)

test_ccao_download_model_input_data(
  test_name = "condo falls back to root flat (path 3)",
  assessment_group = "condo",
  succeed_on = "root_flat",
  expected_path_regexes = c(
    "model-condo-avm/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
    "/files/md5/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
    "/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa$",
    "/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb$",
    "/cc/cccccccccccccccccccccccccccccc$"
  ),
  expected_called_paths = 9
)

test_ccao_download_model_input_data(
  test_name = "condo errors when all paths fail",
  assessment_group = "condo",
  succeed_on = "none",
  expected_path_regexes = c(),
  expected_called_paths = 3
)
