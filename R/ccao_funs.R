# nolint start
#' Calculate sales ratio statistics using CCAO SOPs
#'
#' @description Calculate sales ratio statistics (COD, PRD, PRB) according
#' to Cook County Assessor's Office Standard Operating Procedures (SOPs).
#'
#' The main SOPs affecting these calculations are:
#'
#' 1. Outlier trimming. Sales ratio performance statistics are quite sensitive
#' to outliers. As a result, the top and bottom 5\% of the input vector(s) are
#' always dropped.
#'
#' 2. Minimum sample size. Sales ratio performance statistics can be inaccurate
#' or misleading when the input sample is small. As a result, samples with less
#' than 20 observations after trimming outliers will stop execution or will
#' return NA if \code{suppress = TRUE}.
#'
#' For more information on how each statistic or its confidence interval is
#' calculated. See its respective function in the
#' \href{https://github.com/ccao-data/assessr}{AssessR package}.
#'
#' @param ratio A numeric vector of ratios centered around 1, where the
#'   numerator of the ratio is the estimated fair market value and the
#'   denominator is the actual sale price. NOTE: CCAO typically uses lagged
#'   or leading ratios to lessen the effect of sales chasing.
#' @param assessed A numeric vector of assessed values. Must be the same
#'   length as \code{sale_price}.
#' @param sale_price A numeric vector of sale prices. Must be the same length
#'   as \code{assessed}.
#' @param suppress Default FALSE. A boolean value indicating whether or not
#'   to ignore minimum sample size requirements (N must be >= 20). If TRUE when
#'   N < 20, will return NAs.
#' @param na.rm Default FALSE. A boolean value indicating whether or not to
#'   remove NA values. If missing values are present but not removed the
#'   function will output NA.
#'
#' @return A named list containing the statistic, its 95% confidence interval,
#'   whether or not the statistic meets the IAAO standard, whether or not the
#'   95% confidence interval overlaps the IAAO standard, and the number of
#'   observations used to calculate the statistic (after outliers are removed).
#'
#' @examples
#'
#' # Load sample data from assessr
#' df <- assessr::ratios_sample
#'
#' # Calculate performance stats using CCAO SOP defaults
#' ccao_cod(df$ratio)
#' ccao_prd(df$assessed, df$sale_price)
#' ccao_prb(df$assessed, df$sale_price)
#' @md
#' @family ccao_funs
#' @name ccao_funs
NULL
# nolint end


#' @describeIn ccao_funs Return named list of CCAO SOP compliant COD statistics.
#'
#' @family ccao_funs
#' @export
ccao_cod <- function(ratio, suppress = FALSE, na.rm = FALSE) { # nolint

  # Input checking and error handling
  assessr:::check_inputs(ratio)

  # Remove top and bottom 5% of ratios according to CCAO SOPs
  no_outliers <- ratio[suppressWarnings(!assessr::is_outlier(
    ratio,
    method = "quantile",
    probs = c(0.05, 0.95)
  ))]

  # Get number of observations used to calculate stat
  cod_n <- length(no_outliers[!is.na(no_outliers)])

  # Only sample with 20+ observations are reliable for sales ratio studies
  # If less than 20, return only NAs and warn
  if (cod_n >= 20) {
    # Calculate COD of trimmed ratios
    cod_val <- assessr::cod(no_outliers, na.rm = na.rm)

    # Calculate bootstrapped COD confidence interval
    cod_ci <- assessr::cod_ci(no_outliers, na.rm = na.rm, nboot = 1000)

    # Check if CI range overlaps with IAAO range (statistically meets target)
    cod_ci_met <- max(cod_ci[1], 5) <= min(cod_ci[2], 15)

    # Generate output list of COD, CI, standard, CI meets standard, and N
    out <- list(
      cod_val,
      cod_ci,
      assessr::cod_met(cod_val),
      cod_ci_met,
      cod_n
    )
  } else {
    # Generate empty output list and stop if N < 20 unless suppress
    out <- list(NA, NA, NA, NA, cod_n)

    if (!suppress) stop("Too few obs. (N < 20) for reliable ratio statistics")
  }

  names(out) <- c("COD", "COD_CI", "COD_MET", "COD_CI_MET", "COD_N")
  return(out)
}


#' @describeIn ccao_funs Return named list of CCAO SOP compliant PRD statistics.
#'
#' @family ccao_funs
#' @export
ccao_prd <- function(assessed, sale_price, suppress = FALSE, na.rm = FALSE) { # nolint

  # Input checking and error handling
  assessr:::check_inputs(assessed, sale_price)

  # Create a dataframe of AVs, sales, and ratios. Makes it easier to filter
  # vs filtering and indexing three separate vectors
  ratios_df <- data.frame(
    assessed,
    sale_price,
    "ratio" = assessed / sale_price
  )

  # Remove top and bottom 5% of ratios according to CCAO SOPs, output dataframe
  no_outliers_df <- ratios_df[suppressWarnings(!assessr::is_outlier(
    ratios_df$ratio,
    method = "quantile",
    probs = c(0.05, 0.95)
  )), ]

  # Get number of observations used to calculate stat
  prd_n <- nrow(no_outliers_df[!is.na(no_outliers_df$ratio), ])

  # Only sample with 20+ observations are reliable for sales ratio studies
  # If less than 20, return only NAs and warn
  if (prd_n >= 20) {
    # Calculate PRD of trimmed ratios
    prd_val <- assessr::prd(
      no_outliers_df$assessed,
      no_outliers_df$sale_price,
      na.rm = na.rm
    )

    # Calculate bootstrapped PRD confidence interval
    prd_ci <- assessr::prd_ci(
      no_outliers_df$assessed,
      no_outliers_df$sale_price,
      na.rm = na.rm,
      nboot = 1000
    )

    # Check if CI range overlaps with IAAO range (statistically meets target)
    prd_ci_met <- max(prd_ci[1], 0.98) <= min(prd_ci[2], 1.03)

    # Generate output list of PRD, CI, standard, CI meets standard, and N
    out <- list(
      prd_val,
      prd_ci,
      assessr::prd_met(prd_val),
      prd_ci_met,
      prd_n
    )
  } else {
    # Generate empty output list and stop if N < 20 unless suppress
    out <- list(NA, NA, NA, NA, prd_n)

    if (!suppress) stop("Too few obs. (N < 20) for reliable ratio statistics")
  }

  names(out) <- c("PRD", "PRD_CI", "PRD_MET", "PRD_CI_MET", "PRD_N")
  return(out)
}


#' @describeIn ccao_funs Return named list of CCAO SOP compliant PRB statistics.
#'
#' @family ccao_funs
#' @export
ccao_prb <- function(assessed, sale_price, suppress = FALSE, na.rm = FALSE) { # nolint

  # Input checking and error handling
  assessr:::check_inputs(assessed, sale_price)

  # Create a dataframe of AVs, sales, and ratios. Makes it easier to filter
  # vs filtering and indexing three separate vectors
  ratios_df <- data.frame(
    assessed,
    sale_price,
    "ratio" = assessed / sale_price
  )

  # Remove top and bottom 5% of ratios according to CCAO SOPs, output dataframe
  no_outliers_df <- ratios_df[suppressWarnings(!assessr::is_outlier(
    ratios_df$ratio,
    method = "quantile",
    probs = c(0.05, 0.95)
  )), ]

  # Get number of observations used to calculate stat
  prb_n <- nrow(no_outliers_df[!is.na(no_outliers_df$ratio), ])

  # Only sample with 20+ observations are reliable for sales ratio studies
  # If less than 20, return only NAs and warn
  if (prb_n >= 20) {
    # Calculate PRD of trimmed ratios
    prb_val <- assessr::prb(
      no_outliers_df$assessed,
      no_outliers_df$sale_price,
      na.rm = na.rm
    )

    # Calculate bootstrapped PRB confidence interval
    prb_ci <- assessr::prb_ci(
      no_outliers_df$assessed,
      no_outliers_df$sale_price,
      na.rm = na.rm
    )

    # Check if CI range overlaps with IAAO range (statistically meets target)
    prb_ci_met <- max(prb_ci[1], -0.05) <= min(prb_ci[2], 0.05)

    # Generate output list of PRD, CI, standard, CI meets standard, and N
    out <- list(
      prb_val,
      prb_ci,
      assessr::prb_met(prb_val),
      prb_ci_met,
      prb_n
    )
  } else {
    # Generate empty output list and stop if N < 20 unless suppress
    out <- list(NA, NA, NA, NA, prb_n)

    if (!suppress) stop("Too few obs. (N < 20) for reliable ratio statistics")
  }

  names(out) <- c("PRB", "PRB_CI", "PRB_MET", "PRB_CI_MET", "PRB_N")
  return(out)
}


# nolint start
#' Generate a memorable ID string based on CCAO employee names
#'
#' @description Memorable IDs are useful for referencing objects with arbitrary
#' versions. This function uses the
#' \href{https://github.com/moby/moby/blob/master/pkg/namesgenerator/names-generator.go}{Docker name generator code}
#' to create random memorable IDs using Data Department employee, interns, and
#' fellow names.
#'
#' @param n Integer of the number of IDs to generate. Default 1.
#' @param prefix Prefix to append to the end of each ID, uses the system date
#'   by default. Set to NULL for no prefix or a vector of length n for multiple
#'   different prefixes.
#'
#' @return An n-long character vector of memorable IDs.
#'
#' @examples
#' # Generate a single ID
#' ccao_generate_id()
#'
#' # Multiple IDs
#' ccao_generate_id(10)
#'
#' # IDs with no prefix
#' ccao_generate_id(3, NULL)
#'
#' # Different prefix for each ID
#' ccao_generate_id(3, c("type1", "type2", "type3"))
#' @export
ccao_generate_id <- function(n = 1L, prefix = as.character(Sys.Date())) {
  stopifnot(
    is.numeric(n),
    n >= 1,
    is.character(prefix) | is.null(prefix),
    length(prefix) == 1 | length(prefix) == n | is.null(prefix)
  )

  left <- names_gen$left[stats::runif(n, 1, length(names_gen$left))]
  right <- names_gen$right[stats::runif(n, 1, length(names_gen$right))]

  out <- paste(prefix, left, right, sep = "-")
  out <- gsub("[- ]+$", "", out)

  return(out)
}
# nolint end

#' Download one or more DVC-tracked input datasets for a given model run
#'
#' @description Downloads one or more model input datasets referenced by a
#' `run_id` in `model.metadata`.
#'
#' Folder layout depends on the model year and assessment group:
#' - For `assessment_year <= 2024`, the legacy DVC layout is used.
#' - For `assessment_year >= 2025`, the path includes `model-res-avm` or
#'   `model-condo-avm` depending on `assessment_group`.
#'
#' @param model_run Character `run_id` found in `model.metadata`.
#' @param files Character vector of file keys to download. Valid keys are:
#'   `assessment`, `complex_id`, `land_nbhd`, `land_site`, `training`, `char`,
#'   `hie`, `condo_strata`.
#'
#' @return If `files` has length 1, returns a single data frame (as returned by
#' `arrow::read_parquet()`). If `files` has length > 1, returns a named list of
#' data frames with names equal to `files`.
#'
#' @examples
#' # Download a single dataset
#' char_data <- ccao_download_input_data("2025-01-11-gallant-rina", "char")
#'
#' # Download multiple datasets (returns a named list)
#' inputs <- ccao_download_input_data(
#'   "2025-01-11-gallant-rina",
#'   c("char", "training", "assessment")
#' )
#'
#' # Access one dataset from the list
#' training_data <- inputs[["training"]]
#' @export
ccao_download_input_data <- function(model_run, files) {
  con <- DBI::dbConnect(
    noctua::athena(
      s3_staging_dir = "s3://ccao-athena-results-us-east-1/",
      region_name = "us-east-1"
    )
  )

  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Pull the DVC hashes for this run_id
  dvc_params <- DBI::dbGetQuery(
    con,
    glue::glue("
      SELECT
        assessment_year,
        assessment_group,
        dvc_md5_assessment_data,
        dvc_md5_complex_id_data,
        dvc_md5_land_nbhd_rate_data,
        dvc_md5_land_site_rate_data,
        dvc_md5_training_data,
        dvc_md5_char_data,
        dvc_md5_hie_data,
        dvc_md5_condo_strata_data
      FROM model.metadata
      WHERE run_id = '{model_run}'
    ")
  )

  # Map file key -> md5 column
  md5_map <- c(
    assessment   = "dvc_md5_assessment_data",
    complex_id   = "dvc_md5_complex_id_data",
    land_nbhd    = "dvc_md5_land_nbhd_rate_data",
    land_site    = "dvc_md5_land_site_rate_data",
    training     = "dvc_md5_training_data",
    char         = "dvc_md5_char_data",
    hie          = "dvc_md5_hie_data",
    condo_strata = "dvc_md5_condo_strata_data"
  )

  valid_files <- names(md5_map)

  invalid_files <- setdiff(files, valid_files)

  if (length(invalid_files) > 0) {
    stop(
      glue::glue(
        "Invalid file key(s): {paste(invalid_files, collapse = ', ')}.\n",
        "Valid options are: {paste(valid_files, collapse = ', ')}."
      ),
      call. = FALSE
    )
  }

  yr <- (as.integer(dvc_params$assessment_year))

  grp <- (dvc_params$assessment_group)

  # Folder logic:
  # - <= 2025: old layout
  # - >= 2026: split by assessment_group
  model_folder <- if (yr <= 2025) {
    ""
  } else if (grp == "condo") {
    "model-condo-avm/"
  } else {
    "model-res-avm/"
  }

  # Helper to read a single file
  read_file <- function(f) {
    md5_col <- md5_map[[f]]
    dvc_hash <- dvc_params[[md5_col]]

    if (is.na(dvc_hash) || !nzchar(dvc_hash)) {
      stop(glue::glue(
        "Missing/empty {md5_col} for run_id = '{model_run}'"
      ))
    }

    s3_path <- glue::glue(
      s3_staging_dir,
      if (nzchar(model_folder)) glue::glue("{model_folder}") else "",
      "files/md5/",
      substr(dvc_hash, 1, 2), "/",
      substr(dvc_hash, 3, 32)
    )

    arrow::read_parquet(s3_path)
  }

  result <- lapply(files, read_file)
  names(result) <- files

  # Return a single object if only one file requested
  if (length(result) == 1) {
    return(result[[1]])
  }

  result
}
