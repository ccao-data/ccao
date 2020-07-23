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
#' than 30 observations after trimming outliers will stop execution or will
#' return NA if \code{suppress = TRUE}.
#'
#' For more information on how each statistic or its confidence interval is
#' calculated. See its respective function in the
#' \href{https://gitlab.com/ccao-data-science---modeling/packages/assessr}{AssessR package}.
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
#'   to ignore minimum sample size requirements (N must be >= 30). If TRUE when
#'   N < 30, will return NAs.
#' @param na.rm Default FALSE. A boolean value indicating whether or not to
#'   remove NA values. If missing values are present but not removed the
#'   function will output NA.
#'
#' @return A named list containing the statistic, its 95\% confidence interval,
#'   whether or not the statistic meets IAAO standards, and the number of
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
    probs = c(0.05, 0.95),
    na.rm = na.rm
  ))]

  # Only sample with 30+ observations are reliable for sales ratio studies
  # If less than 30, return only NAs and warn
  if (length(no_outliers) >= 30) {

    # Calculate COD of trimmed ratios
    cod_val <- assessr::cod(no_outliers, na.rm = na.rm)

    # Generate output list of COD, CI, standard, and N
    out <- list(
      cod_val,
      assessr::cod_ci(no_outliers, na.rm = na.rm, nboot = 1000),
      assessr::cod_met(cod_val),
      length(no_outliers)
    )
  } else {

    # Generate empty output list and stop if N < 30 unless suppress
    out <- list(NA, NA, NA, length(no_outliers))

    if (!suppress) stop("Too few obs. (N < 30) for reliable ratio statistics")
  }

  names(out) <- c("COD", "COD_CI", "COD_MET", "COD_N")
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
    probs = c(0.05, 0.95),
    na.rm = na.rm
  )), ]

  # Only sample with 30+ observations are reliable for sales ratio studies
  # If less than 30, return only NAs and warn
  if (nrow(no_outliers_df) >= 30) {

    # Calculate PRD of trimmed ratios
    prd_val <- assessr::prd(
      no_outliers_df$assessed,
      no_outliers_df$sale_price,
      na.rm = na.rm
    )

    # Generate output list of PRD, CI, standard, and N
    out <- list(
      prd_val,
      assessr::prd_ci(
        no_outliers_df$assessed,
        no_outliers_df$sale_price,
        na.rm = na.rm,
        nboot = 1000
      ),
      assessr::prd_met(prd_val),
      nrow(no_outliers_df)
    )
  } else {

    # Generate empty output list and stop if N < 30 unless suppress
    out <- list(NA, NA, NA, nrow(no_outliers_df))

    if (!suppress) stop("Too few obs. (N < 30) for reliable ratio statistics")
  }

  names(out) <- c("PRD", "PRD_CI", "PRD_MET", "PRD_N")
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
    probs = c(0.05, 0.95),
    na.rm = na.rm
  )), ]

  # Only sample with 30+ observations are reliable for sales ratio studies
  # If less than 30, return only NAs and warn
  if (nrow(no_outliers_df) >= 30) {

    # Calculate PRD of trimmed ratios
    prb_val <- assessr::prb(
      no_outliers_df$assessed,
      no_outliers_df$sale_price,
      na.rm = na.rm
    )

    # Generate output list of PRD, CI, standard, and N
    out <- list(
      prb_val,
      assessr::prb_ci(
        no_outliers_df$assessed,
        no_outliers_df$sale_price,
        na.rm = na.rm
      ),
      assessr::prb_met(prb_val),
      nrow(no_outliers_df)
    )
  } else {

    # Generate empty output list and stop if N < 30 unless suppress
    out <- list(NA, NA, NA, nrow(no_outliers_df))

    if (!suppress) stop("Too few obs. (N < 30) for reliable ratio statistics")
  }

  names(out) <- c("PRB", "PRB_CI", "PRB_MET", "PRB_N")
  return(out)
}
