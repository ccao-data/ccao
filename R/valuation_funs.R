#' Limit excessive sales ratios
#'
#' @description Limit very high and low ratios by capping them to a fixed
#' boundary. If a property has a sale, its ratio should be adjusted to be within
#' these bounds.
#'
#' For example, a property with an estimate of \$300,000 and a sale of
#' \$100,000 will have a sale ratio of 3. This function would lower the estimate
#' to the specified lower bound. If the lower bound were 2, then the estimate
#' would become \$200,000.
#'
#' @param truth Vector of true market values. Typically sale prices.
#' @param estimate Vector of estimated market values. Typically model results.
#' @param lower The lower ratio bound for properties with sales. Typically 0.7.
#' @param upper The upper ratio bound for properties with sales. Typically 2.
#'
#' @examples
#' sales <- c(20000, 15000, 30000, NA, 55000, 40000, NA)
#' estimates <- c(30000, 50000, 31000, 10000, 200000, 30000, 20000)
#' val_limit_ratios(sales, estimates, lower = 0.7, upper = 2.0)
#' @return A vector of adjusted estimates.
#'
#' @family valuation_funs
#' @export
val_limit_ratios <- function(truth, estimate, lower, upper) {
  stopifnot(
    is.numeric(truth),
    is.numeric(estimate),
    is.numeric(lower),
    is.numeric(upper),
    length(truth) == length(estimate),
    length(lower) == 1,
    length(upper) == 1,
    upper > 0,
    lower > 0
  )

  ratio <- estimate / truth

  ratio_high <- !is.na(ratio) & ratio > upper
  ratio_low <- !is.na(ratio) & ratio < lower

  estimate[ratio_high] <- truth[ratio_high] * upper
  estimate[ratio_low] <- truth[ratio_low] * lower

  return(estimate)
}


#' Round model values to flat amounts
#'
#' @description Estimated values produced by the CCAO are sometimes rounded to
#'   a nearby flat amount e.g. nearest $5K, nearest $100, etc. This is done
#'   to indicate that model values are estimates, rather than precise
#'   amounts.
#'
#'   The function supports applying different rounding amounts to different
#'   ranges of market values. For example, estimated property values below
#'   $200K might be rounded to the nearest $1,000, while estimated property
#'   values below $1M might be rounded to the nearest $5,000.
#'
#' @param x A numeric vector of market or assessed values. All values must be
#'   greater than or equal to 0.
#' @param breaks A numeric vector of breaks used to split \code{x} into ranges
#'   that receive different rounding amounts. The default splits \code{x} at
#'   $100K, with values above $100K rounded down to the nearest $10K, and values
#'   below $100K rounded down to the nearest $5K.
#' @param round_to A numeric vector of values to round to. Must be 1 longer than
#'   \code{breaks}. Default rounds down to the nearest $5K and $10K.
#' @param type A character value indicating the type of rounding to use.
#'   Options are \code{"floor"}, \code{"normal"}, and \code{"ceiling"}. Default
#'   is \code{"floor"}.
#'
#' @return A numeric vector of rounded values.
#'
#' @export
val_round_fmv <- function(x, breaks = 1e5, round_to = c(5e3, 1e4), type = "floor") { # nolint

  # Input checking and error handling
  stopifnot(
    is.vector(x),
    is.numeric(x),
    length(x) >= 1,
    is.vector(breaks),
    is.numeric(breaks),
    all(breaks > 0),
    is.finite(breaks),
    is.vector(round_to),
    is.numeric(round_to),
    all(round_to > 0),
    is.finite(round_to),
    is.character(type),
    type %in% c("floor", "normal", "ceiling")
  )

  # Check that round_to is 1 longer than breaks
  if ((length(breaks) + 1) != length(round_to)) {
    stop("round_to vector length must be 1 longer than breaks length")
  }

  # Get the amount to round to for each value in the input vector
  round_amts <- as.numeric(as.character(cut(
    x = x,
    breaks = c(0, breaks, Inf),
    labels = round_to,
    include.lowest = TRUE
  )))

  # For ceiling, if a remainder exists, then add 1 round_amt
  # For normal, if a remainder exists that's more than half a round amount,
  # then add 1 round amount
  output <- switch(type,
    floor = round_amts * (
      x %/% round_amts * ifelse(is.finite(x) & !is.nan(x), 1, NA)
    ),
    ceiling = round_amts * (
      x %/% round_amts + as.logical(x %% round_amts)
    ),
    normal = round_amts * (
      x %/% round_amts + as.logical(x %% round_amts > (round_amts / 2))
    )
  )

  return(output)
}
