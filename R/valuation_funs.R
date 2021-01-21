#' Calculate median percentage adjustments
#'
#' @description In cases where models create a biased output or have a skewed
#' error structure, this function can be used to calculate a fixed percentage
#' adjustment that will shift the distribution of errors toward the "true"
#' values.
#'
#' This should not be used lightly. In the case of adjusting modeled values
#' toward "true" sale prices, it should only be used with high confidence that
#' those sales are representative of the overall market.
#'
#' In practice, the primary use of this function is to adjust ratios at the very
#' low and very high end of the market.
#'
#' @param truth Vector of true market values. Typically sale prices.
#' @param estimate Vector of estimated market values. Typically model results.
#' @param min_n Minimum number of true values needed to make an adjustment. If
#'   the number of true values is less than \code{min_n}, then the function will
#'   return NA.
#' @param max_abs_adj The maximum absolute percentage adjustment that can be
#'   returned.
#' @param na.rm Whether or not to remove NAs from inputs.
#'
#' @examples
#' sales <- c(20000, 15000, 30000, NA, 55000, 40000, NA)
#' estimates <- c(30000, 20000, 31000, 10000, 100000, 30000, 20000)
#' val_med_pct_adj(sales, estimates, min_n = 3, max_abs_adj = 0.4)
#' @return A single percentage adjustment. Can be applied to estimated values to
#'   shift them toward the true values.
#'
#' @export
val_med_pct_adj <- function(truth, estimate, min_n, max_abs_adj, na.rm = TRUE) { # nolint
  stopifnot(
    is.numeric(truth),
    is.numeric(estimate),
    is.numeric(min_n),
    is.numeric(max_abs_adj),
    length(truth) == length(estimate),
    length(min_n) == 1,
    length(max_abs_adj) == 1,
    is.logical(na.rm)
  )

  num_sales <- sum(!is.na(truth))

  med_pct_adj <- stats::median((truth - estimate) / estimate, na.rm = na.rm)
  output <- ifelse(
    num_sales >= min_n,
    ifelse(
      abs(med_pct_adj) > max_abs_adj,
      0.4 * sign(med_pct_adj),
      med_pct_adj
    ),
    NA_real_
  )

  return(output)
}


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


#' Create a list of ntiles based on an input distribution.
#'
#' @description Create ntiles based on a numeric distribution. Min and max are
#' replaced with -Inf and Inf so that new values outside the initial range can
#' be used. This function is used in conjunction with
#' \code{\link{val_assign_ntile}}.
#'
#' @param x A numeric vector from which to determine ntiles.
#' @param probs Quantile probabilities or splits.
#' @param na.rm Whether or not to remove NAs from inputs.
#
#' @examples
#' x <- 1:1000
#' ccao:::val_create_ntiles(x, probs = c(0.25, 0.50, 0.75))
#'
#' estimates <- c(30000, 50000, 31000, 10000, 200000, 30000, 20000)
#' ccao:::val_create_ntiles(estimates, probs = c(0.25, 0.50, 0.75))
#' @return A list containing the ntile breakpoints, bounded by -Inf and Inf.
val_create_ntiles <- function(x, probs, na.rm = TRUE) { # nolint
  stopifnot(
    is.numeric(x),
    is.numeric(probs),
    is.logical(na.rm)
  )

  output <- list(c(
    -Inf,
    unique(stats::quantile(x, probs = probs, na.rm = na.rm, names = FALSE)),
    Inf
  ))
  output <- ifelse(all(is.na(x)), list(NA_real_), output)

  return(output)
}


#' Assign an ntile to a numeric value
#'
#' @description Assign a numeric value its respective ntile or quantile, given
#' a pre-made set. This function is used in conjunction with
#' \code{\link{val_create_ntiles}}.
#'
#' @param x A numeric vector to assign ntiles to.
#' @param ntiles A quantile or output from \code{\link{val_create_ntiles}}.
#'
#' @examples
#' x <- 1:100
#' cuts <- list(stats::quantile(x, probs = c(0.25, 0.75)))
#'
#' ccao:::val_assign_ntile(x, cuts)
#' @return A character vector with the assigned ntile range
#'   for each value of \code{x}.
val_assign_ntile <- function(x, ntiles) {
  stopifnot(
    is.numeric(x),
    is.list(ntiles)
  )

  output <- ifelse(
    !is.na(x),
    purrr::pmap_chr(
      list(num = x, cuts = ntiles),
      function(num, cuts) as.character(cut(num, breaks = cuts, dig.lab = 10))
    ),
    NA_character_
  )

  return(output)
}
