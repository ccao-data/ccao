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
#' @family valuation_funs
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
#'
#' @family valuation_funs
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
#'
#' @family valuation_funs
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


#' Adjust identical properties by group
#'
#' @description Within the same development, townhomes with the same
#' characteristics (nearly identical) should have the same value. This function
#' groups such properties and then sets their estimated value to the median of
#' the group. It will also apply a median adjustment if enough sales are
#' available within the group.
#'
#' @param data A dataframe containing property values to be adjusted.
#' @param truth Column in data containing true market values. Typically
#'   sale prices.
#' @param estimate Column in data containing estimated market values. Typically
#'   model results.
#' @param class Column in data containing the property class. Used to filter
#'   out properties that should not be adjusted by this method.
#' @param townhome_group_cols Character vector of column names in the data
#'   used to define groups of identical homes. Should contain characteristic
#'   columns that would not vary for identical units. For example, number of
#'   square feet, number of bedrooms, exterior wall type, etc.
#' @param townhome_min_sales The minimum number of sales within a group needed
#'   to apply a percentage adjustment.
#' @param townhome_min_turnover The minimum amount of turnover within a group
#'   needed to apply a percentage adjustment. This is to prevent groups with
#'   dozens of properties from being adjusted by just a few sales.
#' @param townhome_max_abs_adj The maximum absolute percentage adjustment to
#'   make within a townhome group.
#'
#' @return A summarized dataframe with the adjustment and final value of each
#'   property group.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @family valuation_funs
#' @export
val_townhomes_by_group <- function(data, truth, estimate, class,
                                   townhome_group_cols, townhome_min_sales,
                                   townhome_min_turnover,
                                   townhome_max_abs_adj) {
  data %>%
    dplyr::filter({{ class }} %in% c("210", "295")) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(townhome_group_cols))) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::summarize(
      th_med_est = stats::median({{ estimate }}, na.rm = TRUE),
      th_med_sale = stats::median({{ truth }}, na.rm = TRUE),
      th_num_in_group = dplyr::n(),
      th_num_sales = sum(!is.na({{ truth }})),
      th_med_pct_adj = ccao::val_med_pct_adj(
        truth = {{ truth }},
        estimate = {{ estimate }},
        min_n = townhome_min_sales,
        max_abs_adj = townhome_max_abs_adj
      ),
      th_med_pct_adj = tidyr::replace_na(.data$th_med_pct_adj, 0),
      th_med_pct_adj = ifelse(
        .data$th_num_sales / .data$th_num_in_group >= townhome_min_turnover,
        .data$th_med_pct_adj, 0
      )
    ) %>%
    dplyr::mutate(
      th_final_value = rowSums(
        data.frame(
          .data$th_med_est,
          .data$th_med_est * .data$th_med_pct_adj
        ),
        na.rm = TRUE
      )
    ) %>%
    dplyr::ungroup()
}


#' Create and train a post-modeling adjustment model
#'
#' @description In addition to first-pass modeling, the CCAO also runs a much
#' simpler second-pass model. This second model is internally called
#' "post-modeling", and is responsible for correcting any deficiencies in
#' the first model's predictions. Specifically, post-modeling will:
#'
#' 1. Shift distributions of predicted values such that their median sale
#' ratio (predicted value / actual sale value) moves toward 1. This is done
#' by applying a percentage multiplier to all properties within a neighborhood,
#' modeling group, and estimated value quantile. This percentage multiplier
#' is capped at 40% and requires a minimum number of sales and minimum
#' turnover rate within each adjustment group. This is done to prevent broad
#' over-or under-assessment. Only a small proportion of all properties receive
#' this adjustment.
#'
#' 2. Alter assessed values for properties with sales in the last two years
#' if and only if their sale ratio falls outside specific thresholds. If a
#' property's sale ratio (predicted value / actual sale value) is less than 0.7
#' or greater than 2, its predicted value will be adjusted such that its ratio
#' is exactly at the threshold. For example, if a property sold for \$100K in
#' 2019 and our model predicts that it's value is \$65K, it will have a sale
#' ratio of 0.65, which is below the 0.7 threshold. As a result, the property's
#' predicted value will be adjusted to $70K.
#'
#' 3. Ensure that perfectly identical properties are identically valued.
#' For some property classes, such as townhouses, we manually adjust values
#' such that all identical properties in the same unit or complex receive the
#' same predicted value. This is accomplished by replacing individual predicted
#' values with the median predicted value for all properties in the same unit.
#' The median value is also adjusted using the process in step 1.
#'
#' @param data A dataframe containing values to be adjusted, sale prices, as
#'   well as the grouping columns listed in the other arguments.
#' @param truth Column in data containing true market values. Typically
#'   sale prices.
#' @param estimate Column in data containing estimated market values. Typically
#'   model results.
#' @param class Column in data containing the property class. Used to filter
#'   out properties that should not be adjusted by this method.
#' @param ntile_group_cols Character vector of column names in the data
#'   used to define ntile-based adjustment groups. Typically this is something
#'   like neighborhood, town code, and modeling group.
#' @param ntile_probs The breaks to use for creating and assigning grouping
#'   ntiles. Each property will be "assigned" an ntile based on its estimated
#'   value. This ntile will then be used as an additional grouping variable
#'   when making adjustments.
#' @param ntile_min_sales The minimum number of sales within an ntile group
#'   necessary to make a percentage adjustment.
#' @param ntile_min_turnover The minimum amount of turnover within an ntile
#'   group necessary to make a percentage adjustment.
#' @param ntile_max_abs_adj The maximum absolute percentage adjustment to make
#'   within an ntile group.
#' @param townhome_group_cols Character vector of column names in the data
#'   used to define groups of identical homes. Should contain characteristic
#'   columns that would not vary for identical units. For example, number of
#'   square feet, number of bedrooms, exterior wall type, etc.
#' @param townhome_min_sales The minimum number of sales within a group needed
#'   to apply a percentage adjustment.
#' @param townhome_min_turnover The minimum amount of turnover within a group
#'   needed to apply a percentage adjustment. This is to prevent groups with
#'   dozens of properties from being adjusted by just a few sales.
#' @param townhome_max_abs_adj The maximum absolute percentage adjustment to
#'   make within a townhome group.
#' @param ratio_cap_upper_bound The upper bound for sales ratios. Properties
#'   with ratios higher than this number will have their estimated value set
#'   exactly at the cap.
#' @param ratio_cap_lower_bound The lower bound for sales ratios. Properties
#'   with ratios lower than this number will have their estimated value set
#'   exactly at the cap.
#'
#' @return An object with a class of \code{postval_model} which can be used
#'   to adjust unseen data. See \code{\link{predict.postval_model}}.
#'
#' @md
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @family valuation_funs
#' @export
postval_model <- function(data, truth, estimate, class,
                          ntile_group_cols, ntile_probs, ntile_min_sales,
                          ntile_min_turnover, ntile_max_abs_adj,
                          townhome_group_cols, townhome_min_sales,
                          townhome_min_turnover, townhome_max_abs_adj,
                          ratio_cap_upper_bound,
                          ratio_cap_lower_bound) {

  # For every modeling group within neighborhood, calculate ntiles of estimates
  ntiles_df <- data %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(ntile_group_cols))) %>%
    dplyr::summarize(ntiles_lst = val_create_ntiles(
      x = {{ estimate }},
      probs = ntile_probs
    )) %>%
    dplyr::rowwise() %>%
    dplyr::filter(
      !is.null(.data$ntiles_lst),
      length(.data$ntiles_lst) >= length(ntile_probs) + 2,
      !any(is.na(.data$ntiles_lst))
    ) %>%
    dplyr::ungroup()

  # Assign each estimate a previously created ntile, then count the number of
  # properties in each group
  ntile_prop_counts <- data %>%
    dplyr::left_join(ntiles_df) %>%
    dplyr::mutate(ntile = val_assign_ntile(
      x = {{ estimate }},
      ntiles = .data$ntiles_lst
    )) %>%
    dplyr::group_by(dplyr::across(
      tidyselect::all_of(c(ntile_group_cols, "ntile"))
    )) %>%
    dplyr::summarize(ntile_num_props = dplyr::n())

  # Now assign an ntile by sale price in order to calculate the median
  # adjustment within each ntile. Adjustments will be 0 if there aren't enough
  # sales or the sales don't represent a large enough proportion of the group
  ntile_adjustments <- data %>%
    dplyr::left_join(ntiles_df) %>%
    dplyr::mutate(ntile = val_assign_ntile(
      x = {{ truth }},
      ntiles = .data$ntiles_lst
    )) %>%
    dplyr::group_by(dplyr::across(
      tidyselect::all_of(c(ntile_group_cols, "ntile"))
    )) %>%
    dplyr::summarize(
      ntile_num_sales = sum(!is.na({{ truth }})),
      ntile_med_pct_adj = ccao::val_med_pct_adj(
        truth = {{ truth }},
        estimate = {{ estimate }},
        min_n = ntile_min_sales,
        max_abs_adj = ntile_max_abs_adj
      ),
      ntile_med_pct_adj = tidyr::replace_na(.data$ntile_med_pct_adj, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(ntile_prop_counts) %>%
    dplyr::mutate(
      ntile_med_pct_adj = ifelse(
        .data$ntile_num_sales / .data$ntile_num_props >= ntile_min_turnover,
        .data$ntile_med_pct_adj, 0
      )
    )

  # Get the median adjusted value for townhomes, grouped by townhome_group_cols
  # This value will override ntile-based adjustments for these properties
  townhome_adjustments <- data %>%
    ccao::val_townhomes_by_group(
      truth = {{ truth }},
      estimate = {{ estimate }},
      class = {{ class }},
      townhome_group_cols = townhome_group_cols,
      townhome_min_sales = townhome_min_sales,
      townhome_min_turnover = townhome_min_turnover,
      townhome_max_abs_adj = townhome_max_abs_adj
    )

  # Output "trained" data frames and set class of object
  output <- list(
    ntiles = ntiles_df,
    ntile_adjustments = ntile_adjustments,
    ntile_group_cols = ntile_group_cols,
    ntile_probs = ntile_probs,
    ntile_min_sales = ntile_min_sales,
    ntile_min_turnover = ntile_min_turnover,
    ntile_max_abs_adj = ntile_max_abs_adj,
    townhome_adjustments = townhome_adjustments,
    townhome_group_cols = townhome_group_cols,
    townhome_min_sales = townhome_min_sales,
    townhome_min_turnover = townhome_min_turnover,
    townhome_max_abs_adj = townhome_max_abs_adj,
    ratio_cap_upper_bound = ratio_cap_upper_bound,
    ratio_cap_lower_bound = ratio_cap_lower_bound
  )
  class(output) <- "postval_model"

  return(output)
}


#' Apply post-modeling adjustments to new data
#'
#' @param object A postval_model object created by the
#'   \code{\link{postval_model}} function.
#' @param ... Not currently used.
#' @param new_data A new dataset with a structure identical to the one used to
#'   train the postval model. It must have the same \code{truth} and
#'   \code{estimate} columns as well as all the grouping columns used
#'   to make the initial model.
#' @param truth Column in data containing true market values. Typically
#'   sale prices.
#' @param estimate Column in data containing estimated market values. Typically
#'   model results.
#'
#' @return A numeric vector of final, adjusted model values.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @family valuation_funs
#' @export
predict.postval_model <- function(object, ..., new_data, truth, estimate) {
  new_data %>%
    dplyr::ungroup() %>%

    # Join ntile ranges to unseen data and assign a ntile
    dplyr::left_join(object$ntiles) %>%
    dplyr::mutate(ntile = val_assign_ntile(
      x = {{ estimate }},
      ntiles = .data$ntiles_lst
    )) %>%

    # Join percentage adjustments by neighborhood, modeling group, and ntile
    # then apply the adjustment to all properties
    dplyr::left_join(object$ntile_adjustments) %>%
    dplyr::mutate(
      {{ estimate }} := rowSums(
        data.frame({{ estimate }}, {{ estimate }} * .data$ntile_med_pct_adj),
        na.rm = TRUE
      ),
      {{ estimate }} := ccao::val_limit_ratios(
        truth = {{ truth }},
        estimate = {{ estimate }},
        lower = object$ratio_cap_lower_bound,
        upper = object$ratio_cap_upper_bound
      )
    ) %>%

    # Override townhomes with their own median adjusted estimated value from
    # within the same set of properties
    dplyr::left_join(object$townhome_adjustments) %>%
    dplyr::mutate(
      {{ estimate }} := ifelse(
        !is.na(.data$th_final_value),
        .data$th_final_value,
        {{ estimate }}
      )
    ) %>%

    # Return the final estimated value
    dplyr::pull({{ estimate }})
}
