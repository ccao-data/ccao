#' Clean or alter CCAO data using standardized recipes
#'
#' @description Data extracted from CCAO SQL tables often requires cleaning and
#'   transformation before modeling. These functions standardized such actions
#'   into "recipes", similar to the R recipes package. They should be applied
#'   in the order listed below.
#'
#' @param data A data frame containing data extracted from CCAO SQL tables.
#'   Expects either SQL or "standardized" column names as listed in
#'   \code{\link{vars_dict}}.
#' @param cols_to_rm Additional columns to remove when discarding all
#'   columns not present in \code{\link{vars_dict}}.
#' @param names_to Set of names to use when renaming variables. Options are
#'   listed in \code{\link{vars_rename}}.
#'
#' @return A data frame after the recipes steps are applied.
#'
#' @md
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @family recp_funs
#' @name recp_funs
NULL


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Cleaning Recipes #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#' @describeIn recp_funs Remove all variables not present in
#'   \code{\link{vars_dict}} and any additional variables listed in
#'   \code{cols_to_rm}.
#'
#' @family recp_funs
#' @export
recp_clean_keep_dict_vars <- function(data, cols_to_rm) {

  # Get all possible column names from the variable dictionary
  cols_to_keep <- ccao::vars_dict %>%
    dplyr::filter(.data$var_is_published) %>%
    tidyr::pivot_longer(tidyr::starts_with("var_name")) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::pull(.data$value)

  # Find columns not included in the dictionary
  add_cols_to_rm <- names(data)[!names(data) %in% cols_to_keep]

  # Remove any vars not in the dictionary OR listed in cols_to_rm
  data %>%
    dplyr::select(-dplyr::any_of(c(add_cols_to_rm, cols_to_rm)))
}


#' @describeIn recp_funs Rename variable names from SQL to the "standard" names
#'   contained in \code{\link{vars_dict}}.
#'
#' @family recp_funs
#' @export
recp_clean_rename <- function(data, names_to = "standard") {
  data %>%
    dplyr::rename_at(
      dplyr::vars(tidyr::starts_with("DT_")),
      ~ gsub("DT_", "", .x, fixed = TRUE)
    ) %>%
    ccao::vars_rename(names_to = names_to)
}


#' @describeIn recp_funs Recode variables to their expected types and clean up
#'   other variables like date and age.
#'
#' @family recp_funs
#' @export
recp_clean_recode <- function(data) {

  # Use the vars dictionary to create three vectors of column names, one
  # vector for each type of variable
  dict <- ccao::vars_dict %>%
    tidyr::pivot_longer(tidyr::starts_with("var_name")) %>%
    dplyr::filter(!is.na(.data$value))

  cols_numeric <- dict %>%
    dplyr::filter(.data$var_data_type == "numeric") %>%
    dplyr::pull(.data$value)

  cols_character <- dict %>%
    dplyr::filter(.data$var_data_type == "character") %>%
    dplyr::pull(.data$value)

  cols_factor <- dict %>%
    dplyr::filter(.data$var_data_type == "categorical") %>%
    dplyr::pull(.data$value)

  # Ensure dates are parsed correctly
  data %>%
    dplyr::mutate(dplyr::across(
      tidyr::contains("_date"),
      ~ lubridate::as_date(.x, tz = "America/Chicago")
    )) %>%

    # Convert vars to their expected types
    dplyr::mutate(dplyr::across(
      dplyr::any_of(cols_numeric),
      ~ as.numeric(.x)
    )) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(cols_factor),
      ~ as.factor(.x)
    )) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(cols_character),
      ~ as.character(.x)
    )) %>%

    # Fix/smooth the AGE variable so that it reflects true AGE (the CCAO age var
    # only updates after each assessment cycle, so it jumps by 3)
    dplyr::mutate(char_age = ccao::chars_fix_age(
      .data$char_age, .data$meta_year, .data$meta_town_code
    ))
}


#' @describeIn recp_funs Reorder variables into consistent groups by
#'   standardized name (meta, char, etc.).
#'
#' @family recp_funs
#' @export
recp_clean_relocate <- function(data) {
  data %>%
    dplyr::relocate(tidyr::starts_with("ind_")) %>%
    dplyr::relocate(tidyr::starts_with("time_")) %>%
    dplyr::relocate(tidyr::starts_with("econ_")) %>%
    dplyr::relocate(tidyr::starts_with("geo_")) %>%
    dplyr::relocate(tidyr::starts_with("char_")) %>%
    dplyr::relocate(tidyr::starts_with("meta_"))
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Feature Engineering Recipes #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#' @describeIn recp_funs Create time fixed effect variables for sales using
#'   1997 as a base year.
#'
#' @family recp_funs
#' @export
recp_feat_time <- function(data, origin_date = "1997-01-01") {
  data %>%
    dplyr::mutate(
      # Calculate interval periods and times since Jan 01, 1997
      time_interval = lubridate::interval(
        lubridate::ymd(origin_date),
        lubridate::ymd(.data$meta_sale_date)
      ),
      time_sale_year = lubridate::year(.data$meta_sale_date),
      time_sale_quarter = (time_interval %/% months(1)) %/% 3,
      time_sale_month = time_interval %/% months(1),
      time_sale_week = time_interval %/% weeks(1),
      time_sale_day = time_interval %/% days(1),

      # Get individual components of dates for fixed effects to correct
      # seasonality
      time_sale_quarter_of_year = paste0("Q", as.character(
        lubridate::quarter(.data$meta_sale_date)
      )),
      time_sale_month_of_year = lubridate::month(
        x = .data$meta_sale_date,
        label = TRUE,
        abbr = TRUE
      ),
      time_sale_week_of_year = lubridate::week(.data$meta_sale_date),
      time_sale_day_of_year = lubridate::yday(.data$meta_sale_date),

      # Create indicators for dates that fall in particular months
      time_sale_during_school_year = lubridate::month(
        .data$meta_sale_date) %in% c(1:5, 9:12),
      time_sale_during_holidays = lubridate::month(
        .data$meta_sale_date) %in% c(11, 12, 1)
    ) %>%
    dplyr::select(-time_interval)
}


#' @describeIn recp_funs Create indicator variables on characteristics for
#'  use in modeling.
#'
#' @family recp_funs
#' @export
recp_feat_char_inds <- function(data) {
  predictors_unneeded <- c("char_apts", "char_porch", "char_attic_fnsh")
  predictors <- ccao::vars_dict %>%
    dplyr::filter(
      .data$var_is_clustered,
      !.data$var_name_standard %in% predictors_unneeded
    ) %>%
    dplyr::pull(.data$var_name_standard) %>%
    unique()

  df <- data %>%
    # Create indicator variable for properties that are especially large
    # Create another indicator for homes with potentially erroneous classes
    dplyr::mutate(
      ind_large_home = .data$meta_class %in% c("208", "209"),
      ind_class_error = !ccao::vars_check_class(
        .data$char_age,
        .data$char_bldg_sf,
        .data$meta_class
      ),
      ind_multi_code = as.logical(.data$ind_multi_code),
      ind_garage = .data$char_gar1_size != "7"
    )

  df %>%
    # Indicator for complete cases of regression variables
    dplyr::mutate(
      ind_complete_predictors =
        !rowSums(cbind(sapply(
          dplyr::select(df, dplyr::any_of(predictors)), is.na
        ))) > 0
    )
}


#' @describeIn recp_funs Represents user defined exclusions due to data issues.
#' Observations with ind_arms_length = 1 have "pure market" qualities and
#' good data.
#'
#' @family recp_funs
#' @export
recp_feat_arms_length <- function(data) {
  data %>%
   dplyr::mutate(
      ind_arms_length =
        .data$meta_modeling_group != "NCHARS"
        & dplyr::between(.data$meta_sale_price, 1e4, 1e7)
        & .data$char_bldg_sf > 100 # no super small buildings
        & .data$char_hd_sf > 0
        & is.finite(.data$char_bldg_sf)
        & !.data$ind_class_error # no properties with wrong classes
        & !is.na(.data$char_bldg_sf)
        & !is.na(.data$char_hd_sf)
        & !is.na(.data$char_ext_wall)
        & dplyr::between(.data$char_rooms, 0, 40)
        & dplyr::between(.data$char_beds, 0, 18)
    )
}
