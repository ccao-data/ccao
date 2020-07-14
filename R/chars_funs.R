#' Determine active years for a Home Improvement Exemption (288)
#'
#' @description The State of Illinois has a home improvement exemption program
#' which allows property owners to deduct up to $75,000 per year of any value
#' created by improvements to a residential property.
#'
#' This has the effect of essentially "freezing" a home's characteristics at
#' whatever they were prior to the start of the improvement project. For
#' example, if a property owner adds an additional bedroom and applies for a
#' 288, the property will be valued as if the new bedroom does not exist until
#' the 288 expires and as long as the increase in valuation is less than
#' $75,000.
#'
#' \href{https://www.ilga.gov/legislation/ilcs/fulltext.asp?DocName=003502000K15-180}{Per Illinois statute}, # nolint
#' 288s expire after 4 years or until the next assessment cycle, whichever is
#' longer. For example, a 288 received in 2016 for a property in Northfield
#' (with assessment years 2016, 2019, and 2022) will last 6 years (until 2021,
#' the year before the 2022 reassessment).
#'
#' This function calculates the years a 288 will be active given a start year
#' and the township of a property.
#'
#' @param start_year A numeric vector of start years. Must be either 1 long or
#'   the same length as \code{town}.
#' @param town A character vector of town codes or names. Must be either 1 long
#'   or the same length as \code{start_year}.
#'
#' @return A list of numeric vectors containing the active years for each value
#'   of the input vectors. The list will always be N long, where N is the
#'   length of the longest input vector.
#'
#' @examples
#'
#' # Simplest case to get the length for one property
#' chars_288_active(2016, "Northfield")
#'
#' # Multiple years for a single township
#' chars_288_active(c(2016, 2010, 2020), "77")
#' chars_288_active(c(2016, 2010, 2020), "Evanston")
#'
#' # Multiple townships for a single year
#' chars_288_active(2015, c("77", "Evanston", "10"))
#'
#' # Pairwise lookup (2015 for Evanston, 2017 for Northfield)
#' chars_288_active(c(2015, 2017), c("Evanston", "Northfield"))
#' @family chars_funs
#' @export
chars_288_active <- function(start_year, town) {

  # Input checking and error handling
  stopifnot(
    is.numeric(start_year), # Input years must be numeric
    is.character(town), # Input vector must be char.
    xor(
      length(start_year) == length(town) & length(start_year) != 1,
      (length(start_year) == 1 | length(town) == 1)
    ) # Input vectors must be same len OR one of them must be len == 1
  )

  # Loop through inputs and calculate which years between start_year and
  # start_year + 8 will be active based on the town's reassessment cycle
  mapply(
    function(x, y) {
      if (is.na(x)) {
        return(NA_real_)
      }
      else if (is.na(y)) {
        return(NA_character_)
      }
      else {
        potentially_active_years <- x:(x + 8)

        idx <- potentially_active_years >= x &
          potentially_active_years < max(
            x + 4,
            ccao::town_get_assmnt_year(y, x + 4, round_type = "ceiling")
          )

        return(potentially_active_years[idx])
      }
    },
    x = start_year,
    y = town,
    SIMPLIFY = FALSE
  )
}


#' Sparsify an ADDCHARS data frame
#'
#' @description The ADDCHARS SQL table includes individual rows listing the PIN,
#' start date, and characteristic updates associated with a 288 Home Improvement
#' Exemption. This data format is difficult to work with and complicated by the
#' fact that multiple 288s can be active at the same time for different periods,
#' and some columns from ADDCHARS add to existing characteristics, while some
#' overwrite existing characteristics. Additionally, 288s can be active for
#' multi-code properties, meaning that one building (out of N) could have an
#' improvement while the others remain untouched.
#'
#' The goal of this function is to transform these single rows into a sparse
#' data frame which lists a row and characteristic update per PIN per YEAR
#' per CLASS. CLASS is needed so that if multiple buildings lie on the same
#' PIN, they can be joined by PIN, YEAR, and CLASS.
#'
#' The transformation caused by this function is most easily visualized
#' with a mock dataset.
#'
#' The base ADDCHARS data looks like this:
#'
#' | QU_PIN     | TAX_YEAR | QU_TOWN | QU_UPLOAD_DATE | QU_SQFT_BLD | QU_ROOMS |
#' |------------|----------|---------|----------------|-------------|----------|
#' | 12345      | 2013     | 77      | 130702         | 200         | 0        |
#' | 12345      | 2015     | 77      | 150703         | 300         | 1        |
#'
#' This function will transform it into:
#'
#' | QU_PIN | YEAR | QU_SQFT_BLD | QU_ROOMS |
#' |--------|------|-------------|----------|
#' | 12345  | 2013 | 200         | 0        |
#' | 12345  | 2014 | 200         | 0        |
#' | 12345  | 2015 | 500         | 1        |
#' | 12345  | 2016 | 500         | 1        |
#' | 12345  | 2017 | 500         | 1        |
#' | 12345  | 2018 | 300         | 1        |
#' | 12345  | 2019 | 300         | 1        |
#' | 12345  | 2020 | 300         | 1        |
#'
#' Each PIN will have a row for each year that the 288 is active and a column
#' for each characteristic specified by \code{additive_source} and
#' \code{replacement_source}. This table can be easily left joined to modeling
#' data to update characteristic values. See README.Rmd for examples.
#'
#' @param data A data frame containing ADDCHARS columns.
#' @param pin_col A column name specifying the PIN column. Usually QU_PIN.
#' @param year_col A column name specifying the year column. Usually TAX_YEAR.
#' @param class_col A column name specifying the class column. Usually QU_CLASS.
#' @param town_col A column name specifying the town column. Usually QU_TOWN.
#'   This is used to determine the length of the 288, given the starting date
#'   specified by \code{year_col}.
#' @param upload_date_col A column name specifying the upload date of the 288,
#'   this is use to determine which 288 take precedence if multiple 288s were
#'   filed in the same year (the latest one is chosen). Usually QU_UPLOAD_DATE.
#' @param additive_source A tidyselect selection of columns which contains
#'   additive characteristic values. These are values such as square feet which
#'   get ADDED to existing characteristics. For example, if QU_SQFT_BLD is equal
#'   to 100, then 100 sqft get added to the existing square footage of the
#'   property. The easiest way to specify all additive columns is to use the
#'   built-in crosswalk,
#'   \code{any_of(ccao::chars_cols$add_source)}.
#' @param replacement_source A tidyselect selection of columns which contains
#'   replacement characteristic values. These are values such as number of rooms
#'   which get OVERWRITE existing characteristics. For example, if QU_ROOMS is
#'   equal to 3, then the property will be update to have 3 total rooms.
#'   The easiest way to specify all replacement columns is to use the
#'   built-in crosswalk,
#'   \code{any_of(ccao::chars_cols$rep_source)}.
#'
#' @note Use dplyr/tidyselect syntax for specifying column names. For example,
#'   use \code{QU_PIN} instead of \code{"QU_PIN"}, or use a tidyselect function
#'   such as \code{starts_with("QU_")}.
#'
#' @return A sparsified data frame of characteristic updates per PIN per year.
#' See above for example.
#'
#' @examples
#'
#' # Load the included example dataset
#' data("chars_sample_addchars")
#'
#' chars_sparsify(
#'   chars_sample_addchars,
#'   pin_col = QU_PIN,
#'   year_col = TAX_YEAR,
#'   class_col = QU_CLASS,
#'   town_col = as.character(QU_TOWN),
#'   upload_date_col = QU_UPLOAD_DATE,
#'   additive_source = any_of(chars_cols$add_source),
#'   replacement_source = any_of(chars_cols$rep_source)
#' )
#' @md
#' @importFrom magrittr %>%
#' @family chars_funs
#' @export
chars_sparsify <- function(data, pin_col, year_col, class_col, town_col,
                           upload_date_col, additive_source, replacement_source) { # nolint
  has_active_288 <- NULL
  town <- NULL

  # Get the last nonzero element of a vector. Used to get the latest
  # "replacement" characteristic for each 288
  last_nonzero_element <- function(x) {
    idx <- x != 0
    ifelse(any(idx), x[max(which((idx)))], 0)
  }

  # Goal here is to transform our data from single rows specifying 288s that
  # are active for X years to a dataset of X rows with the characteristic
  # updates for each year. To do this, we first eliminate multiple 288s active
  # on the same PIN, YEAR, and CLASS (say someone made 2 improvements in 1 year)
  # by summing the additive characteristics and taking the latest replacement
  # ones
  data %>%
    dplyr::group_by({{ pin_col }}, {{ year_col }}, {{ class_col }}) %>%
    dplyr::arrange(
      {{ pin_col }},
      {{ year_col }},
      {{ class_col }},
      {{ upload_date_col }}
    ) %>%
    dplyr::summarize(
      town = dplyr::first({{ town_col }}),
      dplyr::across({{ additive_source }}, sum),
      dplyr::across({{ replacement_source }}, last_nonzero_element)
    ) %>%

    # Next, for each single 288 row, we determine the years it will be active
    # by taking the start year and town. This list gets put into a new variable
    # and then expanded using unnest(), which creates a duplicate row for each
    # value of the list
    dplyr::mutate(has_active_288 = ccao::chars_288_active(
      {{ year_col }},
      as.character(town)
    )) %>%
    tidyr::unnest(has_active_288) %>%

    # The number of rows in our dataset is now equal to # of unique PINS * # of
    # classes per PIN * # of years active for each PIN/class combination
    # Our final step is to merge the 288 characteristic updates by PIN and class
    # For each year a PIN/class combo has active 288s, we sum the additive chars
    # and take the last nonzero value for replacement
    dplyr::group_by({{ pin_col }}, has_active_288, {{ class_col }}) %>%
    dplyr::arrange(
      {{ pin_col }},
      has_active_288,
      {{ class_col }},
      {{ year_col }}
    ) %>%
    dplyr::summarize(
      dplyr::across({{ additive_source }}, sum),
      dplyr::across({{ replacement_source }}, last_nonzero_element),
      NUM_288S_ACTIVE = dplyr::n()
    ) %>%
    dplyr::rename(YEAR = has_active_288)
}


# Unexported utility function to lookup equivalent ADDCHARS column given
# a CCAOSFCHARS column
chars_get_col <- function(col) {
  source_cols <- c(ccao::chars_cols$add_source, ccao::chars_cols$rep_source)
  target_cols <- c(ccao::chars_cols$add_target, ccao::chars_cols$rep_target)
  source_cols[which(target_cols %in% col)]
}


#' Update characteristic values using values from ADDCHARS
#'
#' @description Function used to update the characteristic values of a
#' data frame containing both the original characteristic value (CCAOSFCHARS)
#' and the value to add or replace it with (ADDCHARS). This function expect a
#' data frame formatted using \code{chars_sparsify}.
#'
#' @param data A data frame containing CCAOSFCHARS columns AND their equivalent
#'   ADDCHARS columns.
#' @param additive_target A tidyselect selection of CCAOSFCHARS column names
#'   that should have characteristics added to them from their equivalent
#'   ADDCHARS column, which must also be present.
#' @param replacement_target A tidyselect selection of CCAOSFCHARS column names
#'   that should have characteristics replaced using their equivalent ADDCHARS
#'   column, which must also be present.
#'
#' @return A data frame with updated additive and replacement target columns.
#'
#' @importFrom magrittr %>%
#' @family chars_funs
#' @export
chars_update <- function(data, additive_target, replacement_target) {

  # Given an input dataset, this code will lookup each of the CCAOSFCHARS
  # columns specified and ADD or REPLACE their values using the equivalent
  # ADDCHARS column
  data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(
        {{ additive_target }},
        function(x, y = dplyr::cur_column()) {
          sum(x, get(chars_get_col(y)), na.rm = T)
        }
      ),
      dplyr::across(
        {{ replacement_target }},
        function(x, y = dplyr::cur_column()) {
          source_col <- get(chars_get_col(y))
          idx <- !is.na(source_col) & source_col != 0
          replace(x, idx, source_col)
        }
      )
    )
}
