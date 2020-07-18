#' Check validity of property class codes in a dataset based on age and size.
#'
#' @description Check property characteristics against class definitions as defined # nolint
#' \href{https://datascience.cookcountyassessor.com/wiki/data/class-definitions.pdf}{here}. # nolint
#'
#' @param age Integer or numeric vector of ages of properties. Either 1 long
#'   or the same length as \code{sqft} and \code{class}.
#' @param sqft Integer or numeric vector of the square footage of properties.
#'   Either 1 long or the same length as \code{age} and \code{class}.
#' @param class String or character vector of class codes. Either 1 long or
#'   the same length as \code{sqft} and \code{age}.
#'
#' @return A logical vector indicating that the specified class falls within
#'   the parameters specified by \code{\link{class_dict}}. Throws error if input
#'   data types are incorrect or if length conditions of input vectors
#'   are not met.
#'
#' @examples
#' vars_check_class(50, 800, "202")
#' vars_check_class(c(50, 80), c(800, 1000), c("202", "203"))
#' vars_check_class(c(50, 80), 1000, "210")
#' vars_check_class(50, c(800, 2000), "202")
#' vars_check_class(50, 1000, c("202", "203"))
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @family vars_funs
#' @export
vars_check_class <- function(age, sqft, class) {

  # Simple error checking
  stopifnot(
    is.numeric(age),
    is.numeric(sqft),
    is.character(class)
  )

  # Take only the classes from the dictionary which are residential (200)
  res_classes <- dplyr::filter(
    ccao::class_dict, substr(.data$class_code, 1, 1) == "2"
  )

  # Element-wise comparison to test that age & sqft return the expected class
  mapply(function(x, y, z) {
    idx <- res_classes$min_age <= x &
      res_classes$max_age >= x &
      res_classes$min_size <= y &
      res_classes$max_size >= y

    possible_classes <- res_classes$class_code[idx]

    if (length(possible_classes) == 0) {
      return(NA)
    } else {
      return(z %in% possible_classes)
    }
  },
  x = age, y = sqft, z = class,
  USE.NAMES = FALSE,
  SIMPLIFY = TRUE
  )
}


#' Translate variable names from different CCAO data sources
#'
#' @description Bulk rename columns from one type of CCAO to another. For
#' example, rename all columns pulled from SQL to their standard names used
#' in modeling. Or, rename all standard modeling names to "pretty" names for
#' publication. Function will overwrite names it finds in
#' \code{\link{vars_dict}}, all other names in the data will remain unchanged.
#'
#' Options for \code{names_from} and \code{names_to} are: \code{"sql"} (
#' with names like TAX_YEAR, GAR1_SIZE); \code{"addchars"} (with names like
#' QU_AGE, QU_GARAGE_SIZE); \code{"standard"} (with names like meta_tax_year,
#' meta_gar1_size); and \code{"pretty"} (with names like Year, Garage 1 Size)
#'
#' @param data A data frame or tibble with columns to be renamed.
#' @param names_from The source/name type of data. See description
#' @param names_to The target names. See description
#'
#' @return The input data frame with columns renamed.
#'
#' @examples
#'
#' # Rename column names from SQL
#' vars_rename(chars_sample_universe)
#' vars_rename(chars_sample_universe, names_to = "pretty")
#'
#' # No renames will occur since no column names here are from SQL
#' vars_rename(class_dict)
#' @family vars_funs
#' @export
vars_rename <- function(data, names_from = "sql", names_to = "standard") {

  # Stop if input is not a data frame or if name targets are not within
  # the preset types
  stopifnot(
    is.data.frame(data),
    tolower(names_from) %in% c("sql", "addchars", "standard", "pretty"),
    tolower(names_to) %in% c("sql", "addchars", "standard", "pretty")
  )

  from <- paste0("var_name_", names_from)
  to <- paste0("var_name_", names_to)

  # Rename, replacing any NAs with the original column names
  names_wm <- ccao::vars_dict[[to]][match(names(data), ccao::vars_dict[[from]])]
  names_wm[is.na(names_wm)] <- names(data)[is.na(names_wm)]

  names(data) <- names_wm

  return(data)
}



#' Replace numerically coded variables with human-readable values
#'
#' @description The AS/400 stores characteristic values in a numerically encoded
#' format. This function can be used to translate those values into a
#' human-readable format. For example, EXT_WALL = 2 will become
#' EXT_WALL = "Frame + Masonry". Note that the values and their translations are
#' specified in \code{\link{vars_dict}}.
#'
#' @param data A data frame or tibble with columns to have values replaced.
#' @param cols A \code{<tidy-select>} column select or vector of column names.
#'   Looks for all columns with numerically encoded character values by default.
#' @param type Output/recode type. Options are: \code{"long"}, which transforms
#'   EXT_WALL = 1 to EXT_WALL = Frame; \code{"short"}, for EXT_WALL = FRME; and
#'   \code{"code"}, which keeps the original values (useful for removing
#'   improperly coded values, see note below).
#'
#' @note Values which are in the data but are NOT in \code{\link{vars_dict}}
#'   will be converted to NA. For example, there is no numeric value 3 for AIR,
#'   so it will become NA.
#'
#' @return The input data frame with re-encoded values for the specified
#'   columns.
#'
#' @examples
#'
#' # Recode all char columns
#' vars_recode(chars_sample_universe)
#' vars_recode(chars_sample_universe, type = "short")
#'
#' # Recode only the specified columns
#' vars_recode(chars_sample_universe, cols = dplyr::starts_with("GAR"))
#' vars_recode(chars_sample_universe, cols = "GAR1_SIZE")
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @family vars_funs
#' @export
vars_recode <- function(data, cols = dplyr::everything(), type = "long") {

  # Error/input checking
  stopifnot(
    is.data.frame(data),
    type %in% c("code", "short", "long")
  )

  # Translate inputs to column names
  var <- switch(
    type,
    "code" = "var_code",
    "long" = "var_value",
    "short" = "var_value_short"
  )

  # Convert chars dict into long format that can be easily referenced use
  # any possible input column names
  dict <- ccao::vars_dict %>%
    dplyr::filter(
      .data$var_type == "char" & .data$var_data_type == "categorical"
    ) %>%
    dplyr::select(
      .data$var_name_sql:.data$var_name_pretty,
      .data$var_code:.data$var_value_short
    ) %>%
    tidyr::pivot_longer(
      .data$var_name_sql:.data$var_name_pretty,
      names_to = "var_type",
      values_to = "var_name"
    )

  # For each column listed in the input, check if it's a character column
  # If it is, make a lookup table using dict and find the equivalent value
  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::all_of(cols),
      function(x, y = dplyr::cur_column()) {
        if (y %in% dict$var_name) {

          # Find the rows of the dictionary corresponding to column y
          var_rows <- which(dict$var_name == y)
          idx <- match(x, dict$var_code[var_rows])

          return(dict[[var]][var_rows][idx])
        } else {
          return(x)
        }
      }
    )
  )
}
