#' Check if a property class falls within its expected square footage and age
#' boundaries
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


#' Bulk rename variables from CCAO SQL to standardized or pretty names
#' and visa versa
#'
#' @description Bulk rename columns from one type of CCAO data to another. For
#' example, rename all columns pulled from SQL to their standard names used
#' in modeling. Or, rename all standard modeling names to "pretty" names for
#' publication. This function will only rename things specified in
#' the user-supplied \code{dict} argument, all other names in the data will
#' remain unchanged.
#'
#' Options for \code{names_from} and \code{names_to} are specific to the
#' specified \code{dict}. Run this function with \code{names_from} equal to
#' \code{NULL} to see a list of available options for the specified dictionary.
#'
#' @param data A data frame or tibble with columns to be renamed.
#' @param names_from The source/name type of data. See description
#' @param names_to The target names. See description
#' @param type Output type. Either \code{"inplace"}, which renames the input
#'   data frame, or \code{"vector"}, which returns a named character vector with
#'   the construction new_col_name = old_col_name.
#' @param dict The dictionary used to translate names. Uses
#'   \code{\link{vars_dict}} by default. Use \code{\link{vars_dict_legacy}} for
#'   legacy data column names.
#'
#' @return The input data frame with columns renamed.
#'
#' @examples
#'
#' # Rename column names from SQL
#' sample_data <- chars_sample_universe[1:5, 18:27]
#' sample_data
#' 
#' vars_rename(
#'   data = sample_data,
#'   names_from = "sql",
#'   names_to = "standard",
#'   dict = ccao::vars_dict_legacy
#' )
#' vars_rename(
#'   data = sample_data,
#'   names_from = "sql",
#'   names_to = "pretty",
#'   dict = ccao::vars_dict_legacy
#' )
#' vars_rename(
#'   data = sample_data_athena,
#'   names_from = "athena",
#'   names_to = "pretty",
#'   dict = ccao::vars_dict
#' )
#'
#' # No renames will occur since no column names here are from SQL
#' vars_rename(
#'   data = class_dict[1:5, 1:5],
#'   names_from = "sql",
#'   names_to = "pretty",
#'   dict = ccao::vars_dict_legacy
#' )
#' 
#' # With data from Athena
#' sample_data_athena <- chars_sample_athena[1:5, 1:10]
#' sample_data_athena
#' 
#' vars_rename(
#'   data = sample_data_athena,
#'   names_from = "athena",
#'   names_to = "model",
#'   dict = ccao::vars_dict
#' )
#' vars_rename(
#'   data = sample_data_athena,
#'   names_from = "athena",
#'   names_to = "pretty",
#'   dict = ccao::vars_dict
#' )
#' @md
#' @family vars_funs
#' @export
vars_rename <- function(data,
                        names_from = NULL,
                        names_to = NULL,
                        type = "inplace",
                        dict = ccao::vars_dict
                        ) {

  # Check input data dictionary
  stopifnot(
    is.data.frame(dict),
    sum(startsWith(names(dict), "var_name_")) >= 2,
    nrow(dict) > 0
  )

  # Get vector of possible inputs to names_from and names_to from dictionary
  poss_names_args <- gsub(
    "var_name_", "",
    names(dict)[startsWith(names(dict), "var_name_")]
  )

  # If args aren't in possible, throw error and list possible args
  if (is.null(names_from)) {
    stop("names_from must be one of: ", paste(poss_names_args, collapse = ", "))
  } else if (!names_from %in% poss_names_args) {
    stop("names_from must be one of: ", paste(poss_names_args, collapse = ", "))
  }

  if (is.null(names_to)) {
    stop("names_to must be one of: ", paste(poss_names_args, collapse = ", "))
  } else if (!names_to %in% poss_names_args) {
    stop("names_to must be one of: ", paste(poss_names_args, collapse = ", "))
  }

  # Stop if input is not a data frame of character vector or if name targets are
  # not within the preset types
  stopifnot(
    is.data.frame(data) | is.character(data),
    tolower(names_from) %in% poss_names_args,
    tolower(names_to) %in% poss_names_args,
    tolower(type) %in% c("inplace", "vector")
  )

  # If the input is a dataframe, extract the names from that dataframe
  if (is.data.frame(data)) names_lst <- names(data) else names_lst <- data

  from <- paste0("var_name_", names_from)
  to <- paste0("var_name_", names_to)

  # Rename using dict, replacing any NAs with the original column names
  names_wm <- dict[[to]][match(names_lst, dict[[from]])]
  names_wm[is.na(names_wm)] <- names_lst[is.na(names_wm)]

  # Return names inplace if the input data is a data frame, else return a
  # character vector of new names
  if (is.data.frame(data) & type == "inplace") {
    names(data) <- names_wm
    return(data)
  } else if (is.character(data) | type == "vector") {
    return(names_wm)
  }
}


#' Replace numerically coded variables with human-readable values
#'
#' @description The system of record stores characteristic values in a
#' numerically encoded format. This function can be used to translate those
#' values into a human-readable format. For example, EXT_WALL = 2 will become
#' EXT_WALL = "Frame + Masonry". Note that the values and their translations are
#' must be specified via a user-defined dictionary. The default dictionary is
#' \code{\link{vars_dict}}.
#'
#' Options for \code{type} are:
#'
#' - \code{"long"}, which transforms EXT_WALL = 1 to EXT_WALL = Frame
#' - \code{"short"}, which transforms EXT_WALL = 1 to EXT_WALL = FRME
#' - \code{"code"}, which keeps the original values (useful for removing
#'   improperly coded values, see note below)
#'
#' @param data A data frame or tibble with columns to have values replaced.
#' @param cols A \code{<tidy-select>} column selection or vector of column
#'   names. Looks for all columns with numerically encoded character
#'   values by default.
#' @param type Output/recode type. See description for options.
#' @param as_factor If \code{TRUE}, re-encoded values will be returned as
#'   factors with their levels pre-specified by the dictionary. Otherwise, will
#'   return re-encoded values as characters only.
#' @param dict The dictionary used to translate encodings. Uses
#'   \code{\link{vars_dict}} by default. Use \code{\link{vars_dict_legacy}} for
#'   legacy data column encodings.
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
#' sample_data <- chars_sample_universe[1:5, 18:27]
#'
#' sample_data
#' vars_recode(
#'   data = sample_data,
#'   dict = ccao::vars_dict_legacy
#' )
#' vars_recode(
#'   data= sample_data,
#'   type = "short",
#'   dict = ccao::vars_dict_legacy
#' )
#'
#' # Recode only the specified columns
#' gar_sample <- chars_sample_universe[1:5, 30:40]
#'
#' gar_sample
#' vars_recode(
#'   data = gar_sample,
#'   cols = dplyr::starts_with("GAR"),
#'   dict = ccao::vars_dict_legacy
#' )
#' vars_recode(
#'   data = gar_sample,
#'   cols = "GAR1_SIZE",
#'   dict = ccao::vars_dict_legacy
#' )
#' 
#' # Using data from Athena
#' sample_data_athena <- chars_sample_athena[1:5, c(1:5, 10:20)]
#' sample_data_athena
#' vars_recode(
#'   data = sample_data_athena,
#'   type = "code",
#'   dict = ccao::vars_dict_legacy
#' )
#' vars_recode(
#'   data = sample_data_athena,
#'   type = "long",
#'   dict = ccao::vars_dict_legacy
#' )
#' @md
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @family vars_funs
#' @export
vars_recode <- function(data,
                        cols = dplyr::everything(),
                        type = "long",
                        as_factor = TRUE,
                        dict = ccao::vars_dict
                        ) {

  # Check input data dictionary
  stopifnot(
    is.data.frame(dict),
    sum(startsWith(names(dict), "var_name_")) >= 1,
    nrow(dict) > 0
  )

  # Check that the dictionary contains the correct columns
  if (!any(c("var_code", "var_value", "var_value_short") %in% names(dict))) {
    stop(
      "Input dictionary must contain the following columns: ",
      "var_code, var_value, var_value_short"
    )
  }

  # Error/input checking
  stopifnot(
    is.data.frame(data),
    type %in% c("code", "short", "long"),
    is.logical(as_factor)
  )

  # Translate inputs to column names
  var <- switch(type,
    "code" = "var_code",
    "long" = "var_value",
    "short" = "var_value_short"
  )

  # Convert chars dict into long format that can be easily referenced use
  # any possible input column names
  dict_long <- dict %>%
    dplyr::filter(
      .data$var_type == "char" & .data$var_data_type == "categorical"
    ) %>%
    dplyr::select(
      dplyr::starts_with("var_name_"),
      .data$var_code:.data$var_value_short
    ) %>%
    tidyr::pivot_longer(
      dplyr::starts_with("var_name_"),
      names_to = "var_type",
      values_to = "var_name"
    ) %>%
    dplyr::distinct(var_code, var_value, var_value_short, var_name)

  # For each column listed in the input, check if it's a character column
  # If it is, make a lookup table using dict_long and find the equivalent value
  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::all_of(cols),
      function(x, y = dplyr::cur_column()) {
        if (y %in% dict_long$var_name) {

          # Find the rows of the dictionary corresponding to column y
          var_rows <- which(dict_long$var_name == y)
          idx <- match(x, dict_long$var_code[var_rows])

          if (as_factor) {
            out <- factor(
              dict_long[[var]][var_rows][idx],
              levels = dict_long[[var]][var_rows]
            )
          } else {
            out <- dict_long[[var]][var_rows][idx]
          }
          return(out)
        } else {
          return(x)
        }
      }
    )
  )
}
