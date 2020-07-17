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
#'   the parameters specified by \code{class_dict}. Throws error if input data
#'   types are incorrect or if length conditions of input vectors are not met.
#'
#' @examples
#' vars_check_class(50, 800, "202")
#' vars_check_class(c(50, 80), c(800, 1000), c("202", "203"))
#' vars_check_class(c(50, 80), 1000, "210")
#' vars_check_class(50, c(800, 2000), "202")
#' vars_check_class(50, 1000, c("202", "203"))
#' @importFrom magrittr %>%
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
  class_code <- NULL
  res_classes <- ccao::class_dict %>%
    dplyr::filter(substr(class_code, 1, 1) == "2")

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
#' Bulk rename columns from one type of CCAO to another. For example, rename
#' all columns pulled from SQL to their standard names used in modeling. Or,
#' rename all standard modeling names to "pretty" names for publication.
#' Function will overwrite names it finds in \code{vars_dict}, all other names
#' in the data will remain unchanged.
#'
#' Options for \code{names_from} and \code{names_to} are: \code{"sql"} (
#' with names like TAX_YEAR, GAR1_SIZE); \code{"addchars"} (with names like
#' QU_AGE, QU_GARAGE_SIZE); \code{"standard"} (with names like meta_tax_year,
#' meta_gar1_size); and \code{"pretty"} (with names like Year, Garage 1 Size)
#'
#' @param data A data frame or tibble with columns to be renamed.
#' @param names_from The source/name type of data. See note.
#' @param names_to The target names. See note.
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
