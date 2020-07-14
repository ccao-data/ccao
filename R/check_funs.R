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
#' check_class(50, 800, "202")
#' check_class(c(50, 80), c(800, 1000), c("202", "203"))
#' check_class(c(50, 80), 1000, "210")
#' check_class(50, c(800, 2000), "202")
#' check_class(50, 1000, c("202", "203"))
#' @importFrom magrittr %>%
#' @family check_funs
#' @export
check_class <- function(age, sqft, class) {

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
