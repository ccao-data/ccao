#' Fix the age variable in CCAO data
#'
#' @description The AGE variable in many CCAO datasets only updates when a
#' property is reassessed. This function will calculate the correct age of
#' a property given a township, current year, and age. It can be used with
#' \code{\link[dplyr]{mutate}} to calculate a "true" age column.
#'
#' @param age A numeric vector of ages. Must be either 1 long or the same length
#'   as one of the other two inputs.
#' @param year A numeric vector of tax years. Usually the TAX_YEAR column.
#'   Must be either 1 long or the same length as one of the other two inputs.
#' @param town A character vector of town codes or names. Must be either 1 long
#'   or the same length as one of the other two inputs.
#'
#' @return A numeric vector of "true" ages the same length as the longest input
#'   vector.
#'
#' @examples
#'
#' # Simplest case, get true age of one property
#' chars_fix_age(80, 2015, "Evanston")
#'
#' # Getting many ages for different towns
#' chars_fix_age(80, 2015, c("Evanston", "Niles"))
#'
#' # Creating mock data then fixing a column
#' df <- dplyr::tibble(
#'   age = c(120, 120, 123),
#'   year = c(2014, 2015, 2016),
#'   town = rep("25", 3)
#' )
#'
#' df$true_age <- chars_fix_age(df$age, df$year, df$town)
#' df
#' @family chars_funs
#' @export
chars_fix_age <- function(age, year, town) {

  # Input checking and error handling
  stopifnot(
    is.numeric(age),
    is.na(age) | age >= 0, # Age must be greater than 0 or NA
    is.numeric(year), # Input years must be numeric
    is.character(town) # Input vector must be char.
  )

  # Calculate the year offset to add to age
  year_diff <- year - town_get_assmnt_year(town, year, round_type = "floor")

  # Throw error if length of age isn't multiple of other two args
  diffs <- list(age, year_diff)
  diffs <- diffs[order(sapply(diffs, length), decreasing = TRUE)]
  if (length(diffs[[1]]) %% length(diffs[[2]]) != 0) {
    stop("Longer argument must be a multiple of length of shorter")
  }

  return(age + year_diff)
}


# nolint start
#' Return active years given a 288 start date and township
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
#' \href{https://www.ilga.gov/legislation/ilcs/fulltext.asp?DocName=003502000K15-180}{Per Illinois statute},
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
# nolint end
chars_288_active <- function(start_year, town) {

  # Input checking and error handling
  stopifnot(
    is.numeric(start_year), # Input years must be numeric
    is.character(town) # Input vector must be char.
  )

  # Loop through inputs and calculate which years between start_year and
  # start_year + 8 will be active based on the town's reassessment cycle
  out <- tryCatch(
    expr = {
      mapply(
        function(x, y) {
          if (is.na(x) | is.na(y)) {
            return(NA_real_)
          } else {
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
    },
    error = function(e) stop(e),
    warning = function(w) {
      stop("Longer argument must be a multiple of length of shorter")
    }
  )

  return(out)
}
