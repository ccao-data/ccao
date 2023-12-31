#' Remove separators and whitespace from input PIN(s) and warn if invalid
#'
#' Remove common separators, whitespace, and trailing characters
#' from Property Index Numbers (PINs). This function is specifically formatted
#' for PINs, which are expected to be 10 or 14-digit numbers saved as
#' characters (PINs often have leading zeros).
#'
#' @param pin A Property Index Number (PIN) or multiple PINs containing
#'   unnecessary whitespace or separator characters
#'
#' @return A vector of clean PINs with no whitespace or separators.
#'
#' @examples
#'
#' pins <- c("04-34-106-008-0000", " 14172 27008 0000")
#'
#' pin_clean(pins)
#' @export
pin_clean <- function(pin) {
  stopifnot(
    is.vector(pin), # Must be vector input
    is.character(pin) # Input vector must be char
  )

  # No alphabet chars else warn
  if (any(grepl("[a-z]", pin))) {
    warning(
      "PIN(s) contain alphabetical characters, they should be only numbers"
    )
  }

  # Remove separators and quotes
  out <- gsub(pattern = "-|'|\\\"", replacement = "", as.character(pin))

  # Remove all whitespace
  out <- gsub(pattern = " ", replacement = "", out, fixed = TRUE)

  # Remove trailing and leading whitespace
  out <- trimws(out)

  return(out)
}


#' Add dash separators to input PIN(s)
#'
#' Add separators to cleaned PINs to make them more readable in reports
#' and other human-ready outputs. This requires 10 or 14 digits PINs as input.
#' Preserve input PIN length.
#'
#' @param pin A Property Index Number (PIN) or PINs which are cleaned and either
#'   10 or 14 digits long.
#'
#' @param full_length Default FALSE. When TRUE, 14-digit PINs remain 14 digits
#'   and 10-digit PINs remain 10 digits. When FALSE, all PINs are coerced to
#'   10 digits.
#'
#' @return A vector of PINs with separator dashes inserted in the expected
#'   places. Ex. 04-34-106-008-0000
#'
#' @examples
#'
#' pins <- c("04341060080000", "01222040030000")
#'
#' pin_format_pretty(pins)
#' @export
pin_format_pretty <- function(pin, full_length = FALSE) {
  # Input checking and error handling
  stopifnot(
    is.vector(pin), # Must be vector input
    is.character(pin), # Input vector must be char.
    !is.na(as.numeric(pin[!is.na(pin)])), # No non-numbers in PIN
    nchar(pin) == 10 | nchar(pin) == 14 | is.na(pin) # PINs 10 or 14 digit
  )

  # Get the last 4 digits of the input PINs
  pin_after_10 <- sprintf("%04d", as.numeric(substr(pin, 11, nchar(pin))))

  # If the PIN is longer 10 digits and full_length is true, append a dash
  # and then the digits after 10
  last_4 <- ifelse(
    (full_length & nchar(pin) == 14), paste0("-", pin_after_10), ""
  )

  # Create substrings of the initial pins and place dashes between them
  dash_pin <- paste0(
    substr(pin, 1, 2), "-",
    substr(pin, 3, 4), "-",
    substr(pin, 5, 7), "-",
    substr(pin, 8, 10), last_4
  )

  # Replace any NA values in the output
  final_pin <- replace(dash_pin, is.na(pin), NA_character_)

  return(final_pin)
}
