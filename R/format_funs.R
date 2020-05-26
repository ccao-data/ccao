#' Format Predicted Values for Upload to AS/400
#'
#' @description Format input vectors of town, PIN, and predicted values into a
#' fixed-width (80 character) text file consumable by the CCAO's AS/400 system.
#'
#' @param town_code Two digit township code for the specified PIN
#' @param pin Fourteen digit property index number (PIN)
#' @param pred_value Predicted/model value for the specified PIN
#' @param file Output file to write to; file extension should be .txt
#' @param type Change file output style. Options are \code{"res"} or 
#' \code{"condo"}. Res outputs 80 character fixed width files while condo 
#' outputs 21 character files.
#'
#' @return A fixed-width text file saved to the location specified by
#' \code{file}, containing the values of \code{town_code}, \code{pin},
#' \code{pred_value}.
#'
#' @export
format_as400 <- function(town_code, pin, pred_value, file, type = "res") {

  # Check that file extension is .txt, which is necessary for AS400 upload
  if (strsplit(basename(file), split = "\\.")[[1]][-1] != "txt") {
    stop("Extension for file used with the AS/400 should be .txt")
  } else if (anyDuplicated(pin)) {
    stop("Duplicated PINs present in the PIN input vector")
  }

  # Input checks to ensure things are as expected
  stopifnot(
    type %in% c("res", "condo"),
    length(town_code) == length(pin),
    length(town_code) == length(pred_value),
    nchar(town_code) == 2,
    nchar(pin) == 14,
    is.numeric(pred_value),
    is.character(pin),
    is.vector(town_code) & is.vector(pin) & is.vector(pred_value),
    is.character(file),
    all(!grepl("[^0-9]", pin)),
    all(!grepl("[^0-9]", pred_value))
  )

  # Format values as a fixed-width strings and check all lines are correct len
  if (type == "res") {
    formatted_vals <- paste0(
      town_code, " ", pin, "           ",
      sprintf("%09d", pred_value), "                                          A"
    )
    
    stopifnot(sapply(formatted_vals, nchar) == 80)
    
  } else {
    formatted_vals <- paste0(
      town_code,
      substr(pin, 1, 10),
      sprintf("%09d", pred_value)
    )
    
    stopifnot(sapply(formatted_vals, nchar) == 21)
  }

  # Output formatted value as a fixed-width text file
  utils::write.table(
    as.data.frame(formatted_vals),
    file = file,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE
  )
}
