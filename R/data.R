#' Data dictionary for CCAO township codes and triads.
#'
#' A dataset containing a lookup of various townships and their
#' respective triads.
#'
#' @format A data frame with 38 rows and 4 variables:
#' \describe{
#'   \item{township_name}{Common name of the township}
#'   \item{township_code}{Two-digit code used to identify the township}
#'   \item{triad_code}{Single-digit code of the triad the township is in}
#'   \item{triad_name}{Common name of the triad the township is in}
#' }
#'
"town_dict"


#' Data dictionary for CCAO characteristic values.
#'
#' A dataset containing human-readable version of numeric characteristic
#' encodings of property characteristic data.
#'
#' @format A data frame with 61 rows and 4 variables:
#' \describe{
#'   \item{char_name}{Column name of variable in CCAO SQL server}
#'   \item{char_code}{Value code used in CCAO SQL server and in paper forms}
#'   \item{char_value}{Actual meaning of char_code}
#'   \item{char_value_short}{Actual meaning of char_code, shortened}
#' }
#'
#' @source This dictionary was manually created from paper forms as a
#'   translation of numeric variables. char_value_short is the equivalent of
#'   what is used on the AS400 property info screens
"chars_dict"


#' Data dictionary of Cook County property classes.
#'
#' A dataset containing a translation for property class codes to
#' human-readable class descriptions. Also describes which classes are included
#' in residential regressions and reporting classes.
#'
#' @format A data frame with 197 rows and 7 variables:
#' \describe{
#'   \item{major_class_code}{First digit of class code, major class}
#'   \item{major_class_type}{Human-readable description of the major class}
#'   \item{assessment_level}{Level of assessment for the property class}
#'   \item{regression_class}{Boolean indicating whether or not this class is
#'   included in CAMA regressions}
#'   \item{class_code}{Full class code of the property sub-class, character}
#'   \item{class_desc}{Human-readable description of the property sub-class}
#' }
#'
#' @note Includes all Cook County real property classes.
"class_dict"


#' Codes used by the CCAO to identify certain distinct property situations.
#'
#' A dataset containing a lookup of CDU codes. These codes are kind of a mess
#' and have been created and used inconsistently over the years.
#'
#' They can indicate special property types or specific incentives applied to
#' a property.
#'
#' @format A data frame with 47 rows and 4 variables:
#' \describe{
#'   \item{cdu_code}{Two-letter code containing the CDU, as shown in the AS400}
#'   \item{cdu_type}{Class/type of property the CDU applies to}
#'   \item{cdu_desc}{Full description of the CDU}
#'   \item{cdu_desc_short}{Short description of the CDU}
#' }
#'
"cdu_dict"
