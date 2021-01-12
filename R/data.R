#' Appeal reason codes used by the CCAO AS/400 system
#'
#' A dataset containing a lookup of codes for appeal rejection/acceptance. These
#' codes are primarily used in the DKEMPL SQL table. See the \href{https://prodassets.cookcountyassessor.com/s3fs-public/form_documents/reasoncodes.pdf}{CCAO website} for a PDF version. # nolint
#'
#' @format A data frame with 78 rows and 2 variables:
#' \describe{
#'   \item{reason_code}{Numeric code used in the AS/400 system}
#'   \item{reason_desc}{Text description/translation of the numeric code}
#' }
#'
"appeals_dict"


#' Official CCAO color palette
#'
#' The CCAO Communications Department created a palette of colors used widely
#' throughout CCAO press materials and visualizations. Navy, gold, and
#' buttermilk are the colors used in the CCAO logo.
#'
#' @format A named list containing 12 CCAO colors
#'
"ccao_colors"


#' Codes used by the CCAO to identify certain distinct property situations
#'
#' A dataset containing a lookup of CDU codes. These codes are kind of a mess
#' and have been created and used inconsistently over the years.
#'
#' They can indicate special property types or specific incentives applied to
#' a property.
#'
#' @format A data frame with 47 rows and 4 variables:
#' \describe{
#'   \item{cdu_code}{Two-letter code containing the CDU, as shown in the AS/400}
#'   \item{cdu_type}{Class/type of property the CDU applies to}
#'   \item{cdu_desc}{Full description of the CDU}
#'   \item{cdu_desc_short}{Short description of the CDU}
#' }
#'
"cdu_dict"


#' List crosswalk of CCAOSFCHARS columns and their ADDCHARS equivalents
#'
#' A list containing vectors of column names that match those found in the
#' CCAOSFCHARS and ADDCHARS SQL tables. Can be used to translate between
#' tables.
#'
#' @format A list with 4 items:
#' \describe{
#'   \item{add_target}{Target columns with additive characteristics}
#'   \item{add_source}{Cols with source of data to add to \code{add_target}}
#'   \item{rep_target}{Target columns with characteristics to be replaced}
#'   \item{rep_source}{Cols with source of data to replace in \code{rep_target}}
#' }
#'
"chars_cols"


#' Sample dataset from the ADDCHARS SQL table
#'
#' A dataset containing a small subsample of rows from the ADDCHARS table. This
#' sample can be used with \code{chars_sparsify()} to generate sparse data
#' frames suitable for joining onto \code{chars_sample_universe}.
#'
#' @source This data was extracted from SQL by the script at
#' \code{data-raw/chars_sample.R}.
"chars_sample_addchars"


#' Sample dataset from the VW_RES_UNIVERSE SQL view
#'
#' A dataset containing a small subsample of rows from the VW_RES_UNIVERSE view.
#' The PINs present in this subsample match those present in
#' \code{chars_sample_addchars}.
#'
#' @source This data was extracted from SQL by the script at
#' \code{data-raw/chars_sample.R}.
"chars_sample_universe"


#' Data dictionary of Cook County property classes
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
#'   \item{reporting_class}{Reporting class used for internal CCAO reports
#'   and aggregate statistics}
#'   \item{class_code}{Full class code of the property sub-class, character}
#'   \item{class_desc}{Human-readable description of the property sub-class}
#'   \item{min_size}{Integer of minimum size for 200-class property codes}
#'   \item{max_size}{Integer of maximum size for 200-class property codes}
#'   \item{min_age}{Integer of minimum age for 200-class property codes}
#'   \item{max_age}{Integer of maximum age for 200-class property codes}
#' }
#'
#' @note Includes all Cook County real property classes.
"class_dict"


#' Data frame of recodes for CCAO modeling neighborhoods
#'
#' A dataset of neighborhood recodes for CCAO modeling. These neighborhoods
#' either don't exist or are errors in the data. Can be easily be used with
#' dplyr's recode. See examples.
#'
#' @format A data frame with 58 rows and 5 variables:
#' \describe{
#'   \item{township_name}{Common name of the township}
#'   \item{township_code}{Two-digit code used to identify the township}
#'   \item{nbhd}{Three-digit assessor neighborhood code, zero-padded}
#'   \item{town_nbhd}{Combined township code and neighborhood}
#'   \item{recode_to}{Neighborhood to recode TO from town_nbhd. In other words,
#'   values coded as town_nbhd should be replaced with recoded_to}
#' }
#'
#' @examples
#'
#' \dontrun{
#'
#' library(dplyr)
#'
#' # Create a named list of recodes to use with dplyr::recode
#' vals <- ccao::nbhd_recode$recode_to
#' names(vals) <- ccao::nbhd_recode$town_nbhd
#'
#' # Create test recode data
#' test_nbhds <- c("12122", "28103", "39010", "34220", "12000")
#'
#' # Use dplyr to recode all values. Triple !!! expands the named vector vals
#' # into individual arguments passed to recode()
#' recode(test_nbhds, !!!recodes)
#' }
#'
"nbhd_recode"


#' Simple features (sf) data frame of CCAO neighborhoods
#'
#' An \code{sf} spatial data frame containing geometries, names, and codes for
#' CCAO neighborhoods. Use for thematic maps only; these boundaries are
#' recovered from old files and may not be perfectly accurate.
#'
#' NOTE: These boundaries are a simplified version of the boundaries available
#' in the data-raw/ folder.
#'
#' @format A spatial data frame with 845 geometries and 6 variables.
#' \describe{
#'   \item{township_name}{Common name of the township}
#'   \item{township_code}{Two-digit code used to identify the township}
#'   \item{triad_code}{Single-digit code of the triad the township is in}
#'   \item{triad_name}{Common name of the triad the township is in}
#'   \item{nbhd}{Three-digit assessor neighborhood code, zero-padded}
#'   \item{geometry}{Attribute column containing sf geometries}
#' }
#'
"nbhd_shp"


#' Data dictionary for CCAO township codes and triads
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


#' Simple features (sf) data frame of Cook County townships
#'
#' An \code{sf} spatial data frame containing geometries, names, and codes for
#' Cook County political/administrative townships. Use for thematic
#' mapping only.
#'
#' NOTE: These boundaries are a simplified version of the boundaries available
#' in the data-raw/ folder.
#'
#' @format A spatial data frame with 38 geometries and 3 variables.
#' \describe{
#'   \item{township_name}{Common name of the township}
#'   \item{township_code}{Two-digit code used to identify the township}
#'   \item{triad_code}{Single-digit code of the triad the township is in}
#'   \item{triad_name}{Common name of the triad the township is in}
#'   \item{geometry}{Attribute column containing sf geometries}
#' }
#'
"town_shp"


#' Data dictionary for CCAO variables
#'
#' A crosswalk of CCAO variable names used in SQL, modeling, and open data. Also
#' includes a translation of numeric character codes to their human-readable
#' value (ROOF_CNST = 1 becomes ROOF_CNST = Shingle/Asphalt).
#'
#' @format A data frame with 194 rows and 14 variables:
#' \describe{
#'   \item{var_name_sql}{Column name of variable as stored in CCAO SQL servers}
#'   \item{var_name_addchars}{Column name of variable when stored in the
#'   ADDCHARS SQL table}
#'   \item{var_name_socrata}{Column name used for the Cook County Data Portal}
#'   \item{var_name_standard}{Column name used for flat files, open data, and
#'   modeling}
#'   \item{var_name_pretty}{Human-readable column name used for publication}
#'   \item{var_type}{Variable type: meta = identifying information; char =
#'   characteristics retrieved from SQL; time = calculated time variable; ind =
#'   calculated logical/indicator variable; econ = calculated economic
#'   information; geo = attached/calculated geographic information}
#'   \item{var_data_type}{R data type variable values should be stored as}
#'   \item{var_is_published}{Logical value indicating whether to publish in
#'   open data}
#'   \item{var_is_clustered}{Logical value indicating whether to use variable
#'   in comparables clustering algorithm}
#'   \item{var_is_predictor}{Logical value indicating whether to use variable
#'   in modeling on the right-hand side. Left-hand side is always sale price}
#'   \item{var_code}{Factor value for categorical variable. These are the values
#'   stored in the AS/400}
#'   \item{var_value}{Human-readable translation of factor value}
#'   \item{var_value_short}{Human-readable translation of factor value, but as
#'   short as possible}
#'   \item{var_notes}{Field descriptions and caveats}
#' }
#'
#' @source This dictionary was manually created from paper forms as a
#'   translation of numeric variables. char_value_short is the equivalent of
#'   what is used on the AS/400 property info screens
"vars_dict"
