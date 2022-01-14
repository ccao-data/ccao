#' Official CCAO color palette
#'
#' The CCAO Communications Department created a palette of colors used widely
#' throughout CCAO press materials and visualizations. Navy, gold, and
#' buttermilk are the colors used in the CCAO logo.
#'
#' @format A named list containing 12 CCAO colors
#'
"ccao_colors"


#' Data dictionary of codes used to identify specific property situations
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


#' Sample dataset from the VW_RES_UNIVERSE SQL view
#'
#' A dataset containing a small subsample of rows from the VW_RES_UNIVERSE view.
#' The PINs present in this subsample match those present in
#' \code{chars_sample_addchars}.
#'
#' @source This data was extracted from SQL manually on 2021-01-20.
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


#' Data dictionary of Certificate of Error reason codes
#'
#' A dataset containing numeric codes and corresponding text explanations for
#' Certificates of Error (CoEs). These are issued when the Assessor's office
#' makes some kind of mistake that leads to an erroneous valuation.
#'
#' @format A data frame with 95 rows and 2 variables:
#' \describe{
#'   \item{coe_code}{The numeric code for a given CoE}
#'   \item{coe_reason}{The text explaination for the CoE numeric code}
#' }
#'
"coe_dict"


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


#' Data dictionary for CCAO data sets and variables
#'
#' A crosswalk of CCAO variable names used in iasWorld, AWS, modeling,
#' and open data. Also includes a translation of numeric character codes
#' to their human-readable value (ROOF_CNST = 1
#' becomes ROOF_CNST = Shingle/Asphalt).
#'
#' @format A data frame with 442 rows and 17 variables:
#' \describe{
#'   \item{var_from_source}{Name of the raw data source that the variable
#'   was extracted from}
#'   \item{var_from_table}{Name Athena table containing the variable, if
#'   applicable}
#'   \item{var_from_ctas}{Name of the CREATE TABLE AS SELECT statement that
#'   created the variable, if applicable}
#'   \item{var_from_view}{Name of the Athena view that contains the variable.
#'   This is typically the main data source for end users}
#'   \item{var_name_iasworld}{Column name for variable as stored in the system
#'   of record (iasWorld)}
#'   \item{var_name_athena}{Column name used for views and tables in AWS
#'   Athena}
#'   \item{var_name_model}{Column name used while data is flowing through
#'   modeling pipelines}
#'   \item{var_name_publish}{Human-readable column name used for public data
#'   sets}
#'   \item{var_name_pretty}{Human-readable column name used for publication
#'   and reporting}
#'   \item{var_type}{Variable type/prefix indicating the variable's function.
#'   For example, ind_ variables are always indicators (booleans), while char_
#'   variables are always property characteristics.}
#'   \item{var_data_type}{R data type variable values should be stored as}
#'   \item{var_is_published}{Logical value indicating whether to publish in
#'   open data}
#'   \item{var_is_predictor}{Logical value indicating whether to use variable
#'   in modeling on the right-hand side. Left-hand side is always sale price}
#'   \item{var_code}{Factor value for categorical variable. These are the values
#'   stored in the system of record}
#'   \item{var_value}{Human-readable translation of factor value}
#'   \item{var_value_short}{Human-readable translation of factor value, but as
#'   short as possible}
#'   \item{var_notes}{Field descriptions and caveats}
#' }
"vars_dict"


#' Data dictionary for legacy CCAO data and variables
#'
#' NOTE: This dictionary is deprecated for any processes that use current CCAO
#' data infrastructure. Use \code{\link{vars_dict}} for the most up-to-date
#' dictionary.
#'
#' A crosswalk of CCAO variable names used in SQL, modeling, and open data. Also
#' includes a translation of numeric character codes to their human-readable
#' value (ROOF_CNST = 1 becomes ROOF_CNST = Shingle/Asphalt).
#'
#' @format A data frame with 202 rows and 14 variables:
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
"vars_dict_legacy"
