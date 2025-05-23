% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vars_funs.R
\name{vars_recode}
\alias{vars_recode}
\title{Replace numerically coded variables with human-readable values}
\usage{
vars_recode(
  data,
  cols = dplyr::everything(),
  code_type = "long",
  as_factor = TRUE,
  dictionary = ccao::vars_dict
)
}
\arguments{
\item{data}{A data frame or tibble with columns to have values replaced.}

\item{cols}{A \code{<tidy-select>} column selection or vector of column
names. Looks for all columns with numerically encoded character
values by default.}

\item{code_type}{Output/recode type. See description for options.}

\item{as_factor}{If \code{TRUE}, re-encoded values will be returned as
factors with their levels pre-specified by the dictionary. Otherwise, will
return re-encoded values as characters only.}

\item{dictionary}{The dictionary used to translate encodings. Uses
\code{\link{vars_dict}} by default. Use \code{\link{vars_dict_legacy}} for
legacy data column encodings.}
}
\value{
The input data frame with re-encoded values for the specified
columns.
}
\description{
The system of record stores characteristic values in a
numerically encoded format. This function can be used to translate those
values into a human-readable format. For example, EXT_WALL = 2 will become
EXT_WALL = "Masonry". Note that the values and their translations are
must be specified via a user-defined dictionary. The default dictionary is
\code{\link{vars_dict}}.

Options for \code{code_type} are:
\itemize{
\item \code{"long"}, which transforms EXT_WALL = 1 to EXT_WALL = Frame
\item \code{"short"}, which transforms EXT_WALL = 1 to EXT_WALL = FRME
\item \code{"code"}, which keeps the original values (useful for removing
improperly coded values, see note below)
}
}
\note{
Values which are in the data but are NOT in \code{\link{vars_dict}}
will be converted to NA. For example, there is no numeric value 3 for AIR,
so it will become NA.
}
\examples{

# Recode all char columns
sample_data <- chars_sample_universe[1:5, 18:27]

sample_data
vars_recode(
  data = sample_data,
  dictionary = ccao::vars_dict_legacy
)
vars_recode(
  data = sample_data,
  code_type = "short",
  dictionary = ccao::vars_dict_legacy
)

# Recode only the specified columns
gar_sample <- chars_sample_universe[1:5, 30:40]

gar_sample
vars_recode(
  data = gar_sample,
  cols = dplyr::starts_with("GAR"),
  dictionary = ccao::vars_dict_legacy
)
vars_recode(
  data = gar_sample,
  cols = "GAR1_SIZE",
  dictionary = ccao::vars_dict_legacy
)

# Using data from Athena
sample_data_athena <- chars_sample_athena[1:5, c(1:5, 10:20)]
sample_data_athena
vars_recode(
  data = sample_data_athena,
  code_type = "code",
  dictionary = ccao::vars_dict_legacy
)
vars_recode(
  data = sample_data_athena,
  code_type = "long",
  dictionary = ccao::vars_dict_legacy
)
}
\seealso{
Other vars_funs: 
\code{\link{vars_rename}()}
}
\concept{vars_funs}
