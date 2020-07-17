% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{chars_cols}
\alias{chars_cols}
\title{List crosswalk of CCAOSFCHARS columns and their ADDCHARS equivalents}
\format{
A list with 4 items:
\describe{
  \item{add_target}{Target columns with additive characteristics}
  \item{add_source}{Cols with source of data to add to \code{add_target}}
  \item{rep_target}{Target columns with characteristics to be replaced}
  \item{rep_source}{Cols with source of data to replace in \code{rep_target}}
}
}
\usage{
chars_cols
}
\description{
A list containing vectors of column names that match those found in the
CCAOSFCHARS and ADDCHARS SQL tables. Can be used to translate between
tables.
}
\keyword{datasets}