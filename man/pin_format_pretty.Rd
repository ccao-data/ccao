% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pin_funs.R
\name{pin_format_pretty}
\alias{pin_format_pretty}
\title{Add dash separators to input PIN(s)}
\usage{
pin_format_pretty(pin, full_length = FALSE)
}
\arguments{
\item{pin}{A Property Index Number (PIN) or PINs which are cleaned and either
10 or 14 digits long.}

\item{full_length}{Default FALSE. When TRUE, 14-digit PINs remain 14 digits
and 10-digit PINs remain 10 digits. When FALSE, all PINs are coerced to
10 digits.}
}
\value{
A vector of PINs with separator dashes inserted in the expected
  places. Ex. 04-34-106-008-0000
}
\description{
Add separators to cleaned PINs to make them more readable in reports
and other human-ready outputs. This requires 10 or 14 digits PINs as input.
Preserve input PIN length.
}
\examples{

pins <- c("04341060080000", "01222040030000")

pin_format_pretty(pins)
}
