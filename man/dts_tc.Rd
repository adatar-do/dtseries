% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts_tci.R
\name{dts_tc}
\alias{dts_tc}
\title{Calculate time changes (Interannual, Previous period, or Year-to-date cumulative)
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}}
\usage{
dts_tc(
  .ts,
  .vars = NULL,
  .type = c("i", "p", "c"),
  .base = 1,
  drop_vars = TRUE,
  na.omit = drop_vars
)
}
\arguments{
\item{.ts}{A time series object of class \code{ts} or \code{mts}.}

\item{.vars}{Optional character vector of column names to calculate time changes for. If NULL (default),
all columns are used.}

\item{.type}{The type of time change to calculate:
\itemize{
\item "i": Interannual change (default)
\item "p": Change from the previous period
\item "c": Year-to-date cumulative change
}}

\item{.base}{The base for calculating the index or percentage change.
ie. 100 for percentage change or 1 for index change. Default is 1.}

\item{drop_vars}{Logical. If TRUE (default), the original variables are dropped from the output.}

\item{na.omit}{Logical. If TRUE (default), remove rows with NA values.}
}
\value{
A time series object with the calculated time changes.
}
\description{
This function calculates time changes for a time series object, offering options for
interannual change, change from the previous period, or year-to-date cumulative change.
}
\examples{
\dontrun{
  data(AirPassengers)
  # Interannual change
  dts_tc(AirPassengers, .type = "i")

  # Change from the previous period
  dts_tc(AirPassengers, .type = "p")

  # Year-to-date cumulative change
  dts_tc(AirPassengers, .type = "c")
}
}
