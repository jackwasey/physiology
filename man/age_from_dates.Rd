% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age.R
\name{age_from_dates}
\alias{age_from_dates}
\title{age from birth and reference dates}
\usage{
age_from_dates(
  birth_date,
  ref_date = Sys.Date(),
  unit = c("year", "month", "day")
)
}
\arguments{
\item{birth_date}{Date of birth, either as a \code{Date} or something which
will be converted to a \code{Date}}

\item{ref_date}{Date at which to calculate age, defaults to current date,
either as a \code{Date} or something which will be converted to a
\code{Date}}

\item{unit}{character of length, one of "year" or "day".}
}
\value{
integer vector
}
\description{
Calculate age at time of reference date, based on birth date, rounded to the
given unit. These are designed for physiologic estimations, not for accuracy.
The dates can be given as anything which can be coerced into a \code{Date}.
}
\examples{
age_from_dates("2014-11-08", "2014-12-31", unit = "day")
age_from_dates("2014-11-08", "2014-12-31", unit = "day")
age_from_dates("1981-07-09", "2014-06-29", unit = "year")
# age must be zero or positive, may be in future, or error is thrown
\dontrun{
age_from_dates("2120-10-10", "2119-01-01")
}
# leap days work: we are just using internal R date manipulation
age_from_dates("2000-02-28", "2000-03-01", unit = "day")
age_from_dates("2004-02-28", "2004-03-01", unit = "day")
age_from_dates("1900-02-28", "1900-03-01", unit = "day")
age_from_dates("1901-02-28", "1901-03-01", unit = "day")
}
\references{
https://stackoverflow.com/questions/31126726
}
