#' @title age from birth and reference dates
#' @param birthDate Date of birth, either as a \code{Date} or something which
#'   will be converted to a \code{Date}
#' @param refDate Date at which to calculate age, defaults to current date,
#'   either as a \code{Date} or something which will be converted to a
#'   \code{Date}
#' @param unit character of length, one of "year", "month", or "day".
#' @return integer vector
#' @examples
#' ageFromDates("2014-11-08", "2014-12-31", unit = "day")
#' ageFromDates("1981-07-09", "2014-06-29", unit = "year")
#' @export
ageFromDates <- function(birthDate, refDate = Sys.Date(),
                         unit = c("year", "day")) {
  unit <- match.arg(unit)
  birthDate <- as.Date(birthDate)
  refDate <- as.Date(refDate)
  pdt <- as.POSIXlt(c(birthDate, refDate))

  years <- pdt$year[2] - pdt$year[1]
  months <- pdt$mon[2] - pdt$mon[1] # of year
  days <- pdt$mday[2] - pdt$mday[1] # of month
  year.correct <- (months + days / 32) < 0
  if (unit == "year")
    return(years - year.correct)  # not birthday yet this year

  as.integer(refDate - birthDate) # days
}

# TODO: print class to show days to 30d, months to 24 months, then years
# automatically
