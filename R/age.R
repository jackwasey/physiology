#' age from birth and reference dates
#'
#' Calculate age at time of reference date, based on birth date, rounded to the
#' given unit. These are designed for physiologic estimations, not for accuracy.
#' The dates can be given as anything which can be coerced into a \code{Date}.
#' @param birth_date Date of birth, either as a \code{Date} or something which
#'   will be converted to a \code{Date}
#' @param ref_date Date at which to calculate age, defaults to current date,
#'   either as a \code{Date} or something which will be converted to a
#'   \code{Date}
#' @param unit character of length, one of "year" or "day".
#' @return integer vector
#' @examples
#' age_from_dates("2014-11-08", "2014-12-31", unit = "day")
#' age_from_dates("2014-11-08", "2014-12-31", unit = "day")
#' age_from_dates("1981-07-09", "2014-06-29", unit = "year")
#' # age must be zero or positive, may be in future, or error is thrown
#' \dontrun{
#' age_from_dates("2120-10-10", "2119-01-01")
#' }
#' # leap days work: we are just using internal R date manipulation
#' age_from_dates("2000-02-28", "2000-03-01", unit = "day")
#' age_from_dates("2004-02-28", "2004-03-01", unit = "day")
#' age_from_dates("1900-02-28", "1900-03-01", unit = "day")
#' age_from_dates("1901-02-28", "1901-03-01", unit = "day")
#' @references https://stackoverflow.com/questions/31126726
#' @export
age_from_dates <- function(birth_date, ref_date = Sys.Date(),
                           unit = c("year", "month", "day")) {
  birth_date <- as.Date(birth_date)
  ref_date <- as.Date(ref_date)
  unit <- match.arg(unit)
  age <- as.numeric(ref_date - birth_date)
  if (age < 0)
    stop("Calculated age is less than zero")
  switch(unit,
         year = age_d_to_y(age),
         month = age_d_to_m(age),
         day = age
  )
}

#nocov start

#' Calculate age in years from other units
#' @param age_m Months
#' @param age_d Days
#' @examples
#' age_m_to_y(12)
#' age_m_to_y(1)
#' @keywords manip
#' @rdname age_m
#' @export
age_m_to_y <- function(age_m) {
  age_m / 12
}

#' @rdname age_m
#' @export
age_d_to_y <- function(age_d) {
  age_d / 365.25
}

#' @rdname age_m
#' @export
age_d_to_m <- function(age_d) {
  age_d / (365.25 / 12)
}

#' Is age >= 18 years
#' @template age_y
#' @export
is_adult <- function(age_y) {
  age_y >= 18
}

#nocov end
