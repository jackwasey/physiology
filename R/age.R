#' age from birth and reference dates
#'
#' Calculate age at time of reference date, based on birth date.
#' @param birth_date Date of birth, either as a \code{Date} or something which
#'   will be converted to a \code{Date}
#' @param ref_date Date at which to calculate age, defaults to current date,
#'   either as a \code{Date} or something which will be converted to a
#'   \code{Date}
#' @param unit character of length, one of "year" or "day".
#' @return integer vector
#' @examples
#' age_from_dates("2014-11-08", "2014-12-31", unit = "day")
#' age_from_dates("1981-07-09", "2014-06-29", unit = "year")
#' @references
#' https://stackoverflow.com/questions/31126726/efficient-and-accurate-age-calculation-in-years-months-or-weeks-in-r-given-b
#' @export
age_from_dates <- function(birth_date, ref_date = Sys.Date(),
                           unit = c("year", "month", "day")) {
  unit <- match.arg(unit)
  pd <- lubridate::as.period(lubridate::interval(start = birth_date,
                                                 end = ref_date), unit = unit)
  switch(unit,
         year = lubridate::year(pd),
         month = lubridate::month(pd),
         day = lubridate::day(pd)
  )
}

#nocov start

#' Calculate age in years from other units
#' @param age_m Months
#' @param age_d Days
#' @examples
#' age_m(12)
#' age_m(1)
#' @keywords manip
#' @export
age_m <- function(age_m) {
  age_m / 12
}

#' @rdname age_m
age_d <- function(age_d) {
  age_d / 365.25
}

#' Is age >= 18 years
#' @keywords internal
is_adult <- function(age_y) {
  age_y >= 18
}

#nocov end
