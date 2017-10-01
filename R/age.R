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
  pd <- lubridate::as.period(lubridate::new_interval(start = birth_date,
                                                     end = ref_date))
  switch(unit,
         year = lubridate::year(pd),
         month = lubridate::month(pd),
         day = lubridate::day(pd)
  )
}
