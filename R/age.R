#' age from birth and reference dates
#'
#' Calculate age at time of reference date, based on birth date.
#' @param birth.date Date of birth, either as a \code{Date} or something which
#'   will be converted to a \code{Date}
#' @param ref.date Date at which to calculate age, defaults to current date,
#'   either as a \code{Date} or something which will be converted to a
#'   \code{Date}
#' @param unit character of length, one of "year" or "day".
#' @return integer vector
#' @examples
#' age_from_dates("2014-11-08", "2014-12-31", unit = "day")
#' age_from_dates("1981-07-09", "2014-06-29", unit = "year")
#' @references
#' https://stackoverflow.com/questions/31126726/efficient-and-accurate-age-calculation-in-years-months-or-weeks-in-r-given-b
#' @import lubridate
#' @export
age_from_dates <- function(birth_date, ref_date = Sys.Date(),
                           unit = c("year", "month", "day")) {
  unit <- match.arg(unit)
  pd <- as.period(new_interval(start = birth_date, end = ref_date))
  switch(unit,
         year = lubridate::year(pd),
         month = lubridate::month(pd),
         day = lubridate::day(pd)
  )
}

#' Calculate age in years from ages in other units
#'
#' Only one of years, months or days should be supplied. If birth date is known,
#' we can use a calendar to do this exactly. If not known, then we let lubridate
#' apporximate.
#'
#' \code{lubridate::duration} gives mathematically predictable results,
#' \code{lubridate::period} depends on calendar. So, giving a DoB will use the latter
#' \code{lubridate::interval} is a period with a base date
#' @keywords internal
as_age_y <- function(age_y, age_m, age_d, birth_date) {
  stopifnot(sum(missing(age_y), missing(age_m), missing(age_d)) == 2)
  if (missing(age_y)) age_y <- 0
  if (missing(age_m)) age_m <- 0
  if (missing(age_d)) age_d <- 0

  if (missing(birth_date)) {
    # lubridate rightly refuses to add months like this, so estimate by hand:
    return(age_y + age_m/12 + age_y/365.25)
  }

  per <- lubridate::period(c(age_y, age_m, age_d),
                           c("year", "month", "day"))
  interval <- birth_date + per
  difftime(interval, birth_date, units = "days") %>% as.numeric / 365.25
}

is_adult <- function(age_y)
  age_y >= 18

