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
#' \code{lubridate::period} depends on calendar. So, giving a DoB will use the
#' latter.
#' @keywords internal
as_age_y <- function(age_y = 0, age_m = 0, age_d = 0, birth_date = NULL) {
  stopifnot(sum(c(age_y, age_m, age_d) != 0) == 1)

  if (is.null(birth_date)) {
    dur <-
      lubridate::years(age_y) +
      base::months(age_m) +
      lubridate::days(age_d) %>%
      as_duration()
  }


  if (is.null(age_y)) {
    if (is.null(age_d))
      age_y = age_m / 12
    else
      age_y = age_d / 365
  }
  age_y
}

is_adult <- function(age_y)
  age_y >= 18

