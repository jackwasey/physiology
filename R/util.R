#' @title age from birth and reference dates
#' @import lubridate
ageFromDates <- function(birthDate, refDate = Sys.Date(), unit = c("year", "month", "day")) {

  period <- as.period(new_interval(birthDate, refDate), unit = unit)

  period[unit]

}
