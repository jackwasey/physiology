#' @title age from birth and reference dates
#' @param birth.date Date of birth, in format accepted by lubridate
#' @param ref.date Date at which to calculate age, defaults to current date, in
#'   format accepted by lubridate, e.g. Date.
#' @param unit character of length, one of "year", "month", or "day".
#' @import lubridate
#' @export
ageFromDates <- function(birth.date, ref.date = Sys.Date(),
                         unit = c("year", "month", "day")) {
  period <- as.period(new_interval(birth.date, ref.date), unit = unit)
  period[unit]
}

# do this as S3 class?
age <- function(i) {
#  if (class(i) != "interval") stop("must construct with an inteval")
  #structure(i, class = c("age", ""))
}

as.character.age <- function(a) {
  prd <- as.period(a)
  y <- year(prd)
  m <- month(prd)
  d <- prd %/% days(1) # number of days, including months and years

  if (y >= 2) return(y)
  if (d < 28) return(d)
  if (m == 0) return (1)
  return(m)
}

#' @title print age in human readable format
#' @description based on age, will report in days, weeks, months, years.
#' @param age vector
#' @param ageunit single value (all ages must have the same unit)
#' @return character vector
# print.age <- function() stop("todo")
