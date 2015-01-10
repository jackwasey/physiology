#' @title validate phsyiologic input parameters
#' @description User may generate warnings for unreasonable or obviously
#'   erroneous heights. TODO: use age to validate further
#' @template heightm
#' @param ht.min minimum height below which to warn if \code{warn = TRUE}
#' @param ht.max maximum height above which to warn if \code{warn = TRUE}
#' @param ht.min.hard minimum height below which to warn regardless of
#'   \code{warn}
#' @param ht.max.hard maximum height above which to warn if \code{warn}
#' @param do.warn single logical, if TRUE, will give warnings outside of soft
#'   limits
#' @param do.stop single logical, \code{stop} instead of warning if any values
#'   outside hard limits
#' @param extramsg single character string with additional message to append,
#'   default is ""
#' @export
valid_height <- function(heightm, ht.min = 0.1, ht.max = 2.5,
                         ht.min.hard = 0.001, ht.max.hard = 3,
                         extramsg = "", do.warn = TRUE, do.stop = FALSE) {
  valid(heightm, "height", "m",
        ht.min, ht.max, ht.min.hard, ht.max.hard,
        extramsg, do.warn, do.stop)
}

#' @rdname valid_height
#' @export
valid_height_adult <- function(heightm, ht.min = 0.5, ht.max = 2.5,
                         ht.min.hard = 0.001, ht.max.hard = 3,
                         extramsg = "", do.warn = TRUE, do.stop = FALSE) {
  valid(heightm, "height", "m",
        ht.min, ht.max, ht.min.hard, ht.max.hard,
        extramsg, do.warn, do.stop)
}

#' @rdname valid_height
#' @template weightkg
#' @param wt.min minimum height below which to warn if \code{warn = TRUE}
#' @param wt.max maximum height above which to warn if \code{warn = TRUE}
#' @param wt.min.hard minimum height below which to warn regardless of \code{warn}
#' @param wt.max.hard maximum height above which to warn if \code{warn}
#' @export
valid_weight <- function(weightkg, wt.min = 0.1, wt.max = 300,
                         wt.min.hard = 0, wt.max.hard = 600,
                         extramsg = "", do.warn = TRUE, do.stop = FALSE) {
  valid(weightkg, "weight", "kg",
        wt.min, wt.max, wt.min.hard, wt.max.hard,
        extramsg, do.warn, do.stop)
}

#' @rdname valid_height
#' @export
valid_weight_adult <- function(weightkg, wt.min = 5, wt.max = 300,
                         wt.min.hard = 0, wt.max.hard = 600,
                         extramsg = "", do.warn = TRUE, do.stop = FALSE) {
  valid(weightkg, "weight", "kg",
        wt.min, wt.max, wt.min.hard, wt.max.hard,
        extramsg, do.warn, do.stop)
}

#' @rdname valid_height
#' @param age.years numeric
#' @param age.min minimum height below which to warn if \code{warn = TRUE}
#' @param age.max maximum height above which to warn if \code{warn = TRUE}
#' @param age.min.hard minimum height below which to warn regardless of \code{warn}
#' @param age.max.hard maximum height above which to warn if \code{warn}
#' @export
valid_age <- function(age.years, age.min = 0, age.max = 150,
                      age.min.hard = 0.00001, age.max.hard = 150,
                      extramsg = "", do.warn = TRUE, do.stop = FALSE)
  valid(age.years, "age", "yr",
        age.min, age.max, age.min.hard, age.max.hard,
        extramsg, do.warn, do.stop)

#' @rdname valid_height
#' @export
valid_age_adult <- function(age.years, age.min = 18, age.max = 150,
                            age.min.hard = 17, age.max.hard = 150,
                            extramsg = "", do.warn = TRUE, do.stop = FALSE)
  valid(age.years, "age", "yr",
        age.min, age.max, age.min.hard, age.max.hard,
        extramsg, do.warn, do.stop)

valid <- function(var, var.name, var.unit, min, max, min.hard, max.hard,
                  extramsg = "", do.warn = TRUE, do.stop = FALSE) {
  stopifnot(is.numeric(var))
  stopifnot(is.numeric(min))
  stopifnot(is.numeric(max))
  stopifnot(is.numeric(min.hard))
  stopifnot(is.numeric(max.hard))
  stopifnot(length(min) == 1)
  stopifnot(length(max) == 1)
  stopifnot(length(min.hard) == 1)
  stopifnot(length(max.hard) == 1)
  stopifnot(is.logical(do.warn))
  stopifnot(is.logical(do.stop))
  stopifnot(length(do.warn) == 1)
  stopifnot(length(do.stop) == 1)
  stopifnot(is.character(extramsg))
  stopifnot(length(extramsg) == 1)


  if (any(var <= min.hard | var >= max.hard, na.rm = TRUE)) {
    msg <- sprintf("%s(s) <= %0.1f or >= %0.1f %s found. %s",
                   var.name, min.hard, max.hard, var.unit, extramsg)
    if (do.stop)
      stop(msg)
    else {
      warning(msg)
      return()
    }
  }

  if (do.warn && any(var <= min | var >= max, na.rm = TRUE))
    warning(sprintf("%s(s) < %0.1f or > %0.1f %s found. %s",
                    var.name, min, max, var.unit, extramsg))

}
