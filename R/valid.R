#' Validate physiologic input parameters
#'
#' User may generate warnings for unreasonable or obviously erroneous heights.
#' @template height_m
#' @param ht_min minimum height below which to warn if \code{warn = TRUE}
#' @param ht_max maximum height above which to warn if \code{warn = TRUE}
#' @param ht_min_hard minimum height below which to warn regardless of
#'   \code{warn}
#' @param ht_max_hard maximum height above which to warn if \code{warn}
#' @param do_warn single logical, if TRUE, will give warnings outside of soft
#'   limits
#' @param do_stop single logical, \code{stop} instead of warning if any values
#'   outside hard limits
#' @param extra_msg single character string with additional message to append,
#'   default is ""
#' @param equal_ok logical, if true, then being equal to a limit does not
#'   trigger a warning or error
#' @export
valid_height <- function(height_m, ht_min = 0.1, ht_max = 2.5,
                         ht_min_hard = 0.001, ht_max_hard = 3,
                         extra_msg = "", do_warn = TRUE, do_stop = FALSE,
                         equal_ok = FALSE) {
  # TODO: use age to validate further
  valid(height_m, "height", "m",
        ht_min, ht_max, ht_min_hard, ht_max_hard,
        extra_msg, do_warn, do_stop, equal_ok = equal_ok)
}

#' @rdname valid_height
#' @export
valid_height_adult <- function(height_m, ht_min = 0.5, ht_max = 2.5,
                               ht_min_hard = 0.001, ht_max_hard = 3,
                               extra_msg = "", do_warn = TRUE, do_stop = FALSE,
                               equal_ok = FALSE) {
  valid(height_m, "height", "m",
        ht_min, ht_max, ht_min_hard, ht_max_hard,
        extra_msg, do_warn, do_stop, equal_ok = equal_ok)
}

#' @rdname valid_height
#' @template weight_kg
#' @param wt_min minimum height below which to warn if \code{warn = TRUE}
#' @param wt_max maximum height above which to warn if \code{warn = TRUE}
#' @param wt_min_hard minimum height below which to warn regardless of
#'   \code{warn}
#' @param wt_max_hard maximum height above which to warn if \code{warn}
#' @export
valid_weight <- function(weight_kg, wt_min = 0.1, wt_max = 300,
                         wt_min_hard = 0, wt_max_hard = 600,
                         extra_msg = "", do_warn = TRUE, do_stop = FALSE,
                         equal_ok = FALSE) {
  valid(weight_kg, "weight", "kg",
        wt_min, wt_max, wt_min_hard, wt_max_hard,
        extra_msg, do_warn, do_stop, equal_ok = equal_ok)
}

#' @rdname valid_height
#' @export
valid_weight_adult <- function(weight_kg, wt_min = 5, wt_max = 300,
                               wt_min_hard = 0, wt_max_hard = 600,
                               extra_msg = "", do_warn = TRUE, do_stop = FALSE,
                               equal_ok = FALSE) {
  valid(weight_kg, "weight", "kg",
        wt_min, wt_max, wt_min_hard, wt_max_hard,
        extra_msg, do_warn, do_stop, equal_ok = equal_ok)
}

#' @rdname valid_height
#' @param age_y numeric years
#' @param age_min minimum age below which to warn if \code{warn = TRUE}
#' @param age_max maximum age above which to warn if \code{warn = TRUE}
#' @param age_min_hard minimum age below which to warn regardless of \code{warn}
#' @param age_max_hard maximum age above which to warn if \code{warn}
#' @export
valid_age <- function(age_y, age_min = 0, age_max = 150,
                      age_min_hard = 0.00001, age_max_hard = 150,
                      extra_msg = "", do_warn = TRUE, do_stop = FALSE,
                      equal_ok = FALSE) {
  valid(age_y, "age", "yr",
        age_min, age_max, age_min_hard, age_max_hard,
        extra_msg, do_warn, do_stop, equal_ok = equal_ok)
}

#' @rdname valid_height
#' @export
valid_age_adult <- function(age_y, age_min = 18, age_max = 150,
                            age_min_hard = 17, age_max_hard = 150,
                            extra_msg = "", do_warn = TRUE, do_stop = FALSE,
                            equal_ok = FALSE)
  valid(age_y, "age", "yr",
        age_min, age_max, age_min_hard, age_max_hard,
        extra_msg, do_warn, do_stop, equal_ok = equal_ok)

#' @rdname valid_height
#' @param scr_uM numeric serum creatinine (umol/L)
#' @param scr_min minimum serum creatinine below which to warn if \code{warn =
#'   TRUE}
#' @param scr_max maximum serum creatinine above which to warn if \code{warn =
#'   TRUE}
#' @param scr_min_hard minimum serum creatinine below which to warn regardless
#'   of \code{warn}
#' @param scr_max_hard maximum serum creatinine above which to warn if
#'   \code{warn}
#' @export
valid_creatinine <- function(scr_uM, scr_min = 8, scr_max = 1000,
                             scr_min_hard=0, scr_max_hard = 4000,
                             extra_msg = "", do_warn = TRUE, do_stop = FALSE,
                             equal_ok = FALSE) {
  # Note that scr_min was selected to help ensure that unit conversion errors
  # did not occur where scr was not provided in mg/dL which typically ranges
  # below 10 (acute renal failure which would ba a typical maximum is typically
  # defined as scr >= 4 mg/dL).
  valid(scr_uM, "serum creatinine", "umol/L",
        scr_min, scr_max, scr_min_hard, scr_max_hard,
        extra_msg, do_warn, do_stop, equal_ok = equal_ok)
}

valid <- function(var, name, unit, min, max, min_hard, max_hard,
                  extra_msg = "", do_warn = TRUE, do_stop = FALSE,
                  equal_ok = TRUE) {
  stopifnot(is.numeric(var))
  stopifnot(is.numeric(min))
  stopifnot(is.numeric(max))
  stopifnot(is.numeric(min_hard))
  stopifnot(is.numeric(max_hard))
  stopifnot(length(min) == 1)
  stopifnot(length(max) == 1)
  stopifnot(length(min_hard) == 1)
  stopifnot(length(max_hard) == 1)
  stopifnot(is.logical(do_warn))
  stopifnot(is.logical(do_stop))
  stopifnot(length(do_warn) == 1)
  stopifnot(length(do_stop) == 1)
  stopifnot(is.character(extra_msg))
  stopifnot(length(extra_msg) == 1)

  eq_hard <- any(var == min_hard | var == max_hard, na.rm = TRUE)
  df_hard <- any(var < min_hard | var > max_hard, na.rm = TRUE)
  if (df_hard || (eq_hard && !equal_ok)) {
    msg <- sprintf("%s(s) < %0.1f or > %0.1f %s found. %s",
                   name, min_hard, max_hard, unit, extra_msg)
    if (do_stop) {
      stop(msg)
    } else {
      warning(msg)
      return()
    }
  }
  if (!do_warn) return()
  eq_soft <- any(var == min | var == max, na.rm = TRUE)
  df_soft <- any(var < min | var > max, na.rm = TRUE)
  if (df_soft || (eq_soft && !equal_ok))
    warning(sprintf("%s(s) < %0.1f or > %0.1f %s found. %s",
                    name, min, max, unit, extra_msg))
}
