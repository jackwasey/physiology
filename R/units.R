#' French to diameter
#'
#' Convert French size of a catheter to diameter in mm. Currently accepts or
#' returns non-integer French values
#'
#' @param x Size in French units, or mm
#' @family unit conversions
#' @export
french_to_diameter_mm <- function(x) {
  stopifnot(x >= 0)
  x * 3
}

#' @rdname french_to_diameter_mm
diameter_mm_to_french <- function(x) {
  stopifnot(x >= 0)
  x / 3
}
