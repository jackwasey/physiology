#' Estimate volume inside an endotracheal tube
#'
#' Calculations are based on standard endotracheal tubes. The volume is
#' estimated as the cylinder of the given diameter and typical length of a tube
#' of given size.
#' @param diameter_mm The internal diameter of the endotracheal tube in
#'   millimeters. Sizes between 2.0 and 8 are offered. Half sizes between 2.5
#'   and 6 are accepted.
#' @examples
#' ett_vol_mm3(2:8)
#' plot(ett_vol_mm3(2:8))
#' lines(ett_vol_mm3(2:8))
#' ett_vol_mm3(seq(2, 6, 0.5))
#' @return Volumes of each given ETT in cubic millimeters
ett_vol_mm3 <- function(diameter_mm) {
  lookup_diam_mm <- c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 7, 8)
  lookup_len_cm <- c(14, 15, 17, 19, 21, 23, 25, 28, 29, 31, 33)
  stopifnot(diameter_mm %in% lookup_diam_mm)
  len_mm <- 10 * lookup_len_cm[match(diameter_mm, lookup_diam_mm)]
  len_mm * pi * (diameter_mm/2)^2
}
