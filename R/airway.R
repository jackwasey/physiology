#' Estimate volume inside an endotracheal tube
#'
#' Calculations are based on standard endotracheal tubes. The volume is
#' estimated as the cylinder of the given diameter and typical length of a tube
#' of given size.
#' @param diameter_mm The internal diameter of the endotracheal tube in
#'   millimeters. Sizes between 2.0 and 8 are offered. Half sizes between 2.5
#'   and 6 are accepted.
#' @examples
#' ett_vol_ml(2:8)
#' plot(2:8, ett_vol_ml(2:8))
#' lines(2:8, ett_vol_ml(2:8),
#'   xlab = "ETT internal diameter, mm",
#'   ylab = "ETT internal volume, mm^3")
#' (vols_cm3 <- ett_vol_ml(seq(2, 6, 0.5)) / 1000)
#'
#' # Ages through to ETT internal volume
#' ett_vol_ml(ett_size_by_age(1:10))
#'
#' @return Volumes of each given ETT in cubic millimeters
#' @family airway equipment
#' @export
ett_vol_ml <- function(diameter_mm) {
  lookup_diam_mm <- c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 7, 8)
  lookup_len_cm <- c(14, 15, 17, 19, 21, 23, 25, 28, 29, 31, 33)
  stopifnot(diameter_mm %in% lookup_diam_mm)
  len_mm <- 10 * lookup_len_cm[match(diameter_mm, lookup_diam_mm)]
  (len_mm * pi * (diameter_mm / 2) ^ 2) / 1000
}
#' Estimate appropriate size of endotracheal tube for infants and children
#'
#' \code{ett_size_cole} uses the classic Cole formula for uncuffed tubes,
#' Motoyama formula for cuffed tubes with age over two years, and the Khine
#' formula for cuffed tubes with age under two years. All of these, as any
#' anesthesiologist will tell, give poor estimates at any extreme of age, or
#' height. These formulae are for use in pediatric practice only.
#' @param age_y numeric vector
#' @param cuffed logical vector, single value, or a vector of the same length as
#'   the given age vector, defining whether the desired tube is to be cuffed. If
#'   not specified, then it is assumed that a cuffed tube is used.
#' @return ETT size, internal diameter in mm, rounded to nearest half-mm size up
#'   to 6mm, then the nearest integer.
#' @examples
#' teenagers <- ett_size_by_age(13:18)
#' plot(13:18, teenagers,
#'   main = "This formula for ETT tube size overestimates tube
#'   sizes of teenagers, and takes no account of gender")
#' lines(13:18, teenagers)
#' ages <- c(1/12, 1, 2, 4, 7, 11)
#' neonate_to_child <- ett_size_by_age(age = ages)
#' names(neonate_to_child) <- ages
#' print(neonate_to_child)
#' plot(ages, neonate_to_child)
#' lines(ages, neonate_to_child)
#' plot(ages, neonate_to_child, log = "x")
#' lines(ages, neonate_to_child)
#' @references
#' \url{http://anesthesiology.pubs.asahq.org/Article.aspx?articleid=1933172}
#' @family airway equipment
#' @export
ett_size_by_age <- function(age_y, cuffed = TRUE) {
  cole <- 4 + age_y / 4
  motoyama <- 3.5 + age_y / 4
  khine <- 3 + age_y / 4
  out <- cole
  if (!is.null(cuffed)) {
    out[cuffed & age_y >= 2] <- motoyama[cuffed & age_y >= 2]
    out[cuffed & age_y < 2] <- khine[cuffed & age_y < 2]
  }
  out[out < 6] <- round(out[out < 6] * 2) / 2
  out[out >= 6] <- round(out[out >= 6])
  out
}
