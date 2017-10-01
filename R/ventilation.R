#' Estimate ventilatory dead space
#' @param height_m Height in m
#' @param age_y Age in years, optional for estimating ETT and HME sizes
#'   automatically
#' @param male Logical. Default is \code{NULL}
#' @param elbow_ml Numeric: volume of elbow of breathing circuit in ml
#' @param humidifier_ml Numeric: volume of humidifier of breathing circuit in ml
#' @param ett_diameter_mm Numeric: internal diameter of endotrachel tube.
#'   Default is \code{NULL} which would estimate this from the age of patient
#' @examples
#'   height <- seq(1, 2, 0.05)
#'   male <- rep(FALSE, length(height))
#'   iw <- ideal_weight_adult(height_m = height, male = male)
#'   plot(x, deadspace_anatomic_adult(ideal_weight_kg = height))
#'
#'   # discontinuity at age 6 is driven by ideal weight more than the lograithmic calculation
#'   iw <- c(seq(12, 18, 0.2), seq(18.5, 24, 0.5))
#'   youngest = 3
#'   oldest = 9
#'   ages <- seq(youngest, oldest, (oldest - youngest) / (length(iw) - 1))
#'   plot(iw, deadspace_anatomic_child(ideal_weight_kg = iw, age_y = ages), type = "l")
#'
#' @export
deadspace_total <- function(height_m, age_y = NULL, male = NULL, elbow_ml = 10, humidifier_ml = 7, ett_diameter_mm = NULL) {

}

#' @describeIn deadspace_total Estimate anatomic deadspace
#' @references
#'   http://www.atsjournals.org/doi/abs/10.1164/arrd.1971.104.2.215
#'   http://rc.rcjournal.com/content/53/7/885.short
#'   https://www.ncbi.nlm.nih.gov/pubmed/8727530
#' @return estimate of anatomic deadspace in ml
deadspace_anatomic <- function(ideal_weight_kg, age_y = NULL, ...) {

  if (is.null(age_y) || age_y >= 18)
    deadspace_anatomic_adult(ideal_weight_kg = ideal_weight_kg, ...)
  else
    deadspace_anatomic_child(ideal_weight_kg = ideal_weight_kg, age_y = age_y)
}

#' @describeIn deadspace_total Estimate anatomic deadspace in an adult
#' @export
deadspace_anatomic_adult <- function(ideal_weight_kg = NULL) {
  if (is.null(ideal_weight_kg))
    125
  else
    2.2 * ideal_weight_kg
}

#' @describeIn deadspace_total Estimate anatomic deadspace in an infant or child
#' @export
deadspace_anatomic_child <- function(ideal_weight_kg, age_y = NULL) {

  if (is.null(age_y) || age_y >= 6)
    deadspace_anatomic_adult(ideal_weight_kg = ideal_weight_kg)
  else
    ideal_weight_kg * (3.28 - 0.56 * log(1 + age_y))
}

#' @describeIn deadsoace_total intrathoracic component of deadspace is age independent
#' @details "Mean intrathoracic anatomic dead space was 1.03 ml/kg and was not
#'   related to age." Numa, 1985
deadspace_intrathoracic_ml <- function(ideal_weight_kg)
  1.03 * ideal_weight_kg


deadspace_extrathoracic <- function(ideal_weight_kg = NULL, age_y = NULL) {

}

deadspace_physiologic <- function(ideal_weight_kg) {

}
