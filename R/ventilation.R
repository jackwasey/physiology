#' Estimate ventilatory dead space
#' @param height_m Height in m
#' @param age_y Age in years, optional for estimating ETT and HME sizes
#'   automatically
#' @param gender_f Logical: female if \code{TRUE}. Default is not to specify
#'   gender.
#' @param elbow_ml Numeric: volume of elbow of breathing circuit in ml
#' @param humidifier_ml Numeric: volume of humidifier of breathing circuit in ml
#' @param ett_diameter_mm Numeric: internal diameter of endotrachel tube.
#'   Default is \code{NULL} which would estimate this from the age of patient
#' @export
deadspace_total <- function(height_m, age_y = NULL, gender = NULL, elbow_ml = 10, humidifier_ml = 7, ett_diameter_mm = NULL) {

}

#' @describeIn deadspace_total Estimate anatomic deadspace
#' @references
#'   http://www.atsjournals.org/doi/abs/10.1164/arrd.1971.104.2.215
#'   http://rc.rcjournal.com/content/53/7/885.short
#'   https://www.ncbi.nlm.nih.gov/pubmed/8727530
#' @return estimate of anatomic deadspace in ml
deadspace_anatomic <- function(height_m = NULL, weight_kg = NULL, age_y = NULL) {
  if (is.null(height_m) && is.null(weight) && is.null(age)) return(125)

  if (!is.null(height_m)) {
    if (!is.null(weight_kg)) {
      warning("Both height and weight given. Using ideal weight.")
      weight_kg = ideal_weight(height_m, age_y)
    }

  } else {

  }

  warning("Both height and weight given. Using ideal weight.")

  # TODO actually, the formula gives an age of exp((3.28-2.2)/0.56)-1 = 5.88 y
  # where the formula asymptotes to the adult value of 2.2. This should be the
  # cut off.

  if (!is.null(age_y) && age_y <= 6) {
    if (is.null(weight_kg))
      stop("For infants and children under six years, weight is needed to calculate anatomic dead space")
    # use Numa et al, 1985 formula
    return(3.28 - 0.56 * log(1 + age_y))
  }

  # for age > 6, use ideal weight if height given
  if (!is.null(age_y) && age_y > 6) {
    if (age_y >= 18)
      weight_kg <- ideal_weight_adult(height_m = height_m)
    else
      weight_kg <- ideal_weight_child(height_m = height_m, age.years = age_y)
  }

  return(2.2 * weight_kg)

}

deadspace_intrathoracic <- function(height_m = NULL, weight_kg = NULL) {

}

deadspace_extrathoracic <- function(height_m = NULL, weight_kg = NULL, age_y = NULL) {

}

deadspace_physiologic <- function(height_m) {

}
