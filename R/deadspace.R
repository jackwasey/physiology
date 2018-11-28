#' Estimate ventilation dead-space
#'
#' @param ideal_weight_kg Ideal weight in kilograms. May be calculated using
#'   \code{\link{ideal_weight_adult}} or \code{\link{ideal_weight_child}}
#' @param age_y Age in years, optional for estimating ETT and HME sizes
#'   automatically
#' @param elbow_ml Numeric volume of elbow of breathing circuit in ml
#' @param humidifier_ml Numeric volume of humidifier of breathing circuit in ml
#' @param ett_diameter_mm Numeric internal diameter of endotracheal tube.
#'   Default is \code{NULL} which would estimate this from the age of patient
#' @examples
#'   height <- seq(1, 2, 0.05)
#'   male <- rep(FALSE, length(height))
#'   iw <- ideal_weight_adult(height_m = height, male = male)
#'   \dontrun{
#'   plot(iw, deadspace_anatomic_adult(ideal_weight_kg = height))
#'   }
#'
#'   # discontinuity at age 6 is driven by ideal weight more than the
#'   # lograithmic calculation
#'   iw <- c(seq(12, 18, 0.2), seq(18.5, 24, 0.5))
#'   youngest = 3
#'   oldest = 9
#'   ages <- seq(youngest, oldest, (oldest - youngest) / (length(iw) - 1))
#'   \dontrun{
#'   plot(iw, deadspace_anatomic_child(ideal_weight_kg = iw, age_y = ages),
#'        type = "l")
#'   }
#' @concept deadspace
#' @concept dead-space
#' @family respiratory
#' @family airway equipment
#' @export
deadspace_total <- function(ideal_weight_kg,
                            age_y = NULL,
                            elbow_ml = 10,
                            humidifier_ml = 7,
                            ett_diameter_mm = NULL) {

}

#' @describeIn deadspace_total Estimate anatomic dead-space
#' @references
#'   \url{http://www.atsjournals.org/doi/abs/10.1164/arrd.1971.104.2.215}
#'   \url{http://rc.rcjournal.com/content/53/7/885.short}
#'   \url{https://www.ncbi.nlm.nih.gov/pubmed/8727530}
#' @return estimate of anatomic dead-space in ml
#' @export
deadspace_anatomic <- function(ideal_weight_kg, age_y = NULL) {
  if (is.null(age_y) || age_y >= 18)
    deadspace_anatomic_adult(ideal_weight_kg = ideal_weight_kg)
  else
    deadspace_anatomic_child(ideal_weight_kg = ideal_weight_kg, age_y = age_y)
}

#' @describeIn deadspace_total Estimate anatomic dead-space in an adult
#' @export
deadspace_anatomic_adult <- function(ideal_weight_kg = NULL) {
  if (is.null(ideal_weight_kg))
    125
  else
    2.2 * ideal_weight_kg
}

#' @describeIn deadspace_total Estimate anatomic dead-space in an infant or
#'   child
#' @export
deadspace_anatomic_child <- function(ideal_weight_kg, age_y = NULL) {
  out <- numeric(length(ideal_weight_kg))
  a <- deadspace_anatomic_adult(ideal_weight_kg = ideal_weight_kg)
  if (!is.null(age_y)) {
    old <- age_y >= 6
    stopifnot(length(age_y) == length(ideal_weight_kg))
    b <- ideal_weight_kg * (3.28 - 0.56 * log(1 + age_y))
    out[old] <- a[old]
    out[!old] <- b[!old]
    return(out)
  }
  warning("Returning adult anatomic deadspace estimate because age not given")
  a
}

#' @describeIn deadspace_total intrathoracic component of dead-space is age
#'   independent
#' @details 'Mean intrathoracic anatomic dead space was 1.03 ml/kg and was not
#'   related to age.' Numa, 1985
deadspace_intrathoracic_ml <- function(ideal_weight_kg)
  1.03 * ideal_weight_kg

deadspace_extrathoracic <- function(ideal_weight_kg = NULL, age_y = NULL) {
  .NotYetImplemented()
}

deadspace_physiologic <- function(ideal_weight_kg) {
  .NotYetImplemented()
}

#' Calculate equipment deadspace in ventilator breathing circuit
#'
#' There are minor brand variations between these airway devices. For the
#' purposes of rough physiologic calculations, this function gives values based
#' on real, widely-used equipment.
#' @param humidifier Single value, if \code{TRUE}, the default, then we assume
#'   the adult humidifier. If \code{humidifier} is set to the character string
#'   \code{"adult"}, the results is the same as for \code{TRUE}. Alternatively,
#'   \code{"infant"} refers to the lower volume, higher resistance device.
#' @param elbow Single logical value, default is \code{TRUE}
#' @param flexible Single logical value or character string. If \code{FALSE},
#'   the default, no additional flexible tubing is added. If \code{TRUE}, the
#'   volume of typical extended flexible tubing is added. If \code{"compressed"}
#'   or \code{"extended"} are given, the volume of flexible tubing in the given
#'   state is used.
#' @param min numeric, giving the minimum number of obligatory milliliters of
#'   deadspace. The default is zero to allow calculation of additional airway
#'   elements.
#' @examples
#' deadspace_equipment_ml()
#' deadspace_equipment_ml(humidifier = FALSE)
#' deadspace_equipment_ml(humidifier = "infant", elbow = TRUE)
#' deadspace_equipment_ml(flexible = "extended", elbow = FALSE)
#' deadspace_equipment_ml(flexible = "extended", elbow = TRUE)
#' @seealso \code{\link{deadspace_things_ml}}
#' @export
deadspace_equipment_ml <-
  function(humidifier = c("adult", "infant", "none"),
           elbow = TRUE,
           flexible = c("none", "compressed", "extended"),
           min = 0) {
    out <- 0
    if (is.logical(humidifier))
      out <- out + ifelse(humidifier,
                          physiology::deadspace_things_ml$humidifier_adult, 0)
    else
      out <- out + switch(
        match.arg(humidifier),
        "adult" = physiology::deadspace_things_ml$humidifier_adult,
        "infant" = physiology::deadspace_things_ml$humidifier_infant,
        "none" = 0)
    out <- out + ifelse(elbow, physiology::deadspace_things_ml$elbow, 0)
    if (is.logical(flexible))
      out <- out +
      ifelse(flexible, physiology::deadspace_things_ml$flexible_adult, 0)
    else
      out <- switch(
        match.arg(flexible),
        "none" = 0,
        "compressed" = physiology::deadspace_things_ml$flexible_compressed,
        "extended" = physiology::deadspace_things_ml$flexible_extended) + out
    out
  }
