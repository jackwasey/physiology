#' @title Estimate Blood Volume
#' @rdname bloodvol
#' @description estimate blood volume according to the classic 1960s paper by
#'   Nadler. Surgery. 1962 Feb;51(2):224-32. Prediction of blood volume in
#'   normal human adults. Nadler SB, Hidalgo JH, Bloch T.
#' @inheritParams ideal_weight_adult
#' @template weight_kg
#' @param ... passed on to validation
#' @examples
#' blood_vol_Nadler(1.8, 80, male = TRUE)
#' blood_vol_Nadler(1.8, 160, male = TRUE)
#' blood_vol_Nadler(1.8, 80, male = FALSE)
#' @export
blood_vol_Nadler <- function(height_m, weight_kg, male, ...) {
  valid_height(height_m, ...)
  valid_weight(weight_kg, ...)
  stopifnot(is.logical(male))
  (0.3669 - (0.3669 - 0.3561) * !male) * height_m ^ 3 +
    (0.03219 - (0.03219 - 0.03308) * !male) * weight_kg +
    (0.6041 - (0.6041 - 0.1833) * !male)
}

#' @describeIn bloodvol Blood volume by Lemmens et al, 2006. This effectively
#'   reverses engineers an ideal weight from BMI of 22, then use the square root
#'   of its ratio to actual body weight to adjust the 70ml per kg of an ideal
#'   weight person. Age-dependent regression equations for indexed blood volume
#'   `InBV` at ideal body weight. (No adjustment made in obesity by Lemmens.)
#'   `InBV = 90-0.4 X age` (males) `InBV = 85-0.4 X age` (females).
#' @examples
#' blood_vol_Lemmens_sedentary(1.8, 80)
#' blood_vol_Lemmens_sedentary(1.8, 160)
#' @export
blood_vol_Lemmens_sedentary <- function(height_m, weight_kg, ...) {
  weight_kg * blood_vol_Lemmens_indexed(height_m, weight_kg, ...)
}

#' @describeIn bloodvol Lemmens method, giving a volume per kilogram
#' @examples
#' blood_vol_Lemmens_indexed(1.8, 80)
#' blood_vol_Lemmens_indexed(1.8, 160)
#' @export
blood_vol_Lemmens_indexed <- function(height_m, weight_kg, ...) {
  stopifnot(length(height_m) == length(weight_kg))
  valid_height_adult(height_m, ...)
  valid_weight_adult(weight_kg, ...)
  70 / sqrt(weight_kg / (22 * height_m ^ 2))
}

#' @describeIn bloodvol Blood volume for non-obese adult, per Lemmens, applies
#'   to slim adults, but note that the age-related decline is not seen if high
#'   degree of physical activity is maintained. TODO: check BMI not elevated
#' @references 'Davy KP, Seals DR. Total blood volume in healthy young and older
#'   men. J Appl Physiol 1994; 76: 2059-62'
#'
#'   'Parker-Jones P, Davy KP, DeSouza CA et al. Absence of age-related decline
#'   in total blood volume in physically active females. Am J Physiol 1997; 272:
#'   H2534-40'
#' @param male logical
#' @examples
#'   blood_vol_Lemmens_non_obese(80, age_y = 25, male = TRUE)
#'   blood_vol_Lemmens_non_obese(80, age_y = 75, male = TRUE)
#' @export
blood_vol_Lemmens_non_obese <- function(weight_kg, age_y, male, ...) {
  stopifnot(length(weight_kg) == length(age_y))
  stopifnot(length(male) == length(age_y))
  valid_weight_adult(weight_kg, ...)
  valid_age_adult(age_y, ...)
  stopifnot(is.logical(male))
  ifelse(male, # TODO: does this vectorize?
         weight_kg * (90 - (0.4 * age_y)),
         weight_kg * (85 - (0.4 * age_y))
  )
}

#' @describeIn bloodvol Crude estimation based on textbook values for blood
#'   volumes per kilogram from prematurity through childhood. Unsatisfying
#'   having step changes. Not exporting this function until better. Need to
#'   reference an article like https://www.ncbi.nlm.nih.gov/pubmed/17302766 The
#'   `_indexed` function gives a per kilogram result.
#' @param premature logical value, indicating prematurity. Only used if also an
#'   infant, although extremely premature infants could easily have different
#'   blood volumes per weight than full-term infants several months after birth.
#' @keywords internal
blood_vol_peds_indexed <- function(age_y, premature = FALSE, ...) {
  valid_age(age_y = age_y)
  if (age_y <= 1/13) {
    if (premature)
      return(90)
    else
      return(85)
  }
  if (premature)
    message("prematurity flag set, but not used for age > 4wks")
  if (age_y < 1) {
    return(80)
  }
  # so crude...
  70
}

#' @describeIn bloodvol Give blood volume in ml, not ml/kg.
#' @keywords internal
blood_vol_peds <- function(weight_kg, age_y, ...) {
  weight_kg * blood_vol_peds_indexed(age_y = age_y, ...)
}
