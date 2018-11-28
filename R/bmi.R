#' @title ideal weight for adults
#' @description \code{ideal_weight_adult} gives the ideal weight using default
#'   adult algorithm, Devine. If an age is specified and less than 18 years, the
#'   Traub function will be used.
#' @template height_m
#' @template dots
#' @param age_y numeric vector, age(s) in years. Extremely exact age is not
#'   required, so for age in days or months, simplest just to divide. This is
#'   not used in the calculation itself, so may be missing.
#' @template male
#' @rdname ideal_weight
#' @examples
#' ideal_weight_adult(1.7, male = TRUE)
#' ideal_weight_adult(1.7, male = FALSE)
#' ideal_weight_adult(6 * 12 * 2.54 / 100, male = TRUE) # 6ft
#' suppressWarnings(ideal_weight_adult(5, male = FALSE))
#' @concept BMI
#' @family body mass index
#' @family ideal weight
#' @export
ideal_weight <- function(height_m, ..., age_y = NULL, male = NULL) {
  if (is.null(age_y) || age_y > 18)
    ideal_weight_adult(height_m = height_m, male = male, ...)
  else
    ideal_weight_child(height_m = height_m, age_y = age_y, ...)
}

#' @describeIn ideal_weight Ideal weight of an adult
#' @export
ideal_weight_adult <- function(height_m, male, ...)
  ideal_weight_Devine(height_m, male, ...)

#' @describeIn ideal_weight Ideal weight of a child, age >= 1 and age < 18 years
#' @export
ideal_weight_child <- function(height_m, age_y = NULL, ...)
  ideal_weight_Traub(height_m, age_y, ...)

#' ideal weight for child per Traub
#'
#' @description `2.396e0.01863(height)`, where height is in cm. There is an
#'   argument for using another package to capture durations, of which age is a
#'   special case. However, I am resisting bringing in external dependencies,
#'   and for almost all use-cases I can imagine, the age will be captured as a
#'   single number of one type, not a mix of types. Note that gender does not
#'   appear to be important in this relationship.
#'
#'   See package AGD for CDC growth chart data.
#' @inheritParams ideal_weight_child
#' @source \url{http://www.ncbi.nlm.nih.gov/pubmed/6823980}
#' @examples
#' # will warn if given age is not in validate range from publication:
#' \dontrun{
#'   ideal_weight_child(height_m = 0.5, age_y = 0, do_warn = TRUE)
#'   ideal_weight_child(0.8, age_y = 11 / 12, do_warn = TRUE)
#'   ideal_weight_child(0.5, age_y = 25/365, do_warn = TRUE)
#' }
#'   ideal_weight_child(0.5, age_y = 25 / 365, do_warn = FALSE)
#'   ideal_weight_child(1, age_y = 2)
#' @md
#' @export
ideal_weight_Traub <- function(height_m, age_y = NULL, ...) {
  if (!is.null(age_y))
      valid_age(age_y, age_min = 1, age_max = 18,
                age_min_hard = 0, age_max_hard = 150,
                extra_msg = "age < 1 year or age > 17 year \
                not validated from Traub formula",
                ...)
      valid_height(height_m, ...)
      2.396 * exp(1.863 * height_m)
}

#' @title ideal weight by Devine method
#' @description Devine method is the default and most widely used. Normally
#'   stated in inches. Male: 50kg + 2.3kg * inches over 5ft. Female: 45.5kg +
#'   2.3kg * inches over 5ft. (from 1974 genatamicin paper - see Lemmens for
#'   ref.)
#' @rdname ideal_weight
#' @export
ideal_weight_Devine <- function(height_m, male, ...)
  ideal_weight_linear(height_m, male, 60, 50, 45.5, 2.3, 2.3, ...)

#' @title ideal weight by Robinson method
#' @rdname ideal_weight
#' @export
#' @description Robinson's method for ideal weight: different linear
#'   relationship. (Robinson JD, Lupkiewicz SM, Palenik L et al. Determination
#'   of ideal body weight for drug dosage calculations. Am J Hosp Pharm 1983;
#'   40: 1016-9.)
ideal_weight_Robinson <- function(height_m, male, ...)
  ideal_weight_linear(height_m, male, 60, 52, 49, 1.9, 1.7, ...)


#' @title ideal weight by Miller
#' @export
#' @rdname ideal_weight
#' @description Miller's method for ideal weight: different linear relationship.
#'   (Miller DR, Carlson JD, Loyd BJ et al. Determining ideal body weight.
#'   (Letter). Am J Hosp Pharm 1983; 40: 1622.)
ideal_weight_Miller <- function(height_m, male, ...)
  ideal_weight_linear(height_m, male, 60, 56.2, 53.1, 1.41, 1.36, ...)

#' @title ideal weight by Broca
#' @description Calculate ideal weight based on Broca (1871) Height in cm -100
#'   for women, -105 for men Broca PP. Memoires d'anthropologie. Paris 1871 /
#'   1877.
#' @rdname ideal_weight
#' @export
ideal_weight_Broca <- function(height_m, male, ...)
  ideal_weight_linear(height_m, male, 0, -100, -105, 2.54, 2.54, ...)

#' @title ideal weight by Lemmens
#' @description Lemmens method assumes BMI 22 as ideal (Obesity Surgery 2005)
#' @rdname ideal_weight
#' @export
ideal_weight_Lemmens <- function(height_m, ...) {
  valid_height(height_m, ...)
  22 * height_m ^ 2
}

#' @title ideal weight by gender, offset and gradient
#' @description generic internal function to handle linear ideal weight
#'   calculations. Unfortunately mixes inches and meters at present.
#' @param height_mininch height above which to start scaling; consider as a
#'   minimum (for at least some algorithms)
#' @param male_min_kg y intercept i.e. ideal weight at minimum height for males
#' @param female_min_kg y intercept i.e. ideal weight at minimum height for
#'   females
#' @param male_kg_per_inch slope for males
#' @param female_kg_per_inch slope for females
#' @param ... passed on to validation
#' @noRd
#' @keywords internal
ideal_weight_linear <- function(height_m, male,
                                height_mininch,
                                male_min_kg, female_min_kg,
                                male_kg_per_inch, female_kg_per_inch,
                                ...) {
  stopifnot(is.logical(male))
  stopifnot(length(height_m) == length(male))
  valid_height(height_m, ...)

  heightinch <- height_m * 100 / 2.54

  f2mintercept <- male_min_kg - female_min_kg
  f2mgradient <- male_kg_per_inch - female_kg_per_inch

  female_min_kg + f2mintercept * male +
    (heightinch - height_mininch) * (female_kg_per_inch + f2mgradient * male)
}

#' @title Estimate Blood Volume
#' @rdname bloodvol
#' @description estimate blood volume according to the classic 1960s paper by
#'   Nadler. Surgery. 1962 Feb;51(2):224-32. Prediction of blood volume in
#'   normal human adults. Nadler SB, Hidalgo JH, Bloch T.
#' @inheritParams ideal_weight_adult
#' @template weight_kg
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

#' @title Blood volume by Lemmens et al, 2006
#' @rdname bloodvol
#' @description This effectively reverses engineers an ideal weight from BMI of
#'   22, then use the square root of its ratio to actual body weight to adjust
#'   the 70ml per kg of an ideal weight person. Age-dependent regression
#'   equations for indexed blood volume `InBV` at ideal body weight. (No
#'   adjustment made in obesity by Lemmens.) `InBV = 90-0.4 X age` (males) `InBV
#'   = 85-0.4 X age` (females).
#' @return numeric vector
#' @examples
#' blood_vol_Lemmens_sedentary(1.8, 80)
#' blood_vol_Lemmens_sedentary(1.8, 160)
#' @md
#' @export
blood_vol_Lemmens_sedentary <- function(height_m, weight_kg, ...) {
  weight_kg * blood_vol_Lemmens_indexed(height_m, weight_kg, ...)
}

#' @rdname bloodvol
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

#' @title Blood volume for non-obese adult, per Lemmens
#' @rdname bloodvol
#' @description applies to slim adults, but note that the age-related decline is
#'   not seen if high degree of physical activity is maintained. TODO: check BMI
#'   not elevated
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
  ifelse(male,
         weight_kg * (90 - (0.4 * age_y)),
         weight_kg * (85 - (0.4 * age_y))
  )
}

#' @title adjusted body weight
#' @description returns ideal weight + 40% of difference between ideal and
#'   actual weights. Ideal weight is calculated using default algorithm. TODO:
#'   is downward adjustment valid?
#' @inheritParams ideal_weight_adult
#' @param weight_kg weight in kg, may be a vector
#' @param ... passed to validation
#' @examples
#' adj_weight_adult(1.6, 120, male = FALSE)
#' @export
adj_weight_adult <- function(height_m, weight_kg, male, ...) {
  stopifnot(length(height_m) == length(weight_kg))
  stopifnot(length(male) == length(weight_kg))
  valid_height_adult(height_m, ...)
  valid_weight_adult(weight_kg, ...)
  stopifnot(is.logical(male))
  #TODO: is downward adjustment valid?
  0.6 * ideal_weight_adult(height_m, male) + 0.4 * weight_kg
}

#' Body Mass Index (BMI) for adults
#'
#' Calculate body mass index using weight in kg / (height in meters ^ 2)
#' @rdname bmi
#' @template height_m
#' @template weight_kg
#' @param ... passed to validation
#' @examples
#' bmi_adult(1.6, 120)
#' bmi_adult(2, 75)
#' @export
bmi_adult <- function(height_m, weight_kg, ...) {
  stopifnot(length(height_m) == length(weight_kg))
  valid_height_adult(height_m, ...)
  valid_weight_adult(weight_kg, ...)
  weight_kg / (height_m ^ 2)
}

#' @rdname bmi
#' @param heightin height in inches
#' @param weightlb weight in pounds
#' @examples
#' bmi_adult_ins_lbs(72, 200)
#' @export
bmi_adult_ins_lbs <- function(heightin, weightlb, ...) {
  bmi_adult(heightin * 0.0254, weightlb / 2.2046224, ...)
}
