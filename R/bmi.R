#' @title Estimate body surface area of an adult
#' @description \code{bodySurfaceAreaAdult} Estimate body surface area of an
#'   adult using \code{sqrt(wt*ht)/6} TODO: reference for this.
#' @template heightm
#' @param weightkg
#' @return numeric vector
#' @export
bodySurfaceAreaAdult <- function(heightm, weightkg)
  sqrt(heightm * weightkg) / 6

#' @title ideal weight for adults
#' @description \code{idealWeight} gives the ideal weight using default adult
#'   algorithm, Devine. #TODO: param to switch to different method without
#'   knowing the function name.
#' @template heightm
#' @template male
#' @rdname idealWeight
#' @export
idealWeightAdult <- function(heightm, male)
  idealWeightDevine(heightm, male)

#' @title ideal weight for adults
#' @description \code{idealWeight} gives the ideal weight using default
#'   paediatric algorithm. TODO: account for Down's/other well calibrated
#'   developmental differences. Age specifications are mutually exclusive, and
#'   an error will be generated if more than on age i specified per patient.
#' @template heightm
#' @template male
#' @param ageYears numeric vector, age(s) in years
#' @param ageMonths numeric vector, age(s) in months
#' @param ageDays  numeric vector, age(s) in days
#' @rdname idealWeight
#' @export
idealWeightChild <- function(heightm, male, ageYears = NULL, ageMonths = NULL, ageDays = NULL) {
  stopifnot(xor(xor(isnull(ageYears), isnull(ageMonths)), isnull(ageDays)))
  stop("not implemented yet")
}

idealWeightChildStraub <- function(heightm, male, ageYears = NULL, ageMonths = NULL, ageDays = NULL) {
  stopifnot(xor(xor(isnull(ageYears), isnull(ageMonths)), isnull(ageDays)))
  if (any(!is.null(ageYears) & (ageYears < 1 | ageYears > 17)))
    warn("age < 1 year or age > 17 year not validated from Straub formula")
  # http://www.ncbi.nlm.nih.gov/pubmed/6823980
  # 2.396e0.01863(ht), where height is in cm
}


#' @title ideal weight by Devine method
#' @description Devine method is the default and most widely used. Normally
#'   stated in inches. Male: 50kg + 2.3kg * inches over 5ft. Female: 45.5kg +
#'   2.3kg * inches over 5ft. (from 1974 genatamicin paper - see Lemmens for
#'   ref.)
#' @inheritParams idealWeight
#' @rdname idealWeight
#' @export
idealWeightDevine <- function(heightm, male)
  idealWeightGenericLinear(heightm, male, 60, 50, 45.5, 2.3, 2.3)

#' @title ideal weight by Robinson method
#' @inheritParams idealWeight
#' @rdname idealWeight
#' @export
#' @description Robinson's method for ideal weight: different linear
#'   relationship. (Robinson JD, Lupkiewicz SM, Palenik L et al. Determination
#'   of ideal body weight for drug dosage calculations. Am J Hosp Pharm 1983;
#'   40: 1016-9.)
idealWeightRobinson <- function(heightm, male)
  idealWeightGenericLinear(heightm, male, 60, 52, 49, 1.9, 1.7)

#' @title ideal weight by Miller
#' @export
#' @inheritParams idealWeight
#' @rdname idealWeight
#' @description Miller's method for ideal weight: different linear relationship.
#'   (Miller DR, Carlson JD, Loyd BJ et al. Determining ideal body weight.
#'   (Letter). Am J Hosp Pharm 1983; 40: 1622.)
idealWeightMiller <- function(heightm, male)
  idealWeightGenericLinear(heightm, male, 60, 56.2, 53.1, 1.41, 1.36)

#' @title ideal weight by Broca
#' @description Calculate ideal weight based on Broca (1871) Height in cm -100
#'   for women, -105 for men Broca PP. Memoires d'anthropologie. Paris 1871 /
#'   1877.
#' @inheritParams idealWeight
#' @rdname idealWeight
#' @export
idealWeightBroca <- function(heightm, male)
  idealWeightGenericLinear(heightm, male, 0, -100, -105, 2.54, 2.54)

#' @title ideal weight by Lemmens
#' @description Lemmens merhod assumes BMI 22 as ideal (Obesity Surgery 2005)
#' @rdname idealWeight
#' @export
idealWeightLemmens <- function(heightm)
  22 * heightm ^ 2

#' @title ideal weight by gender, offset and gradient
#' @description generic internal function to handle linear ideal weight
#'   calculations. Unofrtunately mixes inches and meters at present.
#' @param heightmininch, height above which to start scaling; consider as a
#'   minimum (for at least some algorithms)
#' @param male_min_kg, y intercept i.e. ideal weight at minimum height for males
#' @param female_min_kg, y intercept i.e. ideal weight at minimum height for
#'   females
#' @param male_kg_per_inch, slope for males
#' @param female_kg_per_inch, slope for females
#' @rdname idealWeight
#' @keywords internal
idealWeightGenericLinear <- function(heightm, male,
                                     heightmininch,
                                     male_min_kg, female_min_kg,
                                     male_kg_per_inch, female_kg_per_inch) {
  #NA height or NA maleness are both allowed, and should give NA.

  if (length(heightm) != length(male))
    stop('height (%d) and sex (%d) vectors are different lengths',
         length(heightm), length(male))

  heightinch <- heightm*100/2.54

  f2mintercept = male_min_kg - female_min_kg
  f2mgradient = male_kg_per_inch - female_kg_per_inch

  # TODO: vectorize errors and result!
  if (any(heightinch < 0.75 * heightmininch, na.rm = TRUE))
    warning(sprintf('calculating ideal weight based on some very low height of %.2fm inches',
                    heightm[which(heightinch < 0.75 * heightmininch)]))

  if (any(heightinch < heightmininch, na.rm = TRUE))
    warning(sprintf('calculating ideal Weight based on some low height of %.3fm inches',
                    heightm[which(heightinch < heightmininch)]))
  if (any(heightinch > 9*12, na.rm = TRUE))
    warning('calculating idealWeight based on some very big heights of %.3fm inches',
            heightm[which(heightinch > 9 * 12)])
  if (any(heightinch > 8*12, na.rm = TRUE))
    warning('calculating idealWeight based on some big heights of %.3fm inches',
            heightm[which(heightinch > 8 * 12)])

  female_min_kg + f2mintercept*male +
    (heightinch - heightmininch) * (female_kg_per_inch + f2mgradient*male)
}

#' @title Nadler Blood Volume
#' @description estimate blood volume according to the classic 1960s paper by
#'   Nadler
#' @param weightkg weight in kilograms
#' @inheritParams idealWeight
#' @export
nadlerBloodVolume <- function(heightm, weightkg, male) {

  if (!is.numeric(heightm)) stop("NadlerBloodVolume requires numeric height input")
  if (any(is.na(heightm))) warning("NadlerBloodVolume requires non-NA height input")
  if (!is.numeric(weightkg)) stop("NadlerBloodVolume requires numeric weight input")
  if (any(is.na(weightkg))) warning("NadlerBloodVolume requires non-NA weight input")

  if (length(heightm) != length(weightkg) |
        length(male) != length(heightm)) {
    stop("length(heightm)=%d", length(heightm))
    stop("length(weightkg)=%d", length(weightkg))
    stop("length(male)=%d", length(male))
    stop("NadlerBloodVolume requires that the height weight and male vectors are all the same length.")
  }

  if (any(heightm <  0.1, na.rm = TRUE)) stop("NadlerBloodVolume: some heights are less than a 10cm!")
  if (any(heightm >  3,   na.rm = TRUE)) stop("NadlerBloodVolume: some heights are greater than 3m")
  if (any(weightkg < 0.1, na.rm = TRUE)) stop("NadlerBloodVolume: some weights are less than 100g")
  if (any(weightkg > 400, na.rm = TRUE)) stop("NadlerBloodVolume: some weights are greater than 400kg")

  nadler <- (0.3669-(0.3669-0.3561)*!male)*heightm^3 +
    (0.03219-(0.03219-0.03308)*!male)*weightkg +
    (0.6041-(0.6041-0.1833)*!male)
}

#' @title Blood volume by Lemmens et al, 2006
#' @description This effectively reverses engineers an ideal weight from BMI of
#'   22, then use the sqaure root of its ratio to actual body weight to adjust
#'   the 70ml/kg of an ideal weight person. Age-dependent regression equations
#'   for indexed blood volume (InBV) at ideal body weight. (No adjustment made
#'   in obesity by Lemmens.) InBV = 90-0.4 X age (males) InBV = 85-0.4 X age
#'   (females). Sounds like he is saying either they are slim and old or younger
#'   and obese. he doesn't attempt to integrate the formulae.
#' @param heightm height in meters
#' @param weightkg actual weight in kilograms
#' @param age years
#' @return numeric vector
#' @export
lemmensBloodVolumeSedentary <- function(heightm, weightkg)
  weightkg * lemmensIndexedBloodVolume(heightm, weightkg)

lemmensIndexedBloodVolume <- function(heightm, weightkg) {
  stopifnot(length(heightm) == length(weightkg))
  70 / sqrt( weightkg / (22 * heightm ^ 2))
}

#' @title blood volume estimate for near ideal weight
#' @description applies to slim adults, but note that the age-related decline is
#'   not seen if high degree of physical activity is maintained.
#'   TODO: check BMI not elevated
#' @details Davy KP, Seals DR. Total blood volume in healthy young and older
#'   men. J Appl Physiol 1994; 76: 2059-62.
#'
#'   Parker-Jones P, Davy KP, DeSouza CA et al. Absence of agerelated decline in
#'   total blood volume in physically active females. Am J Physiol 1997; 272:
#'   H2534-40.
#'   @param weightkg numeric weight in kilograms
#'   @param age years
#'   @param male logical
#' @export
lemmensBloodVolumeNonObese <- function(weightkg, age, male)
  ifelse(male,
         weightkg * ( 90 - (0.4 * age)),
         weightkg * ( 85 - (0.4 * age))
  )

# TODO: consider Nadler's Formula for blood volume, contrast to ideal weight calc
#For Males = 0.3669 * Ht^3 + 0.03219 * Wt in kgs + 0.6041
#For Females = 0.3561 * Ht^3 + 0.03308 x Wt in kgs + 0.1833
# Surgery. 1962 Feb;51(2):224-32.
# Prediction of blood volume in normal human adults.
# Nadler SB, Hidalgo JH, Bloch T.

#' @title adjusted body weight
#' @description returns ideal weight + 40% of difference between ideal and
#'   actual weights. Ideal weight is calculated using default algorithm.
#'   #TODO: is downward adjustment valid?
#' @inheritParams idealWeight
#' @param weightkg weight in kg, may be a vector
#' @export
adjustedWeightAdult <- function(heightm, weightkg, male)
  0.6 * idealWeight(heightm, male) + 0.4 * weightkg #iw + 0.4*(weightkg - iw)

bmiAdult <- function(heightm, weightkg)
  weightkg / (heightm ^ 2)

bmiAdultInches <- function(heightin, weightkg)
  bmiAdult(heightin * 2.54 / 100, weightkg)
