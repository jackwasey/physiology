#' @title Estimate body surface area
#' @description Estimate body surface area (BSA)
#'
#' @details 1.73 m^2 is commonly used as an average adult BSA.
#'
#' @param height_m height(s) in meters
#' @template weight_kg
#' @template male
#' @param ... passed to validation
#' @return numeric vector of body surface areas in m^2.
#' @examples
#' bsa_dubois_dubois(2, 80)
#' bsa_dubois_dubois(1.5, 80)
#' stopifnot(
#'   identical(
#'     bsa_mosteller(1.5, 80),
#'     bsa_adult(1.5, 80)))
#' @name bsa
#' @family body surface area
NULL

#' @describeIn bsa Uses `bsa_mosteller()`
#' @export
bsa_adult <- function(height_m, weight_kg, ...) {
  bsa_mosteller(height_m, weight_kg, ...)
}

# A helper to simplify the repetitiveness of the options for BSA calculation
bsa_simple <- function(coefficient, w_exp, h_exp) {
  function(height_m, weight_kg, ...) {
    stopifnot(length(height_m) == length(weight_kg))
    valid_height_adult(height_m, ...)
    valid_weight_adult(weight_kg, ...)
    coefficient * (weight_kg ^ w_exp) * ((height_m * 100) ^ h_exp)
  }
}

#' @describeIn bsa Du Bois and Du Bois formula (usually the preferred formula
#'   for adults and children)
#' @references Du Bois D, Du Bois EF (Jun 1916). "A formula to estimate the
#'   approximate surface area if height and weight be known". Archives of
#'   Internal Medicine. 17 (6): 863–71.
#' @export
bsa_dubois_dubois <- bsa_simple(coefficient = 0.007184,
                                w_exp = 0.425, h_exp = 0.725)

#' @describeIn bsa Mosteller formula
#' @references Mosteller, RD (1987). "Simplified calculation of body-surface
#'   area". N Engl J Med. 317 (17): 1098.
#'   \url{https://www.ncbi.nlm.nih.gov/pubmed/3657876}
#' @export
bsa_mosteller <- bsa_simple(coefficient = 1 / 60, w_exp = 0.5, h_exp = 0.5)

#' @describeIn bsa Haycock formula
#' @references Haycock, GB, Schwartz, GJ, Wisotsky, DH (1978). Geometric method
#'   for measuring body surface area: A height-weight formula validated in
#'   infants, children and adults. J Pediatr. 93: 62–66.
#' @export
bsa_haycock <- bsa_simple(coefficient = 0.024265,
                          w_exp = 0.5378, h_exp = 0.3964)

#' @describeIn bsa Gehan and George formula
#' @references Gehan EA, George SL, Cancer Chemother Rep 1970, 54:225-235
#' @export
bsa_gehan_george <- bsa_simple(coefficient = 0.0235,
                               w_exp = 0.51456, h_exp = 0.42246)

#' @describeIn bsa Boyd formula
#' @references Boyd, Edith (1935). The Growth of the Surface Area of the Human
#'   Body. University of Minnesota. The Institute of Child Welfare, Monograph
#'   Series, No. x. London: Oxford University Press.
#' @export
bsa_boyd <- function(height_m, weight_kg, ...) {
  stopifnot(length(height_m) == length(weight_kg))
  valid_height_adult(height_m, ...)
  valid_weight_adult(weight_kg, ...)
  0.03330 * (weight_kg ^ (0.6157 - 0.0188 * log10(weight_kg))) *
    ((height_m * 100) ^ 0.3)
}

#' @describeIn bsa Fujimoto formula (often used for Japanese individuals)
#' @references Fujimoto S, Watanabe T, Sakamoto A, Yukawa K, Morimoto K. Studies
#'   on the physical surface area of Japanese. 18. Calculation formulae in three
#'   stages over all ages. Nippon Eiseigaku Zasshi 1968;5:443–50.
#' @export
bsa_fujimoto <- bsa_simple(coefficient = 0.008883, w_exp = 0.444, h_exp = 0.663)

#' @describeIn bsa Takahira formula (a variant of Du Bois that could be used for
#'   Japanese individuals; Fujimoto may be preferred)
#' @references Fujimoto S, Watanabe T, Sakamoto A, Yukawa K, Morimoto K. Studies
#'   on the physical surface area of Japanese. 18. Calculation formulae in three
#'   stages over all ages. Nippon Eiseigaku Zasshi 1968;5:443–50.
#' @export
bsa_takahira <- bsa_simple(coefficient = 0.007241, w_exp = 0.425, h_exp = 0.725)

#' @describeIn bsa Shuter and Aslani formula
#' @references Shuter, B; Aslani, A (2000). "Body surface area: Du Bois and Du
#'   Bois revisited". European Journal of Applied Physiology. 82 (3): 250–254.
#' @export
bsa_shuter_aslani <- bsa_simple(coefficient = 0.00949,
                                w_exp = 0.441, h_exp = 0.655)

bsa_schlich_female <- bsa_simple(coefficient = 0.000975482,
                                 w_exp = 0.46, h_exp = 1.08)
bsa_schlich_male <- bsa_simple(coefficient = 0.000579479,
                               w_exp = 0.38, h_exp = 1.24)

#' @describeIn bsa Schlich formula
#' @references Schlich, E; Schumm, M; Schlich, M (2010). "3-D-Body-Scan als
#'   anthropometrisches Verfahren zur Bestimmung der spezifischen
#'   Körperoberfläche". Ernährungs Umschau. 57: 178–183.
#' @export
bsa_schlich <- function(height_m, weight_kg, male, ...) {
  stopifnot(length(height_m) == length(weight_kg))
  stopifnot(length(height_m) == length(male))
  stopifnot(is.logical(male))
  ifelse(
    male,
    bsa_schlich_male(height_m, weight_kg, ...),
    bsa_schlich_female(height_m, weight_kg, ...)
  )
}
