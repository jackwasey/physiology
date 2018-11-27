#' Temperature in Kelvin from Celsius
#' @param temp_c Temperature in Celsius
#' @family physics
#' @export
temp_c_to_k <- function(temp_c)
  273.15 + temp_c

#' Saturation vapor pressure of water at sea level
#' @param temp_k Temperature in Kelvin
#' @family physics
#' @export
svp_sea_level <- function(temp_k)
  0.00750061683 *
  exp(77.345 + 0.0057 * temp_k - 7235 / temp_k) /
  temp_k ^ 8.2

#' Conversion factor from Pa to torr (mmHg)
#'
#' The conversion is exactly 760 / 101325
#' @keywords datasets
#' @family physics
#' @export
Pa_to_torr <- 760 / 101325
