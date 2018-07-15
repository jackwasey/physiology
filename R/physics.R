#' Temperature in Kelvin from Celsius
#' @export
temp_c_to_k <- function(temp_c)
  273.15 + temp_c

#' Saturation vapor pressure of water at sea level
#' @export
svp_sea_level <- function(temp_k)
  0.00750061683 *
  exp(77.345 + 0.0057 * temp_k - 7235 / temp_k) /
  temp_k ^ 8.2

#' Conversion factor from Pa to torr (mmHg)
#' @keywords datasets
#' @export
Pa_to_torr = 0.0075006168270417