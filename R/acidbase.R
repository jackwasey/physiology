#' pH by Henderson Hasselbalch equation
#'
#' Calculate the pH based on bicarbonate and partial pressure of CO2
#' @param bicarbonate mmol/L
#' @param pp_co2 partial pressure of carbon dioxide in mmHg
#' @examples
#'  bicarbonate <- seq(10, 50, 5)
#'  pp_co2 <- seq(20, 70, 10)
#'  bc <- rep(bicarbonate, length(pp_co2))
#'  pp <- rep(pp_co2, each = length(bicarbonate))
#'  acidbase <- matrix(henderson_hasselbalch(bc, pp), nrow = 9, ncol = 6)
#'  rownames(acidbase) <- paste("bicarb", bicarbonate)
#'  colnames(acidbase) <- paste("PaCO2", pp_co2)
#'  acidbase
#' @family acid-base
#' @export
henderson_hasselbalch <- function(bicarbonate, pp_co2) {
  6.1 + log10(bicarbonate / (0.0307 * pp_co2))
}
