#' @title calculate pH from Henderson Hasselbach equation
#' @param bicarbonate mmol/L
#' @param ppCO2 partial pressure of carbon dioxide in mmHg
#' @examples
#'  bicarbonate <- seq(10, 50, 5)
#'  ppCO2 <- seq(20, 70, 10)
#'  bc <- rep(bicarbonate, length(ppCO2))
#'  pp <- rep(ppCO2, each = length(bicarbonate))
#'  acidbase <- matrix(henderson_hasselbach_pH(bc, pp), nrow = 9, ncol = 6)
#'  rownames(acidbase) <- paste("bicarb", bicarbonate)
#'  colnames(acidbase) <- paste("PaCO2", ppCO2)
#'  acidbase
#' @export
henderson_hasselbach_pH <- function(bicarbonate, ppCO2) {
  6.1 + log10(bicarbonate / (0.0307 * ppCO2))
}
