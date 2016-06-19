#' alveolar gas equation
#'
#' Estimate paO2 in alveolus based on atmospheric pressure, FiO2, PaCO2, and
#' respiratory quotient
#' @param FiO2 fraction of oxygen in inspired gas, from 0 to 1, default is (dry)
#'   room air
#' @param RQ respiratory quotient, i.e. the ratio of CO2 produced to oxygen
#'   consumed, usually between around 0.7 and 1.0, but can legitimately be
#'   greater than 1.0. Default it 0.8.
#' @param paCO2 partial pressure of CO2 in alveolus, which can be approximated
#'   as the end-tidal pCO2
#' @param paH2O partial pressure of water vapor, defaults to 6.25 kPa (47 mmHg)
#'   which is appropriate for body temperature
#' @param Patm atmospheric pressure in kPa
#' @examples
#' # vary RQ
#' rq <- seq(0.6, 1.4, 0.05)
#' plot(rq, alveolar_paO2_mmHg(RQ = rq))
#'
#' # 100% FiO2 at typical atmospheric pressure
#' alveolar_paO2_mmHg(FiO2 = 1)
#'
#' # hyperbaric oxygen at 100%, 2 atmospheres
#' alveolar_paO2_mmHg(FiO2 = 1, Patm_mmHg = 1520)
#' @export
alveolar_paO2_mmHg <- function(FiO2 = 0.209, RQ = 0.8, paCO2_mmHg = 40, Patm_mmHg = 760, paH2O_mmHg = 47) {
  stopifnot(FiO2 >= 0, FiO2 <= 1)
  stopifnot(RQ > 0, RQ < 10)
  stopifnot(paCO2_mmHg > 0, paCO2_mmHg < (Patm_mmHg - paH2O_mmHg))
  stopifnot(Patm_mmHg > 0)
  stopifnot(paH2O_mmHg > 0, paH2O_mmHg < (Patm_mmHg - paCO2_mmHg))

  FiO2 * (Patm_mmHg - paH2O_mmHg) - paCO2_mmHg * (1 - FiO2 * (1 - RQ)) / RQ
}
