#' alveolar gas equation
#'
#' Estimate paO2 in alveolus based on atmospheric pressure, FiO2, PACO2, and
#' respiratory quotient
#' @param FiO2 fraction of oxygen in inspired gas, from 0 to 1, default is (dry)
#'   room air
#' @param RQ respiratory quotient, i.e. the ratio of CO2 produced to oxygen
#'   consumed, usually between around 0.7 and 1.0, but can legitimately be
#'   greater than 1.0. Default it 0.8.
#' @param PACO2_mmHg partial pressure of CO2 in alveolus, which can be approximated
#'   as the end-tidal pCO2
#' @param Patm_mmHg atmospheric pressure in kPa
#' @param PAH2O_mmHg partial pressure of water vapor, defaults to 6.25 kPa (47 mmHg)
#'   which is appropriate for body temperature
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
alveolar_PaO2_mmHg <- function(FiO2 = 0.209, RQ = 0.8, PACO2_mmHg = 40, Patm_mmHg = 760, PAH2O_mmHg = 47) {
  stopifnot(FiO2 >= 0, FiO2 <= 1)
  stopifnot(RQ > 0, RQ < 10)
  stopifnot(PACO2_mmHg > 0, PACO2_mmHg < (Patm_mmHg - PAH2O_mmHg))
  stopifnot(Patm_mmHg > 0)
  stopifnot(PAH2O_mmHg > 0, PAH2O_mmHg < (Patm_mmHg - PACO2_mmHg))

  FiO2 * (Patm_mmHg - PAH2O_mmHg) - PACO2_mmHg * (1 - FiO2 * (1 - RQ)) / RQ
}
