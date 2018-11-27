#' alveolar gas equation
#'
#' Estimate PAO2 in alveolus based on atmospheric pressure, fraction of oxygen
#' in inspired air, partial pressure of carbon dioxide in the alveolus, and the
#' respiratory quotient
#' @param fi_o2 fraction of oxygen in inspired gas, from 0 to 1, default
#'   reflects (dry) room air
#' @param rq respiratory quotient, i.e., the ratio of CO2 produced to oxygen
#'   consumed, usually between around 0.7 and 1.0, but can legitimately be
#'   greater than 1.0. Default it 0.8.
#' @param PACO2_mmHg partial pressure of CO2 in alveolus, which can be roughly
#'   approximated as the end-tidal pCO2
#' @param Patm_mmHg atmospheric pressure in kPa
#' @param PAH2O_mmHg partial pressure of water vapor at sea level, defaults to
#'   6.25 kPa (47 mmHg) which is appropriate for body temperature
#' @examples
#' # vary RQ
#' rq <- seq(0.6, 1.4, 0.05)
#' plot(rq, alveolar_PAO2_mmHg(rq = rq))
#'
#' # 100% fi_o2 at typical atmospheric pressure
#' alveolar_PAO2_mmHg(fi_o2 = 1)
#'
#' # hyperbaric oxygen at 100%, 2 atmospheres
#' alveolar_PAO2_mmHg(fi_o2 = 1, Patm_mmHg = 1520)
#' @family respiratory
#' @export
alveolar_PAO2_mmHg <- function(fi_o2 = 0.209, rq = 0.8, PACO2_mmHg = 40,
                               Patm_mmHg = 760, PAH2O_mmHg = 47) {
  stopifnot(fi_o2 >= 0, fi_o2 <= 1)
  stopifnot(rq > 0, rq < 10)
  stopifnot(PACO2_mmHg > 0, PACO2_mmHg < (Patm_mmHg - PAH2O_mmHg))
  stopifnot(Patm_mmHg > 0)
  stopifnot(PAH2O_mmHg > 0, PAH2O_mmHg < (Patm_mmHg - PACO2_mmHg))

    fi_o2 * (Patm_mmHg - PAH2O_mmHg) - PACO2_mmHg * (1 - fi_o2 * (1 - rq)) / rq
}
