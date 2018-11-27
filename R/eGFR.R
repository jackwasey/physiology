#' Convert serum creatinine from mg/dL to umol/L
#'
#' @details Validation is performed after unit conversion. The result is more
#'   precise than the typical conversion used of \code{1 mg/dL = 88.4 umol/L}.
#' @param scr_mgdl Serum creatinine in mg/dL units
#' @param ... passed to validation
#' @return Serum creatinine in umol/L units
#' @export
#' @references Molecular weight is 113.12 g/mol from
#'   \url{https://pubchem.ncbi.nlm.nih.gov/compound/creatinine}
#' @seealso \code{\link{egfr}}
#' @family renal
creatinine_mgdl_to_uM <- function(scr_mgdl, ...) {
  scr_uM <- scr_mgdl * 10000 / 113.12
  valid_creatinine(scr_uM = scr_uM, ...)
  scr_uM
}

#' Automatically select the best equation to use for estimated glomerular
#' filtration rate (eGFR) calculation.
#'
#' @details
#' GFR estimation is not recommended or validated for individuals with unstable
#' creatinine concentration (including pregnancy, serious comorbid conditions,
#' hospitalized patients, patients with acute renal failure) or extremes in
#' muscle mass and diet (including amputees, paraplegics, bodybuilders, or
#' obese patients; or vegetarians or when taking creatine dietary supplements).
#' For more details, please refer to the NIDDK summary on estimating GFR:
#' \url{https://www.niddk.nih.gov/health}.
#'
#' The main function (\code{egfr}) automatically selects the best
#' method for eGFR calculation based on the following metrics:
#'
#' * If \code{age_y < 18}, use the Bedside Schwartz equation.
#' * If \code{age_y >= 18}:
#'     * Estimate eGFR by the MDRD and CKD-EPI methods
#'     * If eGFR,MDRD is estimated < 60 mL/min/1.73 m^2 and eGFR,CKD-EPI < 60,
#'     return eGFR,MDRD.
#'     * If eGFR,MDRD is estimated >= 60 mL/min/1.73 m^2 and eGFR,CKD-EPI >=
#'     60, return eGFR,CKD-EPI.
#'     * Otherwise, return the average of eGFR,MDRD and eGFR,CKD-EPI.
#'
#' If an IDMS - calibrated assay is used (\code{idms_assay = TRUE}), the MDRD
#' equation will be corrected for the assay by approximately 6%, the CKD-EPI
#' equation is only validated for use with IDMS - calibrated assays, and the
#' Cockcroft - Gault is not calibrated for use with an IDMS - calibrated assay.
#' Most labs follow the National Kidney Disease Education Program (NKDEP)
#' recommendation to use an IDMS - calibrated assay, so by default
#' \code{idms_assay = TRUE}.
#'
#' @param scr_uM Serum creatinine (in micromoles/L, or 'uM').
#' @param age_y Age in years
#' @param height_m Height in meters
#' @param male Logical, TRUE (male) or FALSE (female)
#' @param black Logical, \code{TRUE} (race is Black (African-American in USA) or
#'   \code{FALSE}
#' @param idms_assay Was an isotope dilution mass spectrometry (IDMS) calibrated
#'   assay used for serum creatinine measurement?
#' @param ... passed to subsequent GFR methods (for \code{egfr}) or validation
#'   (for other functions)
#' @return A vector of estimated glomerular filtration rates with units of
#'   mL/min/1.73 m^2 (except that the units are mL/min for
#'   \code{\link{egfr_cockcroft_gault}}.
#' @references Levey AS, Stevens LA, Schmid CH, Zhang YL, Castro AF, 3rd,
#' Feldman HI, et al. A new equation to estimate glomerular filtration rate. Ann
#' Intern Med. 2009;150(9):604-12.
#'
#' Levey AS, Coresh J, Greene T, Stevens LA, Zhang YL, Hendriksen S, Kusek JW,
#' Van Lente F; Chronic Kidney Disease Epidemiology Collaboration. Using
#' standardized serum creatinine values in the modification of diet in renal
#' disease study equation for estimating glomerular filtration rate. Ann Intern
#' Med. 2006 Aug 15;145(4):247-54.
#' @export
#' @seealso \code{\link{creatinine_mgdl_to_uM}}
#' @family renal
egfr <- function(scr_uM, age_y, height_m, male, black, ...) {
  stopifnot(length(scr_uM) == length(age_y))
  stopifnot(length(scr_uM) == length(height_m))
  stopifnot(length(scr_uM) == length(male))
  stopifnot(length(scr_uM) == length(black))
  valid_creatinine(scr_uM, ...)
  valid_height(height_m, ...)
  valid_age(age_y, ...)
  mask_child <- age_y < 18
  mask_adult <- !mask_child
  ret <- rep(NA_real_, length(scr_uM))
  if (any(mask_child)) {
    ret[mask_child] <-
      egfr_bedside_schwartz(
        scr_uM = scr_uM[mask_child],
        height_m = height_m[mask_child],
        ...
      )
  }
  if (any(mask_adult)) {
    mdrd <-
      egfr_mdrd(
        scr_uM = scr_uM[mask_adult],
        age_y = age_y[mask_adult],
        male = male[mask_adult],
        black = black[mask_adult],
        ...
      )
    ckdepi <-
      egfr_ckdepi(
        scr_uM = scr_uM[mask_adult],
        age_y = age_y[mask_adult],
        male = male[mask_adult],
        black = black[mask_adult],
        ...,
        warn_mdrd_preferred = FALSE
      )
    use_mdrd <- mdrd < 60
    use_ckd <- ckdepi >= 60
    use_both <- use_mdrd & use_ckd
    use_neither <- (!use_mdrd) & (!use_ckd)
    use_mdrd[use_both] <- FALSE
    use_ckd[use_both] <- FALSE
    if (any(use_both)) {
      warning(sum(use_both), " values recommend using both MDRD and CKD-EPI ",
              "equations. Averaging the values.")
      mdrd[use_both] <- rowMeans(cbind(mdrd[use_both], ckdepi[use_both]))
    }
    if (any(use_neither)) {
      warning(sum(use_neither), " values recommend using neither MDRD nor ",
              "CKD-EPI equations. Averaging the values.")
      mdrd[use_neither] <- rowMeans(cbind(mdrd[use_neither],
                                          ckdepi[use_neither]))
    }
    mdrd[use_ckd] <- ckdepi[use_ckd]
    ret[mask_adult] <- mdrd
  }
  ret
}

#' @describeIn egfr The Cockcroft - Gault equation for eGFR (not preferred).
#' @template weight_kg
#' @references
#' Cockcroft DW, Gault MH. Prediction of creatinine clearance from serum
#' creatinine. Nephron. 1976;16(1):31-41.
#' @export
#' @family renal
egfr_cockcroft_gault <- function(scr_uM, age_y, weight_kg, male,
                                idms_assay = TRUE, ...) {
  warning("The Cockcroft-Gault equation for eGFR is not recommended by the ",
          "NKDEP or the NIH. MDRD or CKD-EPI are recommended.")
  stopifnot(length(scr_uM) == length(age_y))
  stopifnot(length(scr_uM) == length(weight_kg))
  stopifnot(length(scr_uM) == length(male))
  stopifnot(length(idms_assay) == 1)
  valid_creatinine(scr_uM, ...)
  valid_weight(weight_kg, ...)
  valid_age(age_y, ...)
  if (idms_assay)
    warning("The Cockcroft-Gault equation for eGFR is not calibrated for use ",
            "with an IDMS - calibrated assay. Interpret results with caution.")
  (140 - age_y) *
    weight_kg *
    (0.85 + 0.15 * male) /
    (72 * # The typical contstant in the denominator
       113.12 / 10000 * # unit conversion for umol/L
       scr_uM
    )
}

#' @describeIn egfr The MDRD equation for eGFR (preferred for adults with eGFR <
#'   60).
#' @param warn_ckdepi_preferred When calculating eGFR > 60, should a warning be
#'   generated suggesting CKD-EPI is preferred?
#' @references
#' \url{https://www.niddk.nih.gov/health}
#'
#' Levey AS, Stevens LA, Schmid CH, Zhang YL, Castro AF, 3rd, Feldman HI, et al.
#' A new equation to estimate glomerular filtration rate. Ann Intern Med.
#' 2009;150(9):604-12.
#'
#' Levey AS, Coresh J, Greene T, Stevens LA, Zhang YL, Hendriksen S, Kusek JW,
#' Van Lente F; Chronic Kidney Disease Epidemiology Collaboration. Using
#' standardized serum creatinine values in the modification of diet in renal
#' disease study equation for estimating glomerular filtration rate. Ann Intern
#' Med. 2006 Aug 15;145(4):247-54.
#' @export
#' @family renal
egfr_mdrd <- function(scr_uM, age_y, male, black, idms_assay = TRUE,
                      ..., warn_ckdepi_preferred = TRUE) {
  stopifnot(length(scr_uM) == length(age_y))
  stopifnot(length(scr_uM) == length(male))
  stopifnot(length(scr_uM) == length(black))
  stopifnot(all(male %in% c(TRUE, FALSE)))
  stopifnot(all(black %in% c(TRUE, FALSE)))
  stopifnot(length(idms_assay) == 1)
  valid_creatinine(scr_uM = scr_uM, ...)
  valid_age_adult(age_y, ...)
  if (any(age_y < 18)) {
    warning("The MDRD equation for eGFR is intended for adults, age_y >= 18.")
  }
  idms_correction <- 1
  if (idms_assay) {
    idms_correction <- 175 / 186
  }
  ret <-
    idms_correction *
    32788 *
    (scr_uM ^ (-1.154)) *
    (age_y ^ (-0.203)) *
    (0.742 + 0.258 * male) *
    (1 + 0.212 * black)
  if (any(ret > 60) & warn_ckdepi_preferred)
    warning("The MDRD equation for eGFR is not recommended for values above ",
            "60 mL/min/1.73m^2 (CKD-EPI is recommended). ",
            "Use results above 60 with caution.")
  ret
}

#' @describeIn egfr The CKD-EPI equation for eGFR (preferred for adults
#'   with eGFR >= 60).
#' @param warn_mdrd_preferred When calculating eGFR < 60, should a warning be
#'   generated suggesting MDRD is preferred?
#' @export
#' @family renal
egfr_ckdepi <- function(scr_uM, age_y, male, black, idms_assay = TRUE,
                        ..., warn_mdrd_preferred = TRUE) {
  stopifnot(length(scr_uM) == length(age_y))
  stopifnot(length(scr_uM) == length(male))
  stopifnot(length(scr_uM) == length(black))
  stopifnot(all(male %in% c(TRUE, FALSE)))
  stopifnot(all(black %in% c(TRUE, FALSE)))
  stopifnot(length(idms_assay) == 1)
  valid_creatinine(scr_uM = scr_uM, ...)
  valid_age_adult(age_y, ...)
  if (any(age_y < 18)) {
    warning("The CKD-EPI equation for eGFR is not recommended for ages below ",
            "18 years.")
  }
  if (!idms_assay) {
    warning("The CKD-EPI equation is only designed for use with an ",
            "IDMS-calibrated serum creatinine assay.")
  }
  kappa <- 61.9 + 17.7 * male
  alpha <- -0.329 - 0.082 * male
  ret <-
    141 *
    pmin(scr_uM / kappa, 1) ^ alpha *
    pmax(scr_uM / kappa, 1) ^ (-1.209) *
    0.993 ^ age_y *
    (1.018 - 0.018 * male) *
    (1 + 0.159 * black)
  if (any(ret < 60) & warn_mdrd_preferred) {
    warning("The CKD-EPI equation for eGFR is not recommended for values below",
            " 60 mL/min/1.73m^2 (MDRD is recommended). Use with caution.")
  }
  ret
}

#' @describeIn egfr The Bedside Schwartz equation for eGFR (for children, age
#'   less than 18 years).
#' @references
#'
#' \url{https://www.niddk.nih.gov/health}
#'
#' Schwartz GJ, et al. New equations to estimate GFR in children with CKD. J Am
#' Soc Nephrol. 2009;20:629-637.
#'
#' Schwartz GJ and Work DF. Measurement and estimation of GFR in children and
#' adolescents. Clin J Am Soc Nephrol. 2009;4(11):1832-43.
#' @export
#' @family renal
egfr_bedside_schwartz <- function(scr_uM, height_m, idms_assay = TRUE, ...) {
  stopifnot(length(scr_uM) == length(height_m))
  stopifnot(length(idms_assay) == 1)
  valid_creatinine(scr_uM = scr_uM, ...)
  valid_height(height_m, ...)
  if (!idms_assay) {
    warning("The Bedside-Schwartz equation is calibrated for use with an ",
            "IDMS-traceable assay. eGFR from a non-IDMS-calibrated assay may ",
            "be biased.")
  }
  36.2 * (height_m * 100) / scr_uM
}
