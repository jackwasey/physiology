% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eGFR.R
\name{egfr}
\alias{egfr}
\alias{egfr_cockcroft_gault}
\alias{egfr_mdrd}
\alias{egfr_ckdepi}
\alias{egfr_bedside_schwartz}
\title{Automatically select the best equation to use for estimated glomerular
filtration rate (eGFR) calculation.}
\usage{
egfr(scr_uM, age_y, height_m, male, black, ...)

egfr_cockcroft_gault(scr_uM, age_y, weight_kg, male, idms_assay = TRUE, ...)

egfr_mdrd(
  scr_uM,
  age_y,
  male,
  black,
  idms_assay = TRUE,
  ...,
  warn_ckdepi_preferred = TRUE
)

egfr_ckdepi(
  scr_uM,
  age_y,
  male,
  black,
  idms_assay = TRUE,
  ...,
  warn_mdrd_preferred = TRUE
)

egfr_bedside_schwartz(scr_uM, height_m, idms_assay = TRUE, ...)
}
\arguments{
\item{scr_uM}{Serum creatinine (in micromoles/L, or 'uM').}

\item{age_y}{Age in years}

\item{height_m}{Height in meters}

\item{male}{Logical, TRUE (male) or FALSE (female)}

\item{black}{Logical, \code{TRUE} (race is Black (African-American in USA) or
\code{FALSE}}

\item{...}{passed to subsequent GFR methods (for \code{egfr}) or validation
(for other functions)}

\item{weight_kg}{numeric vector of weight(s) in kg}

\item{idms_assay}{Was an isotope dilution mass spectrometry (IDMS) calibrated
assay used for serum creatinine measurement?}

\item{warn_ckdepi_preferred}{When calculating eGFR > 60, should a warning be
generated suggesting CKD-EPI is preferred?}

\item{warn_mdrd_preferred}{When calculating eGFR < 60, should a warning be
generated suggesting MDRD is preferred?}
}
\value{
A vector of estimated glomerular filtration rates with units of
  mL/min/1.73 m^2 (except that the units are mL/min for
  \code{\link{egfr_cockcroft_gault}}.
}
\description{
Automatically select the best equation to use for estimated glomerular
filtration rate (eGFR) calculation.
}
\details{
GFR estimation is not recommended or validated for individuals with unstable
creatinine concentration (including pregnancy, serious comorbid conditions,
hospitalized patients, patients with acute renal failure) or extremes in
muscle mass and diet (including amputees, paraplegics, bodybuilders, or
obese patients; or vegetarians or when taking creatine dietary supplements).
For more details, please refer to the NIDDK summary on estimating GFR:
\href{https://www.niddk.nih.gov/health-information/professionals/clinical-tools-patient-management/kidney-disease/laboratory-evaluation/glomerular-filtration-rate-calculators}{NIDDK eGFR calculators}.

The main function (\code{egfr}) automatically selects the best
method for eGFR calculation based on the following metrics:

* If \code{age_y < 18}, use the Bedside Schwartz equation.
* If \code{age_y >= 18}:
    * Estimate eGFR by the MDRD and CKD-EPI methods
    * If eGFR,MDRD is estimated < 60 mL/min/1.73 m^2 and eGFR,CKD-EPI < 60,
    return eGFR,MDRD.
    * If eGFR,MDRD is estimated >= 60 mL/min/1.73 m^2 and eGFR,CKD-EPI >=
    60, return eGFR,CKD-EPI.
    * Otherwise, return the average of eGFR,MDRD and eGFR,CKD-EPI.

If an IDMS - calibrated assay is used (\code{idms_assay = TRUE}), the MDRD
equation will be corrected for the assay by approximately 6%, the CKD-EPI
equation is only validated for use with IDMS - calibrated assays, and the
Cockcroft - Gault is not calibrated for use with an IDMS - calibrated assay.
Most labs follow the National Kidney Disease Education Program (NKDEP)
recommendation to use an IDMS - calibrated assay, so by default
\code{idms_assay = TRUE}.
}
\section{Functions}{
\itemize{
\item \code{egfr_cockcroft_gault()}: The Cockcroft - Gault equation for eGFR (not preferred).

\item \code{egfr_mdrd()}: The MDRD equation for eGFR (preferred for adults with eGFR <
60).

\item \code{egfr_ckdepi()}: The CKD-EPI equation for eGFR (preferred for adults
with eGFR >= 60).

\item \code{egfr_bedside_schwartz()}: The Bedside Schwartz equation for eGFR (for children, age
less than 18 years).

}}
\references{
Levey AS, Stevens LA, Schmid CH, Zhang YL, Castro AF, 3rd,
Feldman HI, et al. A new equation to estimate glomerular filtration rate. Ann
Intern Med. 2009;150(9):604-12.

Levey AS, Coresh J, Greene T, Stevens LA, Zhang YL, Hendriksen S, Kusek JW,
Van Lente F; Chronic Kidney Disease Epidemiology Collaboration. Using
standardized serum creatinine values in the modification of diet in renal
disease study equation for estimating glomerular filtration rate. Ann Intern
Med. 2006 Aug 15;145(4):247-54.

Cockcroft DW, Gault MH. Prediction of creatinine clearance from serum
creatinine. Nephron. 1976;16(1):31-41.

\href{https://www.niddk.nih.gov/health-information/professionals/clinical-tools-patient-management/kidney-disease/laboratory-evaluation/glomerular-filtration-rate-calculators}{NIDDK eGFR calculators}

Levey AS, Stevens LA, Schmid CH, Zhang YL, Castro AF, 3rd, Feldman HI, et al.
A new equation to estimate glomerular filtration rate. Ann Intern Med.
2009;150(9):604-12.

Levey AS, Coresh J, Greene T, Stevens LA, Zhang YL, Hendriksen S, Kusek JW,
Van Lente F; Chronic Kidney Disease Epidemiology Collaboration. Using
standardized serum creatinine values in the modification of diet in renal
disease study equation for estimating glomerular filtration rate. Ann Intern
Med. 2006 Aug 15;145(4):247-54.

\href{https://www.niddk.nih.gov/health-information/professionals/clinical-tools-patient-management/kidney-disease/laboratory-evaluation/glomerular-filtration-rate-calculators}{NIDDK eGFR calculators}

Schwartz GJ, et al. New equations to estimate GFR in children with CKD. J Am
Soc Nephrol. 2009;20:629-637.

Schwartz GJ and Work DF. Measurement and estimation of GFR in children and
adolescents. Clin J Am Soc Nephrol. 2009;4(11):1832-43.
}
\seealso{
\code{\link{creatinine_mgdl_to_uM}}

Other renal: 
\code{\link{creatinine_mgdl_to_uM}()}

Other renal: 
\code{\link{creatinine_mgdl_to_uM}()}

Other renal: 
\code{\link{creatinine_mgdl_to_uM}()}

Other renal: 
\code{\link{creatinine_mgdl_to_uM}()}

Other renal: 
\code{\link{creatinine_mgdl_to_uM}()}
}
\concept{renal}
