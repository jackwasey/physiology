% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eGFR.R
\name{creatinine_mgdl_to_uM}
\alias{creatinine_mgdl_to_uM}
\title{Convert serum creatinine from mg/dL to umol/L}
\usage{
creatinine_mgdl_to_uM(scr_mgdl, ...)
}
\arguments{
\item{scr_mgdl}{Serum creatinine in mg/dL units}

\item{...}{passed to validation}
}
\value{
Serum creatinine in umol/L units
}
\description{
Convert serum creatinine from mg/dL to umol/L
}
\details{
Validation is performed after unit conversion. The result is more
  precise than the typical conversion used of \code{1 mg/dL = 88.4 umol/L}.
}
\references{
Molecular weight is 113.12 g/mol from
  \url{https://pubchem.ncbi.nlm.nih.gov/compound/creatinine}
}
\seealso{
\code{\link{egfr}}

Other renal: 
\code{\link{egfr}()}
}
\concept{renal}
