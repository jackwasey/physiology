#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _physiology_pres_atm_frac(SEXP);
extern SEXP _physiology_pres_atm_kPa(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_physiology_pres_atm_frac", (DL_FUNC) &_physiology_pres_atm_frac, 1},
    {"_physiology_pres_atm_kPa",  (DL_FUNC) &_physiology_pres_atm_kPa,  1},
    {NULL, NULL, 0}
};

void R_init_physiology(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
