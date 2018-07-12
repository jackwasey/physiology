#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _physiology_atm_pres(SEXP);
extern SEXP _physiology_atm_pres_frac(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_physiology_atm_pres",      (DL_FUNC) &_physiology_atm_pres,      1},
    {"_physiology_atm_pres_frac", (DL_FUNC) &_physiology_atm_pres_frac, 1},
    {NULL, NULL, 0}
};

void R_init_physiology(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
