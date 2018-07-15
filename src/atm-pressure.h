#include <Rcpp.h>
using namespace Rcpp;

NumericVector atm_pres(NumericVector altitude_m);
NumericVector atm_pres_frac(NumericVector altitude_m);
float getGeopotential(float altitude_km);
float getStandardTemperature(float geopot_height_km);
float getStandardPressure(float altitude_m);
