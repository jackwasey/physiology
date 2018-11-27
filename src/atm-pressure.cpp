#include <Rcpp.h>
#include <math.h>
#include "atm-pressure.h"
using namespace Rcpp;

//' Get mean atmospheric pressure at given altitude in kPa
//' @param altitude_m Altitude above mean sea level in meters
//' @return Pressure in pascals
//' @examples
//' pres_atm_kPa(-430.5) # Dead Sea
//' pres_atm_kPa(0)
//' pres_atm_kPa(3440) # Namche Bazaar
//' pres_atm_kPa(4260) # Dingboche
//' pres_atm_kPa(5364) # Everest Base Camp
//' pres_atm_kPa(6000) # Camp 1
//' pres_atm_kPa(6400) # Camp 2
//' pres_atm_kPa(7200) # Camp 3
//' pres_atm_kPa(7950) # Camp 4
//' pres_atm_kPa(8850) # Everest summit
//' pres_atm_frac(8850) # fraction of sea level pressure on Everest
//' @references
//' Below 51 km: Practical Meteorology by Roland Stull, pg 12. Above 51 km:
//' \url{http://www.braeunig.us/space/atmmodel.htm} Validation data:
//' \url{https://www.avs.org/AVS/files/c7/c7edaedb-95b2-438f-adfb-36de54f87b9e.pdf}
//' @concept atmospheric pressure
//' @family physics
//' @export
// [[Rcpp::export]]
NumericVector pres_atm_kPa(NumericVector altitude_m) {
  return sapply(altitude_m, getStandardPressure);
}

//' @describeIn pres_atm_kPa Get fraction of mean atomspheric pressure at sea level
//' @export
// [[Rcpp::export]]
NumericVector pres_atm_frac(NumericVector altitude_m) {
  NumericVector p = pres_atm_kPa(altitude_m);
  float p0 = getStandardPressure(0);
  return ( p / p0);
}

// [[Rcpp::export]]
float getGeopotential(float altitude_km)
{
  const float EARTH_RADIUS =  6356.766; // km
  return EARTH_RADIUS * altitude_km / (EARTH_RADIUS + altitude_km);
}

//' @title get standard temperature at a given altitude
//' @description `altitude_km = earth_radius * altitude / (earth_radius +
//' altitude)` All in km Temperature is in Kelvin.
//' @seealso \code{\link{pres_atm_kPa}}
//' @param altitude_km Altitude in kilometers
//' @md
//' @noRd
//' @keywords internal
// [[Rcpp::export]]
float getStandardTemperature(float altitude_km) {
  if (altitude_km <= 11)          // Troposphere
    return 288.15f - (6.5 * altitude_km);
  else if (altitude_km <= 20)     // Stratosphere starts
    return 216.65f;
  else if (altitude_km <= 32)
    return 196.65f + altitude_km;
  else if (altitude_km <= 47)
    return 228.65f + 2.8 * (altitude_km - 32);
  else if (altitude_km <= 51)     // Mesosphere starts
    return 270.65f;
  else if (altitude_km <= 71)
    return 270.65f - 2.8 * (altitude_km - 51);
  else if (altitude_km <= 84.85)
    return 214.65f - 2 * (altitude_km - 71);
  // Thermosphere has high kinetic temperature (500 C to 2000 C) but temperature
  // as measured by a thermometer would be very low because of almost vacuum.
  Rcpp::stop("altitude_km must be less than 84.85 km.");
}

// [[Rcpp::export]]
float getStandardPressure(float altitude_m) {
  altitude_m = altitude_m / 1000.0f;  // Convert m to km
  float altitude_km = getGeopotential(altitude_m);
  float t = getStandardTemperature(altitude_km);
  if (altitude_km <= 11)
    return  101325 * pow(288.15f / t, -5.255877f);
  else if (altitude_km <= 20)
    return 22632.06 * exp(-0.1577f * (altitude_km - 11));
  else if (altitude_km <= 32)
    return 5474.889f * pow(216.65f / t, 34.16319f);
  else if (altitude_km <= 47)
    return 868.0187f * pow(228.65f / t, 12.2011f);
  else if (altitude_km <= 51)
    return 110.9063f * exp(-0.1262f * (altitude_km - 47));
  else if (altitude_km <= 71)
    return 66.93887f * pow(270.65f / t, -12.2011f);
  // else if (altitude_km <= 84.85) // checked by geopotential
  return 3.956420f * pow(214.65f / t, -17.0816f);
}
