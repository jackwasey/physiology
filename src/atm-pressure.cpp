#include <Rcpp.h>
#include <math.h>
#include "atm-pressure.h"
using namespace Rcpp;

/*
 * tools::package_native_routine_registration_skeleton(".",
 *  "src/registration.c", character_only = FALSE)
 */

//' Get mean atmospheric pressure at given altitude
//' @param altitude_m Altitude above mean sea level in meters
//' @return Pressure in pascals
//' @examples
//' atm_pres(-430.5) # Dead Sea
//' atm_pres(0)
//' atm_pres(3440) # Namche Bazaar
//' atm_pres(4260) # Dingboche
//' atm_pres(5364) # Everest Base Camp
//' atm_pres(6000) # Camp 1
//' atm_pres(6400) # Camp 2
//' atm_pres(7200) # Camp 3
//' atm_pres(7950) # Camp 4
//' atm_pres(8850) # Everest summit
//' atm_pres_frac(8850) # fraction of sea level pressure on Everest
//' @concept atmospheric pressure
//' @export
// [[Rcpp::export]]
NumericVector atm_pres(NumericVector altitude_m) {
  return sapply(altitude_m, getStandardPressure);
}

//' @describeIn atm_pres Get fraction of mean atomspheric pressure at sea level
//' @export
// [[Rcpp::export]]
NumericVector atm_pres_frac(NumericVector altitude_m) {
  return atm_pres(altitude_m) / getStandardPressure(0);
}

float getGeopotential(float altitude_km)
{
  const float EARTH_RADIUS =  6356.766; // km
  return EARTH_RADIUS * altitude_km / (EARTH_RADIUS + altitude_km);
}

// geopot_height_km = earth_radius * altitude / (earth_radius + altitude) All in
// km Temperature is in kelvins = 273.15 + Celsius
float getStandardTemperature(float geopot_height_km)
{
  // Standard atmospheric pressure
  // Below 51 km: Practical Meteorology by Roland Stull, pg 12
  // Above 51 km: http://www.braeunig.us/space/atmmodel.htm
  if (geopot_height_km <= 11)          // Troposphere
    return 288.15f - (6.5 * geopot_height_km);
  else if (geopot_height_km <= 20)     // Stratosphere starts
    return 216.65f;
  else if (geopot_height_km <= 32)
    return 196.65f + geopot_height_km;
  else if (geopot_height_km <= 47)
    return 228.65f + 2.8 * (geopot_height_km - 32);
  else if (geopot_height_km <= 51)     // Mesosphere starts
    return 270.65f;
  else if (geopot_height_km <= 71)
    return 270.65f - 2.8 * (geopot_height_km - 51);
  else if (geopot_height_km <= 84.85)
    return 214.65f - 2 * (geopot_height_km - 71);
  // Thermosphere has high kinetic temperature (500 C to 2000 C) but temperature
  // as measured by a thermometer would be very low because of almost vacuum.

  throw std::out_of_range("geopot_height_km must be less than 84.85 km.");
}

float getStandardPressure(float altitude)
{
  // Below 51 km: Practical Meteorology by Roland Stull, pg 12. Above 51 km:
  // http://www.braeunig.us/space/atmmodel.htm Validation data:
  // https://www.avs.org/AVS/files/c7/c7edaedb-95b2-438f-adfb-36de54f87b9e.pdf
  altitude = altitude / 1000.0f;  // Convert m to km
  float geopot_height_km = getGeopotential(altitude);
  float t = getStandardTemperature(geopot_height_km);
  if (geopot_height_km <= 11)
    return  101325 * pow(288.15f / t, -5.255877f);
  else if (geopot_height_km <= 20)
    return 22632.06 * exp(-0.1577f * (geopot_height_km - 11));
  else if (geopot_height_km <= 32)
    return 5474.889f * pow(216.65f / t, 34.16319f);
  else if (geopot_height_km <= 47)
    return 868.0187f * pow(228.65f / t, 12.2011f);
  else if (geopot_height_km <= 51)
    return 110.9063f * exp(-0.1262f * (geopot_height_km - 47));
  else if (geopot_height_km <= 71)
    return 66.93887f * pow(270.65f / t, -12.2011f);
  else if (geopot_height_km <= 84.85)
    return 3.956420f * pow(214.65f / t, -17.0816f);
  Rcpp::stop("altitude (in meters) must be less than 86 km.");
}
