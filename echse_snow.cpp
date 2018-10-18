#ifndef SNOW_H
#define SNOW_H

#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

////////////////////////////////////////////////////////////////////////////////
// Saturation vapor pressure as a function of temperature
//
// Source: Dyck & Peschke (1995), p. 58, Eq. 4.6
//
// Input:  Temperature (degree celsius)
// Output: Result in (hPa)

// [[Rcpp::export]]
double satVapPress_overWater (const double temp) {
  return( 6.11 * pow(10., 7.5*temp/(237.3+temp)) );  // Magnus equ. over water
}

// [[Rcpp::export]]
double satVapPress_overIce (const double temp) {
  return( 6.11 * pow(10., 9.5*temp/(265.5+temp)) );  // Magnus equ. over ice
}

////////////////////////////////////////////////////////////////////////////////
// Vapor pressure as a function of temperature and relative humidity
//
// Source: Dyck & Peschke (1995), p. 61, Eq. 4.12 together with the equation
//         to compute the saturation vapor pressure

// Input:  Temperature (degree celsius)
// Input:  Relative humidity (%)
// Output: Result in (hPa)

// [[Rcpp::export]]
double vapPress_overWater (const double temp, const double relhum) {
  return( satVapPress_overWater(temp) * relhum / 100. );
}

// [[Rcpp::export]]
double vapPress_overIce (const double temp, const double relhum) {
  return( satVapPress_overIce(temp) * relhum / 100. );
}

////////////////////////////////////////////////////////////////////////////////
// Dew point temperature as a function of temperature and relative humidity
//
// Source: See explanation in documentation of the snow model

// Input:  Vapor Pressure (hPa)
// Output: Result in (hPa)

// [[Rcpp::export]]
double dewTemp_overWater (const double vapPress) {
  return( 237.3 * log10(vapPress / 6.11) / (7.5 - log10(vapPress / 6.11)) );
}

// [[Rcpp::export]]
double dewTemp_overIce (const double vapPress) {
  return( 265.5 * log10(vapPress / 6.11) / (9.5 - log10(vapPress / 6.11)) );
}

////////////////////////////////////////////////////////////////////////////////
// Vapor pressure deficit (E(T) - e) in hPa as a function of temperature T (°C)
// and relative humidity U (%)
//
// Source: Just a combination of the equations for the actual vapor pressure and
//         the saturation vapor pressure (see there).
//           
// Input:  Temperature (degree celsius)
// Input:  Relative humidity (%)
// Output: Result in (hPa)

// [[Rcpp::export]]
double vapPressDeficit_overWater (const double temp, const double relhum) {
  return( satVapPress_overWater(temp) * (1. - relhum / 100.) );
}

// [[Rcpp::export]]
double vapPressDeficit_overIce (const double temp, const double relhum) {
  return( satVapPress_overIce(temp) * (1. - relhum / 100.) );
}

////////////////////////////////////////////////////////////////////////////////
// Slope of the saturation vapor pressure curve, i.e. derivative of saturation
// vapor pressure with resp. to temperature  s = d E(T) / d T
//
// Source: Dyck & Peschke (1995), p. 188, Eq. 11.13
//
// Input:  Temperature (degree celsius)
// Output: Result in (hPa/K)

// [[Rcpp::export]]
double slopeSatVapPress (const double temp) {
  return(
    6.11 * exp(17.3 * temp / (237.3 + temp)) *
      4105.3 / pow(237.3 + temp, 2)
  );
}

////////////////////////////////////////////////////////////////////////////////
// Specific humidity (-)
//
// Source: Dyck & Peschke (1995), p. 60, Eq. 4.10
//
// Input:  Air pressure (hPa)
// Input:  Vapor pressure (hPa)
// Output: Result is dimensionless

// [[Rcpp::export]]
double specificHumidity (const double pressAir, const double vapPress) {
  return( 0.622 * vapPress / (pressAir - 0.378 * vapPress) );
}

////////////////////////////////////////////////////////////////////////////////
// Latent heat of water evaporation as a function of temperature
//
// Source: Dyck & Peschke (1995), p. 28, Eq. 3.2
//
// Input:  Temperature (degree celsius)
// Output: Result in kJ/kg

// [[Rcpp::export]]
double latentHeatEvap (const double temp) {
  return( 2501. - 2.37 * temp );
}

////////////////////////////////////////////////////////////////////////////////
// Psychrometric constant
//
// Source: Dyck & Peschke (1995), p. 188, Eq. 11.15  (with p. 28, Eq. 3.2)
//
// Input:  Temperature (degree celsius)
// Input:  Air pressure (hPa)
// Output: Result in hPa/K

// [[Rcpp::export]]
double psychroConst (const double temp, const double airpress) {
  return( 0.016286 * airpress / latentHeatEvap(temp) );
}

////////////////////////////////////////////////////////////////////////////////
// Density of dry air
// Note: The error when using this equation for moist air is low as under normal
//       atmospheric conditions the vapor pressure is small when compared to the
//       air pressure.
//
// Source: Dyck & Peschke (1995), p. 60, Eq. 4.9
// 
// Input:  Temperature (degree celsius)
// Input:  Air pressure (hPa)
// Output: Result in kg/m3

// [[Rcpp::export]]
double densityDryAir (const double temp, const double airpress) {
  return( airpress * 0.1 / (0.287 * (273.15 + temp)) );
}

// Precipitation correction after Sevruk (1989) for wind error.
//
// Source: Bremicker (2000), Freiburger Schriften zur Hydrologie No. 11, p. 43, 48
//
// Notes: The function returns a factor for multiplicative correction of
//        precipitation. Only the error due to wind is corrected. Evaporation
//        and interception losses are neglected. The equations derived by
//        Sevruk require the wind speed at the height of the rain gage as input.
//        Here, the wind speed at rain gage height is estimated from speed at
//        measuring height assuming a logarithmic wind profile with zero speed
//        at a selected level z0. The basic functional structure is just
//          u(z) = a * log(z) + b
//        with the two conditions
//          (1) 0 =  a * log(z0) + b
//          (2) observation = a * log(sensor height) + b
//        For two sensors at levels z1 and z2, this leads to
//          u(z1) = u(z2) * log(z1/z0) / log(z2/z0)
//
//        Warning: Using height=0 for the wind sensor will lead to division by
//        zero but this does not make sense anyway. Setting z0=zero is invalid
//        and will be corrected. 
//
// Input: Windspeed (m/s)
// Input: Air temperature (°C)
// Input: Height of rain gage over ground (m), usually 1 m
// Input: Height of wind sensor over ground (m), usually 10 m
// Input: Height over ground where wind speed becomes zero (m), must be > 0,
//        otherwise corrected to a small value.
// Output: Dimensionless correction factor

// [[Rcpp::export]]
double factPrecipCorr (const double wind, const double temp,
                       const double sensHeight_prec, const double sensHeight_wind, const double height_zeroWind
) {
  double wind_corr= wind * log10(sensHeight_prec/fmax(0.001,height_zeroWind)) /
    log10(sensHeight_wind/fmax(0.001,height_zeroWind));
  if (temp >= 0.) {
    return ( 1. + (0.015 * wind_corr) );
  } else if (temp >= -8.) {
    return( 1. + (0.150 * pow(wind_corr, 1.18)) );
  } else if (temp >= -27.) {
    return( 1. + (0.280 * pow(wind_corr, 1.30)) );
  } else {
    return( 1. + (0.550 * pow(wind_corr, 1.40)) );
  }
}





















////////////////////////////////////////////////////////////////////////////////
// Definition of stoichiometry factors
////////////////////////////////////////////////////////////////////////////////

// Conversion of precipitation mass flux (m/s) to energy flux (kJ/m2/s)
// Unit of result: kJ/m3

// [[Rcpp::export]]
double f_prec (
    const double tempAir,       // Air temperature (°C)
    const double tempAir_crit   // Threshold temp. for rain-/snowfall (°C)
) {
  if (tempAir > tempAir_crit) {
    // 4180. = 1000. * 4.18 = densityWater [kg/m3] * specHeatWater [kJ/kg/K]
    // 333.5e+03. = 1000. * 333.5 = densityWater [kg/m3] * meltHeatIce [kJ/kg]
    return( 4180. * fmax(tempAir, 0.) + 333.5e+03 );
  } else {
    // 2090. = 1000. * 2.09 = densityWater [kg/m3] * specHeatIce [kJ/kg/K]
    return( 2090. * fmin(tempAir, 0.) );
  }
}

// Conversion of sublimation mass flux (m/s) to energy flux (kJ/m2/s)
// Unit of result: kJ/m3
// 2837.e+03 = 1000. * 2837. = densityWater [kg/m3] * sublHeatIce [kJ/kg]

// [[Rcpp::export]]
double f_subl() { return( 2837.e+03 ); }

// Conversion of meltwater loss mass flux (m/s) to energy flux (kJ/m2/s)
// Unit of result: kJ/m3
// 333.5e+03. = 1000. * 333.5 = densityWater [kg/m3] * meltHeatIce [kJ/kg]

// [[Rcpp::export]]
double f_flow() { return ( 333.5e+03 ); }


////////////////////////////////////////////////////////////////////////////////
// General relations for estimation of derived variables
////////////////////////////////////////////////////////////////////////////////

// Fraction of liquid water (mass water / (mass water + mass ice))
// Unit of result: Dimensionless, range 0...1

// [[Rcpp::export]]
double snowLiquidFrac (
    const double snowEnergyCont,  // Snow energy content (kJ/m2)
    const double snowWaterEquiv   // Snow water equivalent (m)
) {
  // 333500. = 333.5 * 1000. = meltHeatOfIce (kJ/kg) * densWater (kg/m3)
  // The result is bounded to the range 0...1.
  // If there is no snow, a fraction of 1 is returned.
  if (snowWaterEquiv > 0.) {
    return( fmin(1., fmax(0., snowEnergyCont / (snowWaterEquiv * 333500.))) );
  } else {
    return( 1. );
  }
}

// Mean temperature of the snow pack
// Unit of result: °C  (Range: -Inf ... 0)

// [[Rcpp::export]]
double snowTemp_mean (
    const double snowEnergyCont,  // Snow energy content (kJ/m2)
    const double snowWaterEquiv,  // Snow water equivalent (m)
    const double soilDepth,       // Depth of interacting soil layer (m)
    const double soilDens,        // Density of soil (kg/m3)
    const double soilSpecHeat     // Spec. heat capacity of soil (kJ/kg/K)
) {
  if (snowWaterEquiv > 0.) {
    // If the snow pack is free of liquid water
    if (snowEnergyCont < 0.) {
      // 2090. = 1000. * 2.09 = WaterDensity (kg/m3) * specHeatCapIce (kJ/kg/K)
      return ( snowEnergyCont / (snowWaterEquiv * 2090. +
               soilDepth * soilDens * soilSpecHeat) );
      // If the snow pack contains some liquid water
    } else {
      return ( 0. );
    }
    // Note: Temperature for the case where all water is liquid is not computed.
    // Note: If there is no snow, a temperature of zero is returned.
  } else {
    return ( 0. );
  }
}

// Snow surface temperature
// Unit of result: °C  (Range: -Inf ... 0)

// [[Rcpp::export]]
double snowTemp_surf (
    const double tempSnow_mean,  // Mean temperature of the snow pack (°C)
    const double tempAir,        // Air temperature (°C)
    const double weightAirTemp   // Weighting param. for air temp. (-) in 0...1
) {
  if (tempSnow_mean < 0.) {
    return( fmin(0., (1.-weightAirTemp) * tempSnow_mean +
            weightAirTemp * tempAir) );
  } else {
    return( 0. );
  }
}



////////////////////////////////////////////////////////////////////////////////
// Definition of rate expressions
////////////////////////////////////////////////////////////////////////////////

//------------------------------------------------------------------------------
// Part 1: Radiation flux rates
//------------------------------------------------------------------------------

// Short-wave radiation balance
// Unit of result: W/m2

// [[Rcpp::export]]
double R_netS (
    const double shortRad,      // Incoming short wave radiation (W/m2)
    const double albedo,        // Albedo (-)
    const double snowWaterEquiv // Snow water equivalent (m)
) {
  if (snowWaterEquiv > 0.) {
    return( shortRad * (1. - albedo) );
  } else {
    return( 0. );
  }
}

// Long-wave radiation balance
// Unit of result: W/m2

// [[Rcpp::export]]
double R_netL (
    const double tempSnow_surf,     // Temperature of the snow surface (°C)
    const double emissivitySnowMin, // Minimum snow emissivity used for old snow (-)
    const double emissivitySnowMax, // Maximum snow emissivity used for new snow (-)
    const double tempAir,           // Air temperature (°C)
    const double relHumid,          // Relative humidity (%)
    const double cloudCoverage,     // Cloud cover (0 = clear sky, 1 = fully covered)
    const double albedo,            // Albedo
    const double albedoMin,         // Minimum albedo used for old snow (-)
    const double albedoMax,         // Maximum albedo used for new snow (-)
    const double snowWaterEquiv     // Snow water equivalent (m)
) {
  if (snowWaterEquiv > 0.) {
    // Outgoing part
    // Note: The snow emissivity decreases with the age of the snow cover (Dyck &
    //       Peschke, 1995). We use the dynamically computed albedo as an indicator
    //       for the age of the snow pack. The constant 0.001 is to prevent a Div0
    //       in case of a constant albedo (i.e. albedoMax = albedoMin).
    // 5.67e-08 = Stefan-Boltzmann constant (W/m2/K4)
    double R_outL = (emissivitySnowMin + (emissivitySnowMax - emissivitySnowMin) *
                     (albedo-albedoMin+0.001) / (albedoMax-albedoMin+0.001)) *
                     5.67e-08 * pow((tempSnow_surf + 273.15), 4.);
    // Incoming part 1 - Clear sky emission
    // This is the Stefan-Bolzmann equation with clear sky emissivity estimated
    // from vaporPressure using the empirical Brunt-formula.
    double R_inL_clear = (0.51 + 0.066 * sqrt(vapPress_overIce(tempAir,relHumid))) * 
      5.67e-08 * pow((tempAir + 273.15), 4.);
    // Incoming part 2 - Cloud emission
    // This is the stefan-Boltzmann equation with the emissivity set to 1 (clouds
    // treated as black body). The cloud temperature is approximated by the dew-
    // point temperature.
    double R_inL_cloud = 1.0 * 5.67e-08 *
      pow((dewTemp_overIce(vapPress_overIce(tempAir,relHumid)) + 273.15), 4.);
    // Radiation balance (net radiation)
    return( (1.-cloudCoverage)*R_inL_clear + cloudCoverage*R_inL_cloud - R_outL);    
  } else {
    return ( 0. );
  }
}

// Soil heat flux
// Unit of result: W/m2

// [[Rcpp::export]]
double R_soil() { return ( 0. ); }

// Sensible heat flux
// Unit of result: W/m2

// [[Rcpp::export]]
double R_sens (
    const double tempSnow_surf,  // Temperature of the snow surface (°C)
    const double tempAir,        // Air temperature (°C)
    const double pressAir,       // Air pressure (hPa)
    const double windSpeed,      // Wind speed (m/s)
    const double a0,             // Empirical coeff. (m/s)
    const double a1,              // Empirical coeff. (-)
    const double snowWaterEquiv     // Snow water equivalent (m)
) {
  if (snowWaterEquiv > 0.) {
    // (a0 + a1 * windSpeed) = Empirical estimation of transfer coefficient (m/s)
    // 1005. = Specific heat capacity of air in J/kg/K
    return ( (a0 + a1 * windSpeed) * densityDryAir(tempAir,pressAir) *
             1005. * (tempAir - tempSnow_surf));
  } else {
    return ( 0. );
  }
}


//------------------------------------------------------------------------------
// Part 2: Mass flux rates
//------------------------------------------------------------------------------

// Precipitation mass flux
// Unit or result: m/s

// [[Rcpp::export]]
double M_prec (
    const double precipSumMM,   // Precipitation sum (mm / referenceInterval)
    const double precipSeconds  // Length of referenceInterval (seconds)
) {
  return ( precipSumMM / 1000. / precipSeconds );
}

// Sublimation mass flux
// Unit or result: m/s

// [[Rcpp::export]]
double M_subl (
    const double tempSnow_surf,  // Temperature of the snow surface (°C)
    const double tempAir,        // Air temperature (°C)
    const double pressAir,       // Air pressure (hPa)
    const double relHumid,       // Relative humidity (%)
    const double windSpeed,      // Wind speed (m/s)
    const double a0,             // Empirical coeff. (m/s)
    const double a1,             // Empirical coeff. (-)
    const double snowWaterEquiv  // Snow water equivalent (m)
) {
  if (snowWaterEquiv > 0.) {
    // (a0 + a1 * windSpeed) = Empirical estimation of transfer coefficient (m/s)
    // 1000. = Density of water (kg/m3)
    // 100. = Relative humidity at saturation (%)
    return (
        fmax(0., // Negative flux rates are set to zero (no freezing air moisture)
             (a0 + a1 * windSpeed) * densityDryAir(tempAir,pressAir) / 1000. *
               (specificHumidity(pressAir, vapPress_overIce(tempSnow_surf,100.)) -
               specificHumidity(pressAir, vapPress_overIce(tempAir,relHumid)))
        )
    );
  } else {
    return ( 0. );
  }
}

// Meltwater flux
// Unit or result: m/s

// [[Rcpp::export]]
double M_flow (
    const double snowLiquidFrac,// Fraction of liquid water (-)
    const double kSatSnow,      // Saturated hydraulic conductivity of snow (m/s)
    const double densDrySnow,   // Density of dry snow (kg/m3)
    const double specCapRet,    // Capill. retent. vol. as fract. of solid SWE (-)
    const double snowWaterEquiv // Snow water equivalent (m)
) {
  if (snowWaterEquiv > 0.) {
    // Relative saturation (-)
    // Don't allow 100% liquid water as this will cause a division by zero
    double rss= (fmin(0.99, snowLiquidFrac)/(1.-fmin(0.99, snowLiquidFrac)) - specCapRet) /
      (1000./densDrySnow - 1000./922. - specCapRet);
    // 1000. = Density of water (kg/m3)
    // 922. = Density of ice (kg/m3)
    // Fix negative values of the relative saturation (-)
    return ( kSatSnow * pow(fmax(0., rss), 3.) );
  } else {
    return ( 0. );
  }
}

//------------------------------------------------------------------------------
// Part 3: Other rates
//------------------------------------------------------------------------------

// Change rate of albedo
// Unit of result: 1/s

// [[Rcpp::export]]
double G_alb (
    const double albedo,           // Current albedo (-)
    const double precipSumMM,      // Precipitation sum (mm / referenceInterval)
    const double precipSeconds,    // Length of referenceInterval (seconds)
    const double tempAir,          // Air temperature (°C)
    const double tempAir_crit,     // Threshold temperature for rain/snow (°C)
    const double albedoMin,        // Albedo of oldest snow (-)
    const double albedoMax,        // Albedo of fresh snow (-)
    const double agingRate_tAirPos, // Aging rate for air temperatures > 0 (1/s)
    const double agingRate_tAirNeg, // Aging rate for air temperatures < 0 (1/s)
    const double snowWaterEquiv     // Snow water equivalent (m)
) {
  if (snowWaterEquiv > 0.) {
    // Surface renewal if snow falls
    // Time of renewal set to the reference interval to keep the change rate reasonably
    // small in comparison to other change rates (prevent stiff ODE system). Complete
    // renewal is assumed for a snowfall of 10 mm (as in Utah Energy Balance Model) and
    // a partly renewal is assumed for less precipitation.
    if ((precipSumMM > 0.) & (tempAir < tempAir_crit)) {
      return ( (albedoMax - albedo) / precipSeconds * fmin(1., precipSumMM/10.) );  // This is positive
      // Surface aging
    } else {
      if (tempAir >= 0.) {
        return ( agingRate_tAirPos * (albedoMin - albedo) );  // This is negative
      } else {
        return ( agingRate_tAirNeg * (albedoMin - albedo) );  // This is negative
      }
    }
  } else {
    return ( 0. );
  }
}










////////////////////////////////////////////////////////////////////////////////
// Derivatives of the snow model's state variables with respect to time
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
NumericVector snowModel_derivs(
    const double precipSumMM,
    const double shortRad,
    const double tempAir,
    const double pressAir,
    const double relHumid,
    const double windSpeed,
    const double cloudCoverage,
    
    const double precipSeconds,
    const double a0,
    const double a1,
    const double kSatSnow,
    const double densDrySnow,
    const double specCapRet,
    const double emissivitySnowMin,
    const double emissivitySnowMax,
    const double tempAir_crit,
    const double albedoMin,
    const double albedoMax, 
    const double agingRate_tAirPos,
    const double agingRate_tAirNeg,
    const double soilDepth,
    const double soilDens,
    const double soilSpecHeat,
    const double weightAirTemp,
    
    const double snowEnergyCont,
    const double snowWaterEquiv,
    const double albedo,
    
    double &ddt_sec, // Result unit: kJ/m2/s
    double &ddt_swe, // Result unit: m/s
    double &ddt_alb, // Result unit: 1/s
    double &flux_melt, // Result unit: m/s, Useful for water balance check
    double &flux_subl // Result unit: m/s, Useful for water balance chec
) {
  // Derived variables
  double TEMP_MEAN= snowTemp_mean(snowEnergyCont, snowWaterEquiv,
                                  soilDepth, soilDens, soilSpecHeat);
  double TEMP_SURF= snowTemp_surf(TEMP_MEAN, tempAir, weightAirTemp);
  double LIQU_FRAC= snowLiquidFrac(snowEnergyCont, snowWaterEquiv);
  // Rate expressions used multiple times
  double M_P= M_prec(precipSumMM, precipSeconds);
  double M_S= M_subl(TEMP_SURF, tempAir, pressAir, relHumid, windSpeed, a0, a1, snowWaterEquiv);
  double M_F= M_flow(LIQU_FRAC, kSatSnow, densDrySnow, specCapRet, snowWaterEquiv);
  // Computation of derivatives
  ddt_sec= 0.001 * (
    R_netS(shortRad, albedo, snowWaterEquiv) +
      R_netL(TEMP_SURF, emissivitySnowMin, emissivitySnowMax, tempAir, relHumid,
             cloudCoverage, albedo, albedoMin, albedoMax, snowWaterEquiv) +
               R_soil() +
               R_sens(TEMP_SURF, tempAir, pressAir, windSpeed, a0, a1, snowWaterEquiv)
  )
    + f_prec(tempAir, tempAir_crit) * M_P
    - f_subl() * M_S
    - f_flow() * M_F;
    ddt_swe= M_P - M_S - M_F;
    ddt_alb= G_alb(albedo, precipSumMM, precipSeconds, tempAir, tempAir_crit,
                   albedoMin, albedoMax, agingRate_tAirPos, agingRate_tAirNeg, snowWaterEquiv);
    flux_melt= M_F;
    flux_subl= M_S;
    
    NumericVector results(4);
    
    // results[0]= snowEnergyCont + ddt_sec * precipSeconds;
    // results[1]= snowWaterEquiv + ddt_swe * precipSeconds;
    // results[2]= albedo         + ddt_alb * precipSeconds;
    
    results[0]= ddt_sec;
    results[1]= ddt_swe;
    results[2]= ddt_alb;
    results[3]= M_F;
    
    // if(results[1] <= 0.0) {
    //   results[0]= 0.0;
    //   results[1]= 0.0;
    //   results[2]= albedoMax;
    // }
    
    return(results);
}



////////////////////////////////////////////////////////////////////////////////
// Function to return the fundamental variables of the snow model
// (for debugging purposes and in-depth analyses of the behaviour)
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
NumericVector snowModel_debug(

    const double precipSumMM,
    const double shortRad,
    const double tempAir,
    const double pressAir,
    const double relHumid,
    const double windSpeed,
    const double cloudCoverage,

    const double precipSeconds,
    const double a0,
    const double a1,
    const double kSatSnow,
    const double densDrySnow,
    const double specCapRet,
    const double emissivitySnowMin,
    const double emissivitySnowMax,
    const double tempAir_crit,
    const double albedoMin,
    const double albedoMax, 
    const double agingRate_tAirPos,
    const double agingRate_tAirNeg,
    const double soilDepth,
    const double soilDens,
    const double soilSpecHeat,
    const double weightAirTemp,

    const double snowEnergyCont,
    const double snowWaterEquiv,
    const double albedo,

    double &TEMP_MEAN,
    double &TEMP_SURF,
    double &LIQU_FRAC,
    double &flux_R_netS,
    double &flux_R_netL,
    double &flux_R_soil,
    double &flux_R_sens,
    double &stoi_f_prec,
    double &stoi_f_subl,
    double &stoi_f_flow,
    double &flux_M_prec,
    double &flux_M_subl,
    double &flux_M_flow,
    double &rate_G_alb,
    double &ddt_sec, // Result unit: kJ/m2/s
    double &ddt_swe, // Result unit: m/s
    double &ddt_alb // Result unit: 1/s
) {
  // Derived variables
  TEMP_MEAN= snowTemp_mean(snowEnergyCont, snowWaterEquiv,
                           soilDepth, soilDens, soilSpecHeat);
  TEMP_SURF= snowTemp_surf(TEMP_MEAN, tempAir, weightAirTemp);
  LIQU_FRAC= snowLiquidFrac(snowEnergyCont, snowWaterEquiv);
  // Mass fluxes
  flux_M_prec= M_prec(precipSumMM, precipSeconds);
  flux_M_subl= M_subl(TEMP_SURF, tempAir, pressAir, relHumid, windSpeed, a0, a1, snowWaterEquiv);
  flux_M_flow= M_flow(LIQU_FRAC, kSatSnow, densDrySnow, specCapRet, snowWaterEquiv);
  // Radiation fluxes
  flux_R_netS= R_netS(shortRad, albedo, snowWaterEquiv);
  flux_R_netL= R_netL(TEMP_SURF, emissivitySnowMin, emissivitySnowMax, tempAir,
                      relHumid, cloudCoverage, albedo, albedoMin, albedoMax, snowWaterEquiv);
  flux_R_soil= R_soil();
  flux_R_sens= R_sens(TEMP_SURF, tempAir, pressAir, windSpeed, a0, a1, snowWaterEquiv);
  // Stoichiometry factors
  stoi_f_prec= f_prec(tempAir, tempAir_crit);
  stoi_f_subl= f_subl();
  stoi_f_flow= f_flow();
  // Other rates
  rate_G_alb= G_alb(albedo, precipSumMM, precipSeconds, tempAir, tempAir_crit,
                    albedoMin, albedoMax, agingRate_tAirPos, agingRate_tAirNeg, snowWaterEquiv);
  // Computation of derivatives
  ddt_sec= 0.001 * (
    R_netS(shortRad, albedo, snowWaterEquiv) +
      R_netL(TEMP_SURF, emissivitySnowMin, emissivitySnowMax, tempAir, relHumid,
             cloudCoverage, albedo, albedoMin, albedoMax, snowWaterEquiv) +
               R_soil() +
               R_sens(TEMP_SURF, tempAir, pressAir, windSpeed, a0, a1, snowWaterEquiv)
  )
    + f_prec(tempAir, tempAir_crit) * flux_M_prec
    - f_subl() * flux_M_subl
    - f_flow() * flux_M_flow;
    ddt_swe= flux_M_prec - flux_M_subl - flux_M_flow;
    ddt_alb= G_alb(albedo, precipSumMM, precipSeconds, tempAir, tempAir_crit,
                   albedoMin, albedoMax, agingRate_tAirPos, agingRate_tAirNeg, snowWaterEquiv);
  
  // Export results
  NumericVector results(17);
  
  results[0]= ddt_sec;
  results[1]= ddt_swe;
  results[2]= ddt_alb;
  results[3]= TEMP_MEAN;
  results[4]= TEMP_SURF;
  results[5]= LIQU_FRAC;
  results[6]= flux_M_prec;
  results[7]= flux_M_subl;
  results[8]= flux_M_flow;
  results[9]= flux_R_netS;
  results[10]= flux_R_netL;
  results[11]= flux_R_soil;
  results[12]= flux_R_sens;
  results[13]= stoi_f_prec;
  results[14]= stoi_f_subl;
  results[15]= stoi_f_flow;
  results[16]= rate_G_alb;
  
  return(results);
  
}

////////////////////////////////////////////////////////////////////////////////
// Function to return the fundamental variables of the snow model
// Run in 24 intermediate time steps
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
NumericVector snowModel_inter(
    
    const double precipSumMM,
    const double shortRad,
    const double tempAir,
    const double pressAir,
    const double relHumid,
    const double windSpeed,
    const double cloudCoverage,
    
    const double precipSeconds,
    const double a0,
    const double a1,
    const double kSatSnow,
    const double densDrySnow,
    const double specCapRet,
    const double emissivitySnowMin,
    const double emissivitySnowMax,
    const double tempAir_crit,
    const double albedoMin,
    const double albedoMax, 
    const double agingRate_tAirPos,
    const double agingRate_tAirNeg,
    const double soilDepth,
    const double soilDens,
    const double soilSpecHeat,
    const double weightAirTemp,
    const double tempMaxOff,
    const double tempAmpli,
    
    const double snowEnergyCont,
    const double snowWaterEquiv,
    const double albedo,
    
    double &TEMP_MEAN,
    double &TEMP_SURF,
    double &LIQU_FRAC,
    double &flux_R_netS,
    double &flux_R_netL,
    double &flux_R_soil,
    double &flux_R_sens,
    double &stoi_f_prec,
    double &stoi_f_subl,
    double &stoi_f_flow,
    double &flux_M_prec,
    double &flux_M_subl,
    double &flux_M_flow,
    double &rate_G_alb
) {
  
  // 24 intermediate steps to imitate daily cycle
  // original daily resolution to course
  NumericVector temps_cyc(24);
  NumericVector precs_cyc(24);
  
  // Daily cycle temperature 
  double pi = 3.1415926535;
  for( int i = 0; i < 24; i = i + 1 ) {
    temps_cyc[i] = tempAir + sin(-pi/2 + (i+1 - tempMaxOff) * (2*pi/24)) * tempAmpli/2;
  }
  
  // Precipitation distributed among intermediate time steps
  for( int i = 0; i < 24; i = i + 1 ) {
    precs_cyc[i] = precipSumMM  / 24;
  }
  
  // Radiation
  // ...
  
  double snowEnergyCont_new     =     snowEnergyCont;
  double snowWaterEquiv_new     =     snowWaterEquiv;
  double albedo_new             =     albedo;
  TEMP_MEAN              =     0.;
  TEMP_SURF              =     0.;
  LIQU_FRAC              =     0.;
  flux_R_netS            =     0.;
  flux_R_netL            =     0.;
  flux_R_soil            =     0.;
  flux_R_sens            =     0.;
  stoi_f_prec            =     0.;
  stoi_f_subl            =     0.;
  stoi_f_flow            =     0.;
  flux_M_flow            =     0.;
  flux_M_subl            =     0.;
  flux_M_prec            =     0.;
  rate_G_alb             =     0.;
  NumericVector results(17);
  
  for( int i = 0; i < 24; i = i + 1 ) {
  
  // Derived variables
  double TEMP_MEAN_calc= snowTemp_mean(snowEnergyCont_new, snowWaterEquiv_new,
                           soilDepth, soilDens, soilSpecHeat);
    double TEMP_SURF_calc= snowTemp_surf(TEMP_MEAN_calc, temps_cyc[i], weightAirTemp);
    double LIQU_FRAC_calc= snowLiquidFrac(snowEnergyCont_new, snowWaterEquiv_new);
  // Mass fluxes
  double flux_M_prec_calc= M_prec(precs_cyc[i], precipSeconds/24);
  double flux_M_subl_calc= M_subl(TEMP_SURF_calc, temps_cyc[i], pressAir, relHumid, windSpeed, a0, a1, snowWaterEquiv_new);
  double flux_M_flow_calc= M_flow(LIQU_FRAC_calc, kSatSnow, densDrySnow, specCapRet, snowWaterEquiv_new);
  // Radiation fluxes
  double flux_R_netS_calc= R_netS(shortRad, albedo_new, snowWaterEquiv_new);
  double flux_R_netL_calc= R_netL(TEMP_SURF_calc, emissivitySnowMin, emissivitySnowMax, temps_cyc[i],
                      relHumid, cloudCoverage, albedo_new, albedoMin, albedoMax, snowWaterEquiv_new);
  double flux_R_soil_calc= R_soil();
  double flux_R_sens_calc= R_sens(TEMP_SURF_calc, temps_cyc[i], pressAir, windSpeed, a0, a1, snowWaterEquiv_new);
  // Stoichiometry factors
  double stoi_f_prec_calc= f_prec(temps_cyc[i], tempAir_crit);
  double stoi_f_subl_calc= f_subl();
  double stoi_f_flow_calc= f_flow();
  // Other rates
  double rate_G_alb_calc= G_alb(albedo_new, precs_cyc[i], precipSeconds/24, temps_cyc[i], tempAir_crit,
                    albedoMin, albedoMax, agingRate_tAirPos, agingRate_tAirNeg, snowWaterEquiv_new);
  // Computation of derivatives
  double ddt_sec= 0.001 * (
    R_netS(shortRad, albedo_new, snowWaterEquiv_new) +
      R_netL(TEMP_SURF_calc, emissivitySnowMin, emissivitySnowMax, temps_cyc[i], relHumid,
             cloudCoverage, albedo_new, albedoMin, albedoMax, snowWaterEquiv_new) +
               R_soil() +
               R_sens(TEMP_SURF_calc, temps_cyc[i], pressAir, windSpeed, a0, a1, snowWaterEquiv_new)
  )
    + f_prec(temps_cyc[i], tempAir_crit) * flux_M_prec_calc
    - f_subl() * flux_M_subl_calc
    - f_flow() * flux_M_flow_calc;
  double  ddt_swe= flux_M_prec_calc - flux_M_subl_calc - flux_M_flow_calc;
  double  ddt_alb= G_alb(albedo_new, precs_cyc[i], precipSeconds/24, temps_cyc[i], tempAir_crit,
                   albedoMin, albedoMax, agingRate_tAirPos, agingRate_tAirNeg, snowWaterEquiv_new);
    
    //  Update results
    snowEnergyCont_new = snowEnergyCont_new   +   ddt_sec * precipSeconds/24;
    snowWaterEquiv_new = snowWaterEquiv_new   +   ddt_swe * precipSeconds/24;
    albedo_new         = albedo_new           +   ddt_alb * precipSeconds/24;
    TEMP_MEAN              =     TEMP_MEAN            +   TEMP_MEAN_calc/24;
    TEMP_SURF              =     TEMP_SURF            +   TEMP_SURF_calc/24;
    LIQU_FRAC              =     LIQU_FRAC            +   LIQU_FRAC_calc/24;
    flux_R_netS            =     flux_R_netS          +   flux_R_netS_calc/24;
    flux_R_netL            =     flux_R_netL          +   flux_R_netL_calc/24;
    flux_R_soil            =     flux_R_soil          +   flux_R_soil_calc/24;
    flux_R_sens            =     flux_R_sens          +   flux_R_sens_calc/24;
    stoi_f_prec            =     stoi_f_prec          +   stoi_f_prec_calc/24;
    stoi_f_subl            =     stoi_f_subl          +   stoi_f_subl_calc/24;
    stoi_f_flow            =     stoi_f_flow          +   stoi_f_flow_calc/24;
    flux_M_flow            =     flux_M_flow          +   flux_M_flow_calc/24;
    flux_M_subl            =     flux_M_subl          +   flux_M_subl_calc/24;
    flux_M_prec            =     flux_M_prec          +   flux_M_prec_calc/24;
    rate_G_alb             =     rate_G_alb           +   rate_G_alb_calc/24;
    
    // Correct if SWE is 0
    if(snowWaterEquiv_new <=  0.) {
      snowEnergyCont_new = 0.;
      snowWaterEquiv_new = 0.;
      albedo_new         = albedoMax;
    }
    
    // Export results
    if(i >=  23) {
      
      results[0]= snowEnergyCont_new;
      results[1]= snowWaterEquiv_new;
      results[2]= albedo_new;
      results[3]= TEMP_MEAN;
      results[4]= TEMP_SURF;
      results[5]= LIQU_FRAC;
      results[6]= flux_M_prec;
      results[7]= flux_M_subl;
      results[8]= flux_M_flow;
      results[9]= flux_R_netS;
      results[10]= flux_R_netL;
      results[11]= flux_R_soil;
      results[12]= flux_R_sens;
      results[13]= stoi_f_prec;
      results[14]= stoi_f_subl;
      results[15]= stoi_f_flow;
      results[16]= rate_G_alb;
      
    }
    
    
  }
    
    return(results);
    
}

#endif