#' Compute Fresnel Correction for Downwelling Light Sensor (DLS)
#'
#' This function calculates the Fresnel correction factor for irradiance measured by
#' a Downwelling Light Sensor (DLS). It uses the function `multilayer_transmission` to
#' apply Fresnel equations for multilayer transmission.
#'
#' @param x A data frame or a list containing at least the columns/elements `Irradiance` and `SunSensorAngle`.
#'          `Irradiance` is the measured irradiance value, and `SunSensorAngle` is the angle between
#'          the sun and the sensor, measured in radians.
#'
#' @return The Fresnel transmission coefficient, which is a correction factor that can be
#'         applied to the raw irradiance measurements.
#' @export
#'
#' @examples
#' fresnel_correction(data.frame(Irradiance = c(1000, 1100), SunSensorAngle = c(0.1, 0.2)))
fresnel_correction <- function(x) {

  # Extract relevant variables from the input data frame or list
  Irradiance = x$Irradiance
  SunSensorAngle = x$SunSensorAngle

  # Define constants
  n1 = 1.000277  # Refractive index of the first medium (usually air)
  n2 = 1.38  # Refractive index of the second medium (usually the DLS material)
  polarization = c(0.5, 0.5)  # Proportions of s-polarization and p-polarization

  # Convert sun-sensor angle from radians to degrees (if needed)
  SunSensorAngle_deg <- SunSensorAngle * (180 / pi)

  # Perform the multilayer Fresnel correction using the helper function
  Fresnel = multilayer_transmission(SunSensorAngle, c(n1, n2), polarization)

  # Return the Fresnel correction factor
  return(Fresnel)
}
