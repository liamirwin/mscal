#' Compute Corrected Horizontal Irradiance
#'
#' This function computes the corrected horizontal irradiance for each image, considering the Fresnel
#' correction and the direct and scattered components of irradiance.
#'
#' @param xmp_all_ssa A data frame containing the needed variables including `Date`, `BandName`, `Irradiance`, etc.
#' @param ratios A data frame containing the ratios needed for the correction.
#'
#' @return A data frame containing the corrected irradiance values.
#' @export
#'
#' @examples
#' \dontrun{
#'   corrected_irradiance_df <- compute_corrected_irradiance(xmp_all_ssa = your_data_frame_here, ratios = your_ratios_here)
#' }
compute_corrected_irradiance <- function(xmp_all_ssa, ratios) {

  # Start the timer
  tictoc::tic()

  # Group data by date and band name
  # Nest the Irradiance and SunSensorAngle for each group
  # Compute the Fresnel correction for each nested data frame
  xmp_corrected <- xmp_all_ssa %>%
    group_by(Date, BandName) %>%
    nest(data = c(Irradiance, SunSensorAngle)) %>%
    mutate(Fresnel = as.numeric(map(.x = data, .f = fresnel_correction))) %>%
    unnest(data)

  # Join the computed ratios by Date
  # Perform the calculations for sensor, direct, and scattered irradiances
  # Apply corrections to account for Fresnel effect, sensor angle, and other factors
  xmp_corrected <- xmp_corrected %>%
    left_join(ratios, by = "Date") %>%
    mutate(SensorIrradiance = as.numeric(SpectralIrradiance) / Fresnel, # Adjust for reflected light from the DLS diffuser
           DirectIrradiance_new = SensorIrradiance / (dir_diff_ratio + cos(as.numeric(SunSensorAngle))), # Adjust for sun angle
           HorizontalIrradiance_new = DirectIrradiance_new * (dir_diff_ratio + sin(as.numeric(SolarElevation))), # Compute horizontal irradiance
           ScatteredIrradiance_new = HorizontalIrradiance_new - DirectIrradiance_new) # Compute scattered irradiance

  # Stop the timer and print the elapsed time
  tictoc::toc()

  return(xmp_corrected)
}
