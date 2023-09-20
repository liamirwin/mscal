#' Compute sun angle and save the results as an RDS file
#'
#' This function reads an RDS file containing XMP data, computes the sun angle based on SolarElevation,
#' SolarAzimuth, Roll, Pitch, and Yaw, and then saves the result as an RDS file.
#'
#' @param site_dir The site directory where the input and output RDS files are located in /CSV/Corrected_values.
#'
#' @return None. This function saves the computed data as an RDS file.
#'
#' @examples
#' \dontrun{
#' compute_and_save_sun_angle("your_directory_here")
#' }
compute_and_save_sun_angle <- function(site_dir) {

  tictoc::tic()

  # Read the XMP data
  xmp_data <- readr::read_rds(glue::glue("{site_dir}\\CSV\\Corrected_values\\XMP_all.rds"))

  # Compute Sun Sensor Angle
  xmp_with_ssa <- xmp_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(SunSensorAngle = compute_sun_angle(SolarElevation,
                                                     SolarAzimuth,
                                                     Roll,
                                                     Pitch,
                                                     Yaw))

  # Save the result as an RDS file
  saveRDS(xmp_with_ssa,
                   file = glue::glue("{site_dir}\\CSV\\Corrected_values\\XMP_with_SSA.rds"))

  print(glue::glue('Computed sun angle and updated RDS files for {site_dir}'))

  tictoc::toc()

  return(xmp_with_ssa)

}
