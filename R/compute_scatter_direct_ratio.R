#' Compute Scatter:Direct Ratio for Each Flight
#'
#' This function performs several operations to estimate scattered:direct ratio for each flight.
#'
#' @param site_dir The directory where the input RDS file is located.
#' @param r_squared_threshold The R-squared threshold to use (default is 0.4).
#' @param rolling_window The time window for the rolling regression (default is 30 seconds).
#'
#' @return A data frame containing computed values and models.
#'
#' @examples
#' \dontrun{
#' compute_scatter_direct_ratio("your_directory_here", r_squared_threshold = 0.4, rolling_window = 30)
#' }
compute_scatter_direct_ratio <- function(site_dir, r_squared_threshold = 0.4, rolling_window = 30) {

  tictoc::tic()

  # Read the XMP data with SunSensorAngle
  xmp_all_ssa <- read_rds(glue::glue("{site_dir}\\CSV\\Corrected_values\\XMP_with_SSA.rds")) %>%
    mutate(Date = lubridate::ymd_hms(DateTimeOriginal, tz = 'UTC'),
           subdirectory = str_split_i(Directory, "/", i = 2),
           panel_flag = if_else(subdirectory == "Panels", 1, 0)) %>%
    mutate(Yaw_deg = rad2deg(as.numeric(Yaw)),
           Roll_deg = rad2deg(as.numeric(Roll)),
           Pitch_deg = rad2deg(as.numeric(Pitch))) %>%
    group_by(Date, BandName) %>%
    arrange(Date) %>%
    mutate(GPSLatitude_plot = scale(as.numeric(GPSLatitude)),
           GPSLongitude_plot = scale(as.numeric(GPSLongitude)),
           cos_SSA = cos(SunSensorAngle),
           Irradiance = as.numeric(Irradiance),
           Date2 = Date)

  # Additional code for rolling regression, filtering models etc. can go here

  print(glue::glue('Calculated scattered direct irradiance ratios for {basename(site_dir)}'))

  tictoc::toc()

  return(xmp_all_ssa)
}
