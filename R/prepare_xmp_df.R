#' Clean and Format XMP Dataframe
#'
#' This function performs several operations to estimate scattered:direct ratio for each flight.
#'
#' @param site_dir The project directory where the input RDS file are located in /CSV/Corrected_values/XMP_with_SSA.rds
#' @return A data frame containing computed values and models.
#'
#' @examples
#' \dontrun{
#' prepare_xmp_df("your_directory_here")
#' }
prepare_xmp_df <- function(site_dir) {

  tictoc::tic()

  # Construct path to the RDS file
  rds_path <- file.path(site_dir, "CSV", "Corrected_values", "XMP_with_SSA.rds")

  # Read the XMP Data with SunSensorAngle
  xmp_all_ssa <- readr::read_rds(rds_path)

  # Not sure what is going on here with the panel flag business...?
  xmp_all_ssa <- xmp_all_ssa %>%
    dplyr::mutate(Date = lubridate::ymd_hms(DateTimeOriginal, tz = 'UTC'),
                  subdirectory = stringr::str_split_i(Directory, "/", i = 2),
                  panel_flag = dplyr::if_else(subdirectory == "Panels", 1, 0))

  # Read the XMP data with SunSensorAngle
  xmp_all_ssa <- xmp_all_ssa %>%
    dplyr::mutate(Yaw_deg = pracma::rad2deg(as.numeric(Yaw)),
                  Roll_deg = pracma::rad2deg(as.numeric(Roll)),
                  Pitch_deg = pracma::rad2deg(as.numeric(Pitch)))

  # Clean up other columns
  xmp_all_ssa <- xmp_all_ssa %>%
    dplyr::group_by(Date, BandName) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(GPSLatitude_plot = scale(as.numeric(GPSLatitude)),
                  GPSLongitude_plot = scale(as.numeric(GPSLongitude)),
                  cos_SSA = cos(SunSensorAngle),
                  Irradiance = as.numeric(Irradiance),
                  Date2 = Date)

  print(glue::glue('Prepared {site_dir} Dataframe for Processing...'))

  tictoc::toc()

  return(xmp_all_ssa)
}
