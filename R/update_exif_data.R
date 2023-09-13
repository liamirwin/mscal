#' Update EXIF Data with Corrected and Original Values
#'
#' This function updates the EXIF data of image files with corrected values
#' while also preserving the original values as new attributes.
#'
#' @param site_dir The directory path where the site-specific data is stored.
#' @return NULL. The function updates the EXIF data in place.
#' @examples
#' \dontrun{
#' update_exif_data("C:/path/to/site_dir")
#' }
#' @export
update_exif_data <- function(site_dir) {
  # Construct the full path to the corrected values file
  corrected_values_path <- glue::glue("{site_dir}/CSV/Corrected_values/xmp_corrected.rds")

  # Read corrected values
  xmp_corrected = readRDS(corrected_values_path) %>%
    mutate(TargetFile = str_replace(SourceFile, "_save", ""))

  # Extract vectors
  targets = xmp_corrected$TargetFile
  SSA = xmp_corrected$SunSensorAngle
  horirrorig = xmp_corrected$HorizontalIrradiance
  horirr = xmp_corrected$HorizontalIrradiance_new
  dirirr = xmp_corrected$DirectIrradiance_new
  scairr = xmp_corrected$ScatteredIrradiance_new

  for (i in seq_along(targets)) {
    # Save original values as new EXIF attributes (e.g., OriginalSunSensorAngle)
    original_call = glue::glue("-config {site_dir}/MicaSense.config",
                               " -OriginalSunSensorAngle={SSA[i]}",
                               " -OriginalHorizontalIrradiance={horirrorig[i]}",
                               " {targets[i]}")

    exiftool_call(original_call, quiet = TRUE)

    # Overwrite tags with computed values
    corrected_call = glue::glue("-config {site_dir}/MicaSense.config",
                                " -overwrite_original_in_place",
                                " -SunSensorAngle={SSA[i]}",
                                " -HorizontalIrradiance={horirr[i]}",
                                " -HorizontalIrradianceDLS2={horirrorig[i]}",
                                " -DirectIrradiance={dirirr[i]}",
                                " -ScatteredIrradiance={scairr[i]}",
                                " {targets[i]}")

    exiftool_call(corrected_call, quiet = TRUE)
    print(glue::glue("{i}/{length(targets)} updated"))
  }
}
