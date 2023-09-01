#' Plot Geolocation against Sun Sensor Angle
#'
#' This function plots the GPSLongitude and GPSLatitude colored by SunSensorAngle.
#'
#' @param xmp_corrected A data frame containing the corrected irradiance values.
#' @param band_name The band name to filter ("NIR" is default).
#' @return A ggplot2 plot.
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_geolocation(xmp_corrected, "NIR")
#' }
plot_geolocation <- function(xmp_corrected, band_name = "NIR") {
  plot <- xmp_corrected %>%
    filter(BandName == band_name) %>%
    ggplot(aes(x = GPSLongitude, y = GPSLatitude, color = rad2deg(SunSensorAngle))) +
    geom_point(size = 4) +
    theme_bw() +
    scale_color_viridis_c() +
    facet_wrap(. ~ Date, scales = "free")

  return(plot)
}
