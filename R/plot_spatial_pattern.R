#' Plot Spatial Pattern of Sun Sensor Angles
#'
#' This function generates a ggplot to visualize the spatial pattern of Sun Sensor Angles.
#' It's a scatter plot with the longitude and latitude as coordinates, and the sun sensor angle as the color.
#'
#' @param df A data frame containing the necessary variables, including `GPSLongitude`, `GPSLatitude`, `SunSensorAngle`, and `Date`.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_spatial_pattern(df = your_df)
#' }
plot_spatial_pattern <- function(df) {

  p <- df %>%
    ggplot(aes(x = GPSLongitude, y = GPSLatitude, color = pracma::rad2deg(SunSensorAngle))) +
    geom_point(aes(color = ifelse(is.na(pracma::rad2deg(SunSensorAngle)), "grey60", pracma::rad2deg(SunSensorAngle)))) +  # Color points based on SunSensorAngle, grey if SunSensorAngle is NA
    geom_point(size = 3) +
    theme_bw() +
    scale_color_viridis_c()
    #facet_wrap(. ~ Date, scales = "free")  # Facets by Date

  return(p)
}
