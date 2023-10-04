#' Plot Sun-Sensor Angles
#'
#' This function takes a data frame containing the sun-sensor angle and plots it to
#' check if the angles are within a reasonable range.
#'
#' @param df A data frame that contains the Date, BandName, Date2, and SunSensorAngle columns.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' plot_sun_sensor_angles(xmp_all_ssa)
#' }
plot_sun_sensor_angles <- function(df) {

  # Filter data and plot
  p <- df %>%
    dplyr::filter(BandName == "Blue") %>%  # Identical across bands in a rig, just check one
    dplyr::select('Date', 'BandName', 'Date2', 'SunSensorAngle') %>%
    dplyr::group_by(Date, BandName) %>%
    ggplot2::ggplot(ggplot2::aes(x = Date, y = pracma::rad2deg(SunSensorAngle))) +
    ggplot2::geom_point(color = "red4", alpha = .5) +
    ggplot2::labs(x = 'Time (UTC)', y = 'Sun Sensor Angle (°)') +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::theme(legend.position = "none")

  return(p)

}
