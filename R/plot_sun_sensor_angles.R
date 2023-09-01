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
#' plot_sun_sensor_angles(xmp_all_ssa)
plot_sun_sensor_angles <- function(df) {

  # Filter data and plot
  p <- df %>%
    filter(BandName == "Blue") %>%  # Identical across bands in a rig, just check one
    group_by(Date, BandName) %>%
    ggplot(aes(x = Date2, y = rad2deg(SunSensorAngle))) +
    geom_point(color = "red4", alpha = .5) +
    theme_bw(base_size = 16) +
    facet_wrap(. ~ Date,
               scales = "free_x")

  return(p)
}
