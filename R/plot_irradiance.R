#' Plot Sensor and Horizontal Irradiance
#'
#' This function plots the Sensor and Horizontal Irradiance against Date2.
#'
#' @param xmp_corrected A data frame containing the corrected irradiance values.
#' @param band_name The band name to filter ("Blue" is default).
#' @return A ggplot2 plot.
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_irradiance(xmp_corrected, "Blue")
#' }
plot_irradiance <- function(xmp_corrected, band_name = "Blue") {
  plot <- xmp_corrected %>%
    filter(BandName == band_name) %>%
    ggplot(aes(x = Date2, y = SensorIrradiance)) +
    geom_point(size = 1) +
    geom_point(aes(y = HorizontalIrradiance_new), color = "red4", size = 1) +
    geom_hline(yintercept = 0) +
    theme_bw() +
    facet_wrap(. ~ Date, scales = "free", ncol = 3)

  return(plot)
}
