#' Generate Scattered Light Ratio Plot
#'
#' This function generates a ggplot to visualize the rolling regression results. It plots the percent of scattered light over time.
#' The color of the points represents the R-squared values of the linear models used in the rolling regression.
#'
#' @param df A data frame containing the necessary variables, including `Date2`, `percent_scattered`, and `R2`.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#'   plot_scattered_light(df = your_df)
#' }
plot_scattered_light <- function(df) {
  p <- df %>%
    group_by(Date, BandName) %>%
    ggplot(aes(x = Date2, y = percent_scattered, color = R2)) +
    geom_point(aes(color = ifelse(is.na(R2), "grey", R2))) +  # Color points based on R2, grey if R2 is NA
    geom_hline(yintercept = 1, linetype = 2) +  # Dotted line at y=1
    geom_hline(yintercept = 0, linetype = 2) +  # Dotted line at y=0
    geom_hline(aes(yintercept = mean_scattered), color = "red4", linewidth = 1) +  # Red line for mean
    scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0,1)) +  # Y-axis scale
    ggnewscale::new_scale_color() +
    theme_bw(base_size = 16) +  # Theme
    facet_wrap(. ~ Date, scales = "free")  # Facets

  return(p)
}
