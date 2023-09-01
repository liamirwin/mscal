#' Calculate Aggregate Ratios and Save as RDS File
#'
#' This function calculates aggregate ratios such as mean scattered ratio and directional difference ratio.
#' The function also calculates the mean latitude and longitude, and mean Date for each unique Date in the data frame.
#' The results are saved as an RDS file in the specified directory.
#'
#' @param df A data frame containing the variables `Date`, `mean_scattered`, `dir_diff_ratio`, `GPSLatitude`, `GPSLongitude`, and `Date2`.
#' @param site_dir The directory path where the RDS file should be saved.
#'
#' @return A data frame with aggregated values of mean_scattered, dir_diff_ratio, Lat_mean, Long_mean, and Date_mean.
#' @export
#'
#' @examples
#' \dontrun{
#'   aggregate_ratios(df = your_df, site_dir = "your_directory_path_here")
#' }
aggregate_ratios <- function(df, site_dir) {

  tictoc::tic()

  ratios <- df %>%
    dplyr::select(Date, mean_scattered, dir_diff_ratio,
                  GPSLatitude, GPSLongitude, Date2) %>%
    dplyr::group_by(Date) %>%
    dplyr::mutate(Lat_mean = mean(GPSLatitude),
           Long_mean = mean(GPSLongitude),
           Date_mean = mean(Date2)) %>%
    distinct(Date, mean_scattered, dir_diff_ratio, Lat_mean, Long_mean, Date_mean)

  # Save the resulting data frame as an RDS file in the specified directory
  saveRDS(ratios, glue::glue("{site_dir}\\CSV\\Corrected_values\\ratios.rds"))

  print(glue::glue('Computed aggregate ratios for {basename(site_dir)}'))

  tictoc::toc()

  return(ratios)
}
