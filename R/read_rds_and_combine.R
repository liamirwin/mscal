#' Combine RDS Files Into a Single DataFrame
#'
#' This function reads in multiple RDS files based on a list of dates and band numbers,
#' and combines them into a single dataframe.
#'
#' @param site_dir The directory where the image files and rds are located.
#'
#' @return A data frame containing all combined EXIF data.
#'
#' @examples
#' \dontrun{
#' result <- read_rds_and_combine(dir = "your_directory", site_dir = "your_site_directory")
#' }
#' @export
read_rds_and_combine <- function(site_dir) {

  # Initialize empty data frame to store all EXIF data
  exif_df_all = data.frame()

  for (j in 1:10) {
    # Assumes bands are from 1 to 10; adjust if needed
    # Construct the RDS file path and read it
    rds_file_path = glue::glue("{site_dir}/CSV/Corrected_values/df_{j}.rds")
    exif_df = read_rds(rds_file_path)

    # Combine the data frames
    if (j == 1) {
      exif_df_all = as.data.frame(exif_df)
    } else {
      exif_df_all = dplyr::bind_rows(exif_df_all, exif_df)
    }
  }

  saveRDS(exif_df_all, glue::glue("{site_dir}/CSV/Corrected_values/XMP_all.rds"))

  print(glue::glue('Read and combine RDS files for {site_dir}'))

  return(exif_df_all)
}
