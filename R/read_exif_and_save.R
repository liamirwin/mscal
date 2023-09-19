#' Read EXIF data and save it
#'
#' @name read_exif_and_save
#' @param site_dir The directory where the site's data is located.
#' @param num_cores Number of cores to run in parallel (default is 1 - serial)
#' @return Nothing, but saves EXIF data to RDS files.
#' @export
#'
#' @examples
#' \dontrun{
#' read_exif_and_save(site_dir = "your_site_directory")
#' }
read_exif_and_save <- function(site_dir, num_cores = 1L) {

  tictoc::tic()

  # Register parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  # Initialize an empty dataframe
  df_all = data.frame()

  for (j in 1:10) {

    pics = list.files(glue("{site_dir}/DAP/MS/"),
                      pattern = c(glue("IMG_...._{j}.tif"), glue("IMG_...._{j}_.tif")),
                      recursive = TRUE,
                      full.names = TRUE)

    print(paste0("Found ", length(pics), " pictures."))

    pics_root = list.files(glue("{site_dir}/DAP/MS/"),
                           pattern = c(glue("IMG_...._{j}.tif"), glue("IMG_...._{j}_.tif")),
                           recursive = TRUE,
                           full.names = FALSE)

    exif_df = data.frame()

    exif_df_list <- foreach(i = 1:length(pics), .combine = "rbind", .packages = c("exifr", "dplyr", "glue")) %dopar% {

      pic = pics[i]
      pic_root = tail(unlist(strsplit(pics_root[i], "Panels/")), n = 1)
      pan_flag = length(unlist(strsplit(pics_root[i], "Panels/"))) - 1

      if (substr(pic_root, 11, 11) == "0") {
        band = substr(pic_root, 10, 11)
      } else {
        band = substr(pic_root, 10, 10)
      }

      img_exif = exifr::read_exif(pic)

      exif_df_temp = as.data.frame(img_exif)
      exif_df_temp = dplyr::mutate(exif_df_temp, panel_flag = pan_flag)

      return(exif_df_temp)
    }

    # Combine the list into a dataframe
    exif_df = do.call(rbind, exif_df_list)

    saveRDS(exif_df, glue("{site_dir}/CSV/Corrected_values/df_{j}.rds"))
  }

  print(glue('Read and locally saved EXIF data for all ten bands'))

  # Stop the cluster
  stopCluster(cl)

  tictoc::toc()
}
