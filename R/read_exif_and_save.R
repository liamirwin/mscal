#' Read XMP data into R format and save as RDS files.
#'
#' @param site_dir The directory where the image files are located.
#' @param parallel Whether to run the function in parallel. Default is FALSE.
#' @param cores Number of cores to use for parallel processing. Default is 2.
#' @return Nothing, but saves .RDS files with exif data in specified directories.
#' @examples
#' read_exif_and_save('D:/Agisoft/ON/Block18_N_MX')
#' @export

library(furrr)
library(progressr)
library(glue)
library(dplyr)

read_exif_and_save <- function(site_dir, parallel = FALSE, cores = 2) {

  # Initialize an empty dataframe
  df_all = data.frame()

  if(parallel) {
    # Setting up parallel processing
    future::plan(future::multisession, workers = cores)
    print(glue::glue('Initialized parallel processing for {site_dir} with {cores} CPU cores'))
  }

  # Initialize a progress bar if running in parallel
  if(parallel) {
    handlers(global = TRUE)
    p <- progressor(steps = 10)
  }

  # Choose the correct map function
  map_func <- ifelse(parallel, furrr::future_map, purrr::map)

  # Run
  map_func(1:10, ~{

    if(parallel) {
      p()  # Update the progress bar
    }

    j <- .x

    pics = list.files(glue::glue("{site_dir}/DAP/MS/"),
                      pattern = c(glue::glue("IMG_...._{j}.tif"), glue::glue("IMG_...._{j}_.tif")),
                      recursive = TRUE,
                      full.names = TRUE)

    pics_root = list.files(glue::glue("{site_dir}/DAP/MS/"),
                           pattern = c(glue::glue("IMG_...._{j}.tif"), glue::glue("IMG_...._{j}_.tif")),
                           recursive = TRUE,
                           full.names = FALSE)

    exif_df = data.frame()

    for (i in seq_along(pics)) {
      pic = pics[i]
      pic_root = tail(unlist(strsplit(pics_root[i], "Panels/")), n = 1)
      pan_flag = length(unlist(strsplit(pics_root[i], "Panels/"))) - 1

      if (substr(pic_root, 11, 11) == "0") {
        band = substr(pic_root, 10, 11)
      } else {
        band = substr(pic_root, 10, 10)
      }

      img_exif = read_exif(pic)
      print(glue::glue("band {j} {i}/{length(pics)}"))

      if (i == 1) {
        exif_df = as.data.frame(img_exif) %>% mutate(panel_flag = pan_flag)
      } else {
        exif_df = merge(exif_df, img_exif, by = intersect(names(exif_df), names(img_exif)), all = TRUE)
      }
    }

    saveRDS(exif_df, glue::glue("{site_dir}/CSV/Corrected_values/df_{j}.rds"))
  })
}

read_exif_and_save(site_dir,parallel = TRUE, cores = 3)
