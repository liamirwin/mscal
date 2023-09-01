#' Read EXIF data and save it
#'
#' @name read_exif_and_save
#' @param site_dir The directory where the site's data is located.
#' @param parallel Logical flag to run the function in parallel.
#' @param cores Number of cores to use for parallel processing.
#'
#' @return Nothing, but saves EXIF data to RDS files.
#' @export
#'
#' @examples
#' \dontrun{
#' read_exif_and_save(site_dir = "your_site_directory", parallel = TRUE, cores = 2)
#' }
read_exif_and_save <- function(site_dir, parallel = FALSE, cores = 2) {
  tryCatch({
    # Initialize an empty dataframe
    df_all = data.frame()

    if(parallel) {
      # Setting up parallel processing
      future::plan(future::multisession, workers = cores)
      print(glue::glue('Initialized parallel processing for {site_dir} with {cores} CPU cores'))
    }

    if(parallel) {
      progressr::handlers(global = TRUE)
      p <- progressr::progressor(steps = 10)  # Adjust the number of steps based on your actual number of iterations
    }

    # Choose the correct map function
    map_func <- ifelse(parallel, furrr::future_map, purrr::map)

    # Run
    map_func(1:10, ~{
      if(parallel) {
        p()  # Update the progress bar
      }
      print("Started processing...")  # Debug line

      j <- .x

      pics = list.files(glue::glue("{site_dir}/DAP/MS/"),
                        pattern = c(glue::glue("IMG_...._{j}.tif"), glue::glue("IMG_...._{j}_.tif")),
                        recursive = TRUE,
                        full.names = TRUE)

      print(paste0("Found ", length(pics), " pictures."))  # Debug line

      pics_root = list.files(glue::glue("{site_dir}/DAP/MS/"),
                             pattern = c(glue::glue("IMG_...._{j}.tif"), glue::glue("IMG_...._{j}_.tif")),
                             recursive = TRUE,
                             full.names = FALSE)

      exif_df = data.frame()

      for (i in seq_along(pics)) {
        print(paste0("Processing picture ", i, " out of ", length(pics)))  # Debug line

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
    })  # End of map_func
  }, error = function(e) {
    print(paste0("Caught an error: ", e$message))
  })  # End of tryCatch
}

