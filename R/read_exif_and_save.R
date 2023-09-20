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
#' read_exif_and_save(site_dir = "your_site_directory", num_cores = 6L)
#' }
read_exif_and_save <- function(site_dir, num_cores = 1L) {
  # Start the timer
  tictoc::tic()

  # Set up the future plan for parallelization
  future::plan(future::multisession, workers = num_cores)

  # Define Function
  process_band <-  function(site_dir, j) {

    # List all image files
    pics = list.files(
      glue::glue("{site_dir}/DAP/MS/"),
      pattern = c(
        glue::glue("IMG_...._{j}.tif"),
        glue::glue("IMG_...._{j}_.tif")
      ),
      recursive = TRUE,
      full.names = TRUE
    )

    # Output the number of pictures found
    print(glue::glue("Found {length(pics)} pictures for band {j}"))

    # Function to process a single picture
    process_pic <- function(pic, p) {
      # Extract the root name of the picture
      pic_root <- gsub(".*/(IMG_.*)(\\.tif)$", "\\1", pic)
      # Read EXIF data from the picture
      img_exif <- read_exif(pic)
      # Update the progress
      p()
      # Convert all columns to character
      img_exif[] <- lapply(img_exif, as.character)
      # Return as a data frame
      as.data.frame(img_exif)
    }

    with_progress({
      # Initialize the progress bar
      p <- progressor(steps = length(pics))

      # Loop over each picture to read EXIF data using furrr
      exif_df <- furrr::future_map_dfr(pics, ~process_pic(.x, p))

      # Check and create necessary directories
      if (!dir.exists(glue::glue("{site_dir}/CSV"))) {
        dir.create(glue::glue("{site_dir}/CSV"))
      }
      if (!dir.exists(glue::glue("{site_dir}/CSV/Corrected_values"))) {
        dir.create(glue::glue("{site_dir}/CSV/Corrected_values"))
      }

      # Save the EXIF dataframe for each band as an RDS file
      saveRDS(exif_df, file = glue::glue('{site_dir}/CSV/Corrected_values/df_{j}.rds'))

      return(NULL)
    })

  }

  # Parallel loop over each band
  for(j in 1:10){
    tictoc::tic()
    process_band(site_dir, j)
    print('Read and saved exif data for band {j} for {basename(site_dir)}')
    tictoc::toc()
  }

  # Stop the future plan
  future::plan(future::sequential)

  # Stop the timer
  tictoc::toc()

  print(glue('Read and locally saved EXIF data for all ten bands'))

  return(NULL)


}

