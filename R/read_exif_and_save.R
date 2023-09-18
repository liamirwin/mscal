#' Read EXIF data and save it
#'
#' @name read_exif_and_save
#' @param site_dir The directory where the site's data is located.
#'
#' @return Nothing, but saves EXIF data to RDS files.
#' @export
#'
#' @examples
#' \dontrun{
#' read_exif_and_save(site_dir = "your_site_directory")
#' }
read_exif_and_save <- function(site_dir) {

    # Initialize an empty dataframe
    df_all = data.frame()

    for (j in 1:10){ # bands 1 through 10

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

        pic = pics[i]
        pic_root = tail(unlist(strsplit(pics_root[i], "Panels/")), n = 1)
        pan_flag = length(unlist(strsplit(pics_root[i], "Panels/"))) - 1

        if (substr(pic_root, 11, 11) == "0") {
          band = substr(pic_root, 10, 11)
        } else {
          band = substr(pic_root, 10, 10)
        }

        img_exif = exifr::read_exif(pic)
        print(glue::glue("band {j}: Image {i}/{length(pics)}"))

        if (i == 1) {
          exif_df = as.data.frame(img_exif)
          exif_df = dplyr::mutate(exif_df, panel_flag = pan_flag)
        } else {
          exif_df = merge(exif_df, img_exif, by = intersect(names(exif_df), names(img_exif)), all = TRUE)
        }
      }

      saveRDS(exif_df, glue::glue("{site_dir}/CSV/Corrected_values/df_{j}.rds"))
    }

    print(glue::glue('Read and locally saved EXIF data for all ten bands'))
}

