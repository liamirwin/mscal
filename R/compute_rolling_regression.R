#' @title Compute Rolling Regression and Subsequent Filtering
#' @description This function computes a rolling regression for the given data.
#' It fits a linear model to estimate the relationship between Irradiance and cos_SSA.
#' It then applies additional transformations and filtering to the data.
#' @param site_dir The directory path where the RDS file should be saved.
#' @param xmp_all_ssa The data frame containing the relevant variables.
#' @param r_squared_threshold The R-squared threshold to use (default is 0.4).
#' @param lookback_seconds The lookback window in seconds for the rolling regression.
#' @return A data frame with refined results of the rolling regression.
#' @export
#' @examples
#' \dontrun{
#'   result_df <- compute_rolling_regression(site_dir = "your_directory_path_here", xmp_all_ssa = your_data_frame_here)
#' }
compute_rolling_regression <- function(site_dir,
                                       xmp_all_ssa,
                                       r_squared_threshold = 0.4,
                                       lookback_seconds = 30) {

  # Start the timer for performance monitoring
  tictoc::tic()

  # Remove NA values from the relevant columns
  mod_frame <- xmp_all_ssa %>%
    drop_na(Date, Date2, cos_SSA, Irradiance) %>% ungroup() %>%
    group_by(BandName)

  print(glue::glue('Fitting rolling regression to {nrow(mod_frame)} observations
                   This could take a while...'))

  mod_frame <-  mod_frame %>%
  # fit a rolling regression; for each image, fit a linear model of all images (of the same band)
  # within 30 seconds of the image
  tidyfit::regress(Irradiance ~ cos_SSA, m("lm"),
                     .cv = "sliding_index", .cv_args = list(lookback = lubridate::seconds(30), index = "Date2"),
                     .force_cv = TRUE, .return_slices = TRUE)

  # Extract useful statistics from each fitted model
  df <- mod_frame %>%
    mutate(R2 = map(model_object, ~summary(.x)$adj.r.squared)) %>%
    coef() %>%
    unnest(model_info) %>%
    mutate(Date2 = ymd_hms(slice_id))

  # Pivot longer format to wider format for easier analysis
  df_params = df %>%
    dplyr::select(BandName:estimate, Date2) %>%
    # we will have to go from long, with 2 observations per model, to wide
    pivot_wider(names_from = term, values_from = estimate, values_fn = {first}) %>%
    dplyr::rename("Intercept" = `(Intercept)`,
                  "Slope" = "cos_SSA")

  # Extract p-value and R-squared for cos_SSA
  df_p <- df %>%
    filter(term == "cos_SSA")

  # Merge the additional attributes from the original data frame
  df_filtered <- df_params %>%
    left_join(df_p) %>%
    left_join(xmp_all_ssa) %>%
    mutate(percent_scattered = Intercept / (Slope + Intercept),
           dir_diff = Intercept/Slope)

  # Apply filters and calculate summary statistics
  df_to_use <- df_filtered %>%
    mutate(R2 = as.numeric(R2)) %>%
    filter(R2 > r_squared_threshold & Slope > 0 & Intercept > 0) %>%
    group_by(Date) %>%
    mutate(mean_scattered = mean(percent_scattered),
           dir_diff_ratio = mean(dir_diff))

  # Save the filtered data frame as an RDS file
  readr::write_rds(df_to_use, glue::glue("{site_dir}\\CSV\\Corrected_values\\rolling_regression_used.rds"))

  # Print a message to indicate the operation's status
  print(glue::glue('Calculated Rolling Regression values for {site_dir}'))

  # Stop the timer and print elapsed time
  tictoc::toc()

  return(df_to_use)
}
