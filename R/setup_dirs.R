#' Setup Required Directories
#'
#' This function creates the required directories if they do not already exist.
#'
#' @param site_dir The root directory under which to create the new directories.
#' @return Nothing, but creates directories.
#' @examples
#' setup_dirs('D:/Agisoft/ON/Block18_N_MX')
#' @export
setup_dirs <- function(site_dir) {
  # Define the sub-directory structure to be created under the site directory.
  targ_dirs <- c(
    "DAP",
    "DAP/RGB",
    "DAP/MS",
    "DAP/MS/MX",
    "DAP/MS/PANELS",
    "DAP/MS/MASKS",
    "CSV",
    "CSV/Corrected_values",
    "PLOTS"
  )
  # Use glue to dynamically build the full paths for these directories.
  targ_dirs <- unlist(lapply(targ_dirs, function(x) glue::glue("{site_dir}/{x}")))
  # Create these directories, suppressing any warnings.
  lapply(targ_dirs, FUN = dir.create, showWarnings = FALSE)
}

