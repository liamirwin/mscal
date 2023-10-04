# Run MX Calibration

library(mscal)

site_dir <- 'G:/scantiques_roadshow/Block18_2023/Micasense/NorthEast'


# Setup Calibration Directories
setup_dirs(site_dir)

# User must now paste the panels into /DAP/MS/PANELS and the panel masks into /DAP/MS/MASKS

# Read Exif data from images and save as RDS

read_exif_and_save(site_dir, num_cores = 8L)

# Merge RDS into one dataframe

rds_df <- read_rds_and_combine(site_dir)

# Compute sun angle for dataframe and save

rds_df_sun <- compute_and_save_sun_angle(site_dir)

# Prepare Dataframe For Processing
xmp_all_ssa <- prepare_xmp_df(site_dir)

# Plot Sun angles - Should range around 30 in summer

plot_sun_sensor_angles(xmp_all_ssa)

ggplot2::ggsave(filename = glue::glue('{site_dir}/PLOTS/sun_angles.png'))

# Compute rolling regression
library(tidyfit)
library(tidyr)
library(dplyr)
mod_frame <- compute_rolling_regression(site_dir, xmp_all_ssa, r_squared_threshold = 0.4, lookback_seconds =  30)

# Plot rolling regression results; check if you're keeping enough models

plot_scattered_light(mod_frame)
ggsave(filename = glue::glue('{site_dir}/PLOTS/regression.png'))

# Plot Spatial Pattern

plot_spatial_pattern(mod_frame)
ggsave(filename = glue::glue('{site_dir}/PLOTS/spatial_pattern.png'))

# Calculate aggregate ratios and save RDS

ratios <- aggregate_ratios(mod_frame, site_dir)

# Print diffuse/direct ratio

print(glue::glue('Direct/Diffuse Ratio is: {round(ratios$dir_diff_ratio, 2)} for {basename(site_dir)}'))

# Compute corrected irradiance values

xmp_corrected <- compute_corrected_irradiance(xmp_all_ssa = mod_frame, ratios = ratios)

# Plot irradiance values

plot_irradiance(xmp_corrected)

# Plot geolocation

plot_geolocation(xmp_corrected)
