# Run MX Calibration

library(mscal)

site_dir <- 'G:/Ontario_2023/MicaSense_Calibration/Block18_NW'


# Setup Calibration Directories
setup_dirs(site_dir)

# User must now paste the panels into /DAP/MS/PANELS and the panel masks into /DAP/MS/MASKS

# Read Exif data from images and save as RDS

read_exif_and_save(site_dir, parallel = TRUE, cores = 1)

# Merge RDS into one dataframe

rds_df <- read_rds_and_combine(site_dir)

# Compute sun angle for dataframe and save

rds_df_sun <- compute_and_save_sun_angle(site_dir)

# Compute scattered direct irradiance ratios

xmp_all_ssa <- compute_scatter_direct_ratio(site_dir)

# Plot Sun angles

plot_sun_sensor_angles(xmp_all_ssa)
ggsave(filename = glue::glue('{site_dir}/PLOTS/sun_angles.png'), plot = p1)

# Compute rolling regression

mod_frame <- compute_rolling_regression(site_dir, xmp_all_ssa)

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
