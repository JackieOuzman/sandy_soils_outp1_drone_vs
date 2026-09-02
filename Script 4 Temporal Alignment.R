# =============================================================================
# Script 4: Temporal Alignment
# -----------------------------------------------------------------------------
# Purpose : Two distinct outputs, not one merged table:
#           (a) matched_drone_satellite - for each drone flight, find the
#               single closest satellite date PER STRIP, so drone and
#               satellite can be compared fairly at that moment. Reports the
#               actual day-gap so a mismatch isn't misread as a real
#               difference in NDVI.
#           (b) satellite_trajectory - the full satellite time series,
#               unchanged, kept separate for season-long trend analysis
#               (Script 5). Not touched by the matching logic below.
#
# Inputs  : {site_name}_zonal_stats_script3.rds
#
# Outputs : {site_name}_matched_drone_satellite_script4.csv/.rds
#           {site_name}_satellite_trajectory_script4.csv/.rds
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below.
# =============================================================================

library(dplyr)
library(readr)

# ============================== SITE CONFIG =================================
site_name     <- "1.Walpeup_MRS125"
pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"
output_folder         <- file.path(pipeline_output_base, site_name)

flag_gap_days <- 10   # day-gap above this gets flagged for caution, not dropped
# =============================================================================

# ---- 1. Load Script 3's zonal stats ----------------------------------------
zonal_stats <- readRDS(file.path(output_folder, paste0(site_name, "_zonal_stats_script3.rds")))

# ---- 2. (a) Match each drone date to its nearest satellite date, per strip -
drone_rows     <- zonal_stats %>% filter(source == "drone")
satellite_rows <- zonal_stats %>% filter(source == "satellite")

matched_drone_satellite <- drone_rows %>%
  select(plot, treat, treatment_name, plot_order,
         date_drone = date, mean_drone = mean, stdev_drone = stdev, count_drone = count) %>%
  left_join(
    satellite_rows %>% select(plot, date_satellite = date,
                              mean_satellite = mean, stdev_satellite = stdev, count_satellite = count),
    by = "plot"
  ) %>%
  mutate(day_gap = abs(as.numeric(date_drone - date_satellite))) %>%
  group_by(plot, date_drone) %>%
  slice_min(day_gap, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(flag_large_gap = day_gap > flag_gap_days) %>%
  select(plot, treat, treatment_name, plot_order,
         date_drone, date_satellite, day_gap, flag_large_gap,
         mean_drone, mean_satellite, stdev_drone, stdev_satellite,
         count_drone, count_satellite)

matched_drone_satellite

# ---- 3. (b) Full satellite trajectory, unchanged, kept separate -----------
satellite_trajectory <- satellite_rows

# ---- 4. Save both outputs ---------------------------------------------------
write_csv(matched_drone_satellite,
          file.path(output_folder, paste0(site_name, "_matched_drone_satellite_script4.csv")))
saveRDS(matched_drone_satellite,
        file.path(output_folder, paste0(site_name, "_matched_drone_satellite_script4.rds")))

write_csv(satellite_trajectory,
          file.path(output_folder, paste0(site_name, "_satellite_trajectory_script4.csv")))
saveRDS(satellite_trajectory,
        file.path(output_folder, paste0(site_name, "_satellite_trajectory_script4.rds")))


matched_drone_satellite %>% print(n = Inf)

# Quick summary: how far apart are the matched dates?
matched_drone_satellite %>% distinct(date_drone, date_satellite, day_gap, flag_large_gap)
