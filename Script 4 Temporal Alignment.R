# =============================================================================
# Script 4: Temporal Alignment
# -----------------------------------------------------------------------------
# Purpose : For each of the three extraction levels (treatment, zone, strip x
#           zone), produce matched comparisons across all three sources:
#             - drone <-> satellite (nearest satellite date to each drone flight)
#             - drone <-> planet    (nearest Planet date to each drone flight)
#             - planet <-> satellite (nearest satellite date to each Planet
#               date - tests whether Planet's ~3m resolution shows anything
#               Sentinel's 10m doesn't, independent of drone)
#           Plus full trajectories (satellite, planet) kept separate and
#           unmatched, for season-long trend analysis in later scripts.
#           Each matched table reports day_gap and flag_large_gap so a date
#           mismatch is never misread as a real NDVI difference.
#
# Inputs  : {site_name}_zonal_stats_script3.rds                    (L1: sat+drone)
#           {site_name}_zone_zonal_stats_script3.rds                (L2: sat+drone)
#           {site_name}_strip_zone_zonal_stats_script3.rds          (L3: sat+drone)
#           {site_name}_planet_zonal_stats_script3b.rds             (L1: planet)
#           {site_name}_planet_zone_zonal_stats_script3b.rds        (L2: planet)
#           {site_name}_planet_strip_zone_zonal_stats_script3b.rds  (L3: planet)
#
# Outputs : per level (treatment / zone / stripzone), per pairing:
#           {site_name}_{level}_matched_drone_satellite_script4.csv/.rds
#           {site_name}_{level}_matched_drone_planet_script4.csv/.rds
#           {site_name}_{level}_matched_planet_satellite_script4.csv/.rds
#           plus trajectories:
#           {site_name}_{level}_satellite_trajectory_script4.csv/.rds
#           {site_name}_{level}_planet_trajectory_script4.csv/.rds
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below.
# =============================================================================

library(dplyr)
library(readr)

# ============================== SITE CONFIG =================================
site_name     <- "1.Walpeup_MRS125"
pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"
output_folder         <- file.path(pipeline_output_base, site_name)

flag_gap_days <- 10
# =============================================================================

# ---- 1. Generic matching function - works for any two sources/levels ------
# join_keys: columns that uniquely identify a comparison unit at this level
#   Level 1 (treatment):    "plot"
#   Level 2 (zone):         "zone_code"
#   Level 3 (strip x zone): c("plot", "zone_code")

match_nearest <- function(df_a, df_b, join_keys, label_a, label_b) {
  df_a2 <- df_a %>% rename(date_a = date, mean_a = mean, stdev_a = stdev, count_a = count)
  df_b2 <- df_b %>% rename(date_b = date, mean_b = mean, stdev_b = stdev, count_b = count) %>%
    select(all_of(join_keys), date_b, mean_b, stdev_b, count_b)
  
  matched <- df_a2 %>%
    left_join(df_b2, by = join_keys) %>%
    mutate(day_gap = abs(as.numeric(date_a - date_b))) %>%
    group_by(across(all_of(c(join_keys, "date_a")))) %>%
    slice_min(day_gap, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(flag_large_gap = day_gap > flag_gap_days)
  
  names(matched)[names(matched) == "date_a"]  <- paste0("date_", label_a)
  names(matched)[names(matched) == "date_b"]  <- paste0("date_", label_b)
  names(matched)[names(matched) == "mean_a"]  <- paste0("mean_", label_a)
  names(matched)[names(matched) == "mean_b"]  <- paste0("mean_", label_b)
  names(matched)[names(matched) == "stdev_a"] <- paste0("stdev_", label_a)
  names(matched)[names(matched) == "stdev_b"] <- paste0("stdev_", label_b)
  names(matched)[names(matched) == "count_a"] <- paste0("count_", label_a)
  names(matched)[names(matched) == "count_b"] <- paste0("count_", label_b)
  
  matched
}

# ---- 2. Function that runs all matchings + saves, for ONE level -----------
process_level <- function(zonal_sat_drone, zonal_planet, join_keys, id_cols, level_name) {
  
  drone_rows     <- zonal_sat_drone %>% filter(source == "drone")
  satellite_rows <- zonal_sat_drone %>% filter(source == "satellite")
  planet_rows    <- zonal_planet
  
  # drone <-> satellite
  m1 <- match_nearest(drone_rows, satellite_rows, join_keys, "drone", "satellite") %>%
    select(all_of(id_cols), date_drone, date_satellite, day_gap, flag_large_gap,
           mean_drone, mean_satellite, stdev_drone, stdev_satellite, count_drone, count_satellite)
  
  # drone <-> planet
  m2 <- match_nearest(drone_rows, planet_rows, join_keys, "drone", "planet") %>%
    select(all_of(id_cols), date_drone, date_planet, day_gap, flag_large_gap,
           mean_drone, mean_planet, stdev_drone, stdev_planet, count_drone, count_planet)
  
  # planet <-> satellite (independent of drone)
  m3 <- match_nearest(planet_rows, satellite_rows, join_keys, "planet", "satellite") %>%
    select(all_of(id_cols), date_planet, date_satellite, day_gap, flag_large_gap,
           mean_planet, mean_satellite, stdev_planet, stdev_satellite, count_planet, count_satellite)
  
  write_csv(m1, file.path(output_folder, paste0(site_name, "_", level_name, "_matched_drone_satellite_script4.csv")))
  saveRDS(m1,  file.path(output_folder, paste0(site_name, "_", level_name, "_matched_drone_satellite_script4.rds")))
  
  write_csv(m2, file.path(output_folder, paste0(site_name, "_", level_name, "_matched_drone_planet_script4.csv")))
  saveRDS(m2,  file.path(output_folder, paste0(site_name, "_", level_name, "_matched_drone_planet_script4.rds")))
  
  write_csv(m3, file.path(output_folder, paste0(site_name, "_", level_name, "_matched_planet_satellite_script4.csv")))
  saveRDS(m3,  file.path(output_folder, paste0(site_name, "_", level_name, "_matched_planet_satellite_script4.rds")))
  
  write_csv(satellite_rows, file.path(output_folder, paste0(site_name, "_", level_name, "_satellite_trajectory_script4.csv")))
  saveRDS(satellite_rows,  file.path(output_folder, paste0(site_name, "_", level_name, "_satellite_trajectory_script4.rds")))
  
  write_csv(planet_rows, file.path(output_folder, paste0(site_name, "_", level_name, "_planet_trajectory_script4.csv")))
  saveRDS(planet_rows,  file.path(output_folder, paste0(site_name, "_", level_name, "_planet_trajectory_script4.rds")))
  
  list(drone_satellite = m1, drone_planet = m2, planet_satellite = m3)
}

# ---- 3. Level 1: treatment ---------------------------------------------------
zonal_stats    <- readRDS(file.path(output_folder, paste0(site_name, "_zonal_stats_script3.rds")))
planet_zonal   <- readRDS(file.path(output_folder, paste0(site_name, "_planet_zonal_stats_script3b.rds")))

level1_results <- process_level(zonal_stats, planet_zonal,
                                join_keys = "plot",
                                id_cols   = c("plot", "treat", "treatment_name", "plot_order"),
                                level_name = "treatment")

level1_results$drone_planet

# ---- 4. Level 2: zone ---------------------------------------------------------
zone_zonal_stats  <- readRDS(file.path(output_folder, paste0(site_name, "_zone_zonal_stats_script3.rds")))
planet_zone_zonal <- readRDS(file.path(output_folder, paste0(site_name, "_planet_zone_zonal_stats_script3b.rds")))

level2_results <- process_level(zone_zonal_stats, planet_zone_zonal,
                                join_keys = "zone_code",
                                id_cols   = c("zone_code", "zone_label"),
                                level_name = "zone")

level2_results$drone_planet

# ---- 5. Level 3: strip x zone --------------------------------------------------
strip_zone_zonal_stats  <- readRDS(file.path(output_folder, paste0(site_name, "_strip_zone_zonal_stats_script3.rds")))
planet_strip_zone_zonal <- readRDS(file.path(output_folder, paste0(site_name, "_planet_strip_zone_zonal_stats_script3b.rds")))

level3_results <- process_level(strip_zone_zonal_stats, planet_strip_zone_zonal,
                                join_keys = c("plot", "zone_code"),
                                id_cols   = c("plot", "treat", "treatment_name", "plot_order", "zone_code", "zone_label"),
                                level_name = "stripzone")

level3_results$drone_planet

