# =============================================================================
# Script 7: Relate NDVI/NDRE to Ground Truth
# -----------------------------------------------------------------------------
# Purpose : Point-by-point calibration of NDVI AND NDRE against field
#           measurements (Establishment, Biomass_flowering). For each field
#           sample point, extracts a buffered mean (1m radius) from the
#           nearest-date raster - not a zone-wide average - so the
#           comparison reflects the actual measurement location. Buffer
#           absorbs GPS positional error and local pixel noise.
#           Satellite NDRE uses the raw 10-band stack (raw_path, see Script
#           1/2/3); Planet NDRE uses its Red Edge band (see Script 2b/3b).
#           Drone excluded entirely: no red-edge band available (no NDRE
#           possible), AND nearest drone date is 38 days from Establishment
#           sampling / 10 days from Biomass sampling - too large a gap given
#           how fast NDVI moves through the season (see Script 2 QC).
#
# Inputs  : {site_name}_biomass_points_geo_script6.rds
#           {site_name}_establishment_points_geo_script6.rds
#           {site_name}_site_inventory_script1.rds  (raster paths by date;
#           satellite rows need raw_path, planet rows need mask_path)
#
# Outputs : {site_name}_establishment_groundtruth_pointlevel_script7.csv/.rds
#           {site_name}_biomass_groundtruth_pointlevel_script7.csv/.rds
#           {site_name}_groundtruth_correlation_summary_script7.csv/.rds
#           {site_name}_establishment_vs_ndvi_script7.png
#           {site_name}_biomass_vs_ndvi_script7.png
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below.
# =============================================================================

library(dplyr)
library(readr)
library(sf)
library(terra)
library(exactextractr)
library(ggplot2)

# ============================== SITE CONFIG =================================
site_name     <- "1.Walpeup_MRS125"
pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"
output_folder         <- file.path(pipeline_output_base, site_name)

buffer_m <- 1   # radius around each point - absorbs GPS error + pixel noise

establishment_date     <- as.Date("2025-05-19")
biomass_flowering_date <- as.Date("2025-09-22")
# =============================================================================

# ---- 1. Load point geometry (Script 6) and raster inventory (Script 1) ----
biomass_pts      <- readRDS(file.path(output_folder, paste0(site_name, "_biomass_points_geo_script6.rds")))
establishment_pts <- readRDS(file.path(output_folder, paste0(site_name, "_establishment_points_geo_script6.rds")))
site_inventory    <- readRDS(file.path(output_folder, paste0(site_name, "_site_inventory_script1.rds")))

# ---- 2. Find nearest satellite + planet date to each field sample date ----
nearest_date <- function(source_name, target_date, inventory) {
  inventory %>%
    filter(source == source_name) %>%
    distinct(date) %>%
    mutate(gap = abs(as.numeric(date - target_date))) %>%
    slice_min(gap, n = 1, with_ties = FALSE) %>%
    pull(date)
}

est_sat_date    <- nearest_date("satellite", establishment_date, site_inventory)
est_planet_date <- nearest_date("planet", establishment_date, site_inventory)
bio_sat_date    <- nearest_date("satellite", biomass_flowering_date, site_inventory)
bio_planet_date <- nearest_date("planet", biomass_flowering_date, site_inventory)

est_sat_date; est_planet_date; bio_sat_date; bio_planet_date

# ---- 3. Functions: build NDVI+NDRE raster stacks for satellite and Planet -
satellite_indices_raster <- function(file_path, raw_path) {
  ndvi_r <- rast(file_path)
  NAflag(ndvi_r) <- 0
  raw_r  <- rast(raw_path)
  ndre_r <- (raw_r[["nbart_nir_1"]] - raw_r[["nbart_red_edge_1"]]) /
    (raw_r[["nbart_nir_1"]] + raw_r[["nbart_red_edge_1"]])
  NAflag(ndre_r) <- 0
  stack <- c(ndvi_r, ndre_r)
  names(stack) <- c("NDVI", "NDRE")
  stack
}

planet_indices_raster <- function(file_path, mask_path) {
  r <- rast(file_path)
  m <- rast(mask_path)
  ndvi <- (r[["nir"]] - r[["red"]]) / (r[["nir"]] + r[["red"]])
  ndre <- (r[["nir"]] - r[["rededge"]]) / (r[["nir"]] + r[["rededge"]])
  stack <- c(ndvi, ndre)
  names(stack) <- c("NDVI", "NDRE")
  mask(stack, m[["clear"]], maskvalues = 0)
}

# ---- 4. Function: buffered mean NDVI+NDRE at each point, for one raster ---
# Returns a data frame with mean.NDVI and mean.NDRE columns (exact_extract's
# standard naming for 2-layer input - same convention as Scripts 3/3b).
extract_indices_at_points <- function(r, points_sf, buffer_m) {
  points_buffered <- points_sf %>% st_buffer(dist = buffer_m)
  exact_extract(r, points_buffered, fun = "mean")
}

# ---- 5. Get raster/mask paths for the matched dates ------------------------
get_path <- function(source_name, target_date, col = "file_path") {
  site_inventory %>% filter(source == source_name, date == target_date) %>% pull(.data[[col]])
}

est_sat_path     <- get_path("satellite", est_sat_date)
est_sat_raw      <- get_path("satellite", est_sat_date, "raw_path")
bio_sat_path     <- get_path("satellite", bio_sat_date)
bio_sat_raw      <- get_path("satellite", bio_sat_date, "raw_path")
est_planet_path  <- get_path("planet", est_planet_date)
bio_planet_path  <- get_path("planet", bio_planet_date)
est_planet_mask  <- get_path("planet", est_planet_date, "mask_path")
bio_planet_mask  <- get_path("planet", bio_planet_date, "mask_path")

# ---- 6. Extract buffered NDVI+NDRE at every Establishment and Biomass point
est_sat_idx    <- extract_indices_at_points(satellite_indices_raster(est_sat_path, est_sat_raw), establishment_pts$geometry, buffer_m)
est_planet_idx <- extract_indices_at_points(planet_indices_raster(est_planet_path, est_planet_mask), establishment_pts$geometry, buffer_m)

bio_sat_idx    <- extract_indices_at_points(satellite_indices_raster(bio_sat_path, bio_sat_raw), biomass_pts$geometry, buffer_m)
bio_planet_idx <- extract_indices_at_points(planet_indices_raster(bio_planet_path, bio_planet_mask), biomass_pts$geometry, buffer_m)

establishment_pts <- establishment_pts %>%
  mutate(
    ndvi_satellite = est_sat_idx$mean.NDVI,
    ndre_satellite = est_sat_idx$mean.NDRE,
    ndvi_planet    = est_planet_idx$mean.NDVI,
    ndre_planet    = est_planet_idx$mean.NDRE
  )

biomass_pts <- biomass_pts %>%
  mutate(
    ndvi_satellite = bio_sat_idx$mean.NDVI,
    ndre_satellite = bio_sat_idx$mean.NDRE,
    ndvi_planet    = bio_planet_idx$mean.NDVI,
    ndre_planet    = bio_planet_idx$mean.NDRE
  )

establishment_pts %>% st_drop_geometry() %>%
  select(pt_id, treat, establishment_plants_m2, ndvi_satellite, ndre_satellite, ndvi_planet, ndre_planet)

biomass_pts %>% st_drop_geometry() %>%
  select(pt_id, treat, Biomass_flowering, ndvi_satellite, ndre_satellite, ndvi_planet, ndre_planet)

# ---- 7. Correlation summary (NDVI and NDRE, both sources) -----------------
correlation_summary <- tibble(
  variable          = rep(c("Establishment", "Biomass_flowering"), each = 2),
  metric            = rep(c("NDVI", "NDRE"), times = 2),
  cor_satellite = c(
    cor(establishment_pts$establishment_plants_m2, establishment_pts$ndvi_satellite),
    cor(establishment_pts$establishment_plants_m2, establishment_pts$ndre_satellite),
    cor(biomass_pts$Biomass_flowering, biomass_pts$ndvi_satellite),
    cor(biomass_pts$Biomass_flowering, biomass_pts$ndre_satellite)
  ),
  cor_planet = c(
    cor(establishment_pts$establishment_plants_m2, establishment_pts$ndvi_planet),
    cor(establishment_pts$establishment_plants_m2, establishment_pts$ndre_planet),
    cor(biomass_pts$Biomass_flowering, biomass_pts$ndvi_planet),
    cor(biomass_pts$Biomass_flowering, biomass_pts$ndre_planet)
  )
)

correlation_summary

# ---- 8. Scatter plots (best-correlated source per variable, from Script 5
# results: satellite for Establishment, Planet for Biomass) ----------------
est_plot <- ggplot(establishment_pts, aes(x = ndvi_satellite, y = establishment_plants_m2)) +
  geom_point(aes(colour = treat), size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey30") +
  labs(title = "Establishment vs Satellite NDVI", x = "NDVI (satellite)", y = "Establishment (plants/m²)")

bio_plot <- ggplot(biomass_pts, aes(x = ndvi_planet, y = Biomass_flowering)) +
  geom_point(aes(colour = treat), size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey30") +
  labs(title = "Biomass vs Planet NDVI", x = "NDVI (Planet)", y = "Biomass_flowering (kg/ha)")

est_plot
bio_plot

ggsave(file.path(output_folder, paste0(site_name, "_establishment_vs_ndvi_script7.png")),
       est_plot, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_folder, paste0(site_name, "_biomass_vs_ndvi_script7.png")),
       bio_plot, width = 8, height = 6, dpi = 300)

# ---- 9. Save all outputs ----------------------------------------------------
write_csv(establishment_pts %>% st_drop_geometry(),
          file.path(output_folder, paste0(site_name, "_establishment_groundtruth_pointlevel_script7.csv")))
saveRDS(establishment_pts,
        file.path(output_folder, paste0(site_name, "_establishment_groundtruth_pointlevel_script7.rds")))

write_csv(biomass_pts %>% st_drop_geometry(),
          file.path(output_folder, paste0(site_name, "_biomass_groundtruth_pointlevel_script7.csv")))
saveRDS(biomass_pts,
        file.path(output_folder, paste0(site_name, "_biomass_groundtruth_pointlevel_script7.rds")))

write_csv(correlation_summary,
          file.path(output_folder, paste0(site_name, "_groundtruth_correlation_summary_script7.csv")))
saveRDS(correlation_summary,
        file.path(output_folder, paste0(site_name, "_groundtruth_correlation_summary_script7.rds")))

