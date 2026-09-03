# =============================================================================
# Script 7: Relate NDVI to Ground Truth
# -----------------------------------------------------------------------------
# Purpose : Point-by-point calibration of NDVI against field measurements
#           (Establishment, Biomass_flowering). For each field sample point,
#           extracts a buffered mean NDVI (1m radius) from the nearest-date
#           raster - not a zone-wide average - so the comparison reflects
#           the actual measurement location. Buffer absorbs GPS positional
#           error and local pixel noise (especially relevant for drone's
#           cm-scale pixels).
#           Drone excluded: nearest drone date is 38 days from Establishment
#           sampling and 10 days from Biomass sampling - too large a gap
#           given how fast NDVI moves through the season (see Script 2 QC).
#
# Inputs  : {site_name}_biomass_points_geo_script6.rds
#           {site_name}_establishment_points_geo_script6.rds
#           {site_name}_site_inventory_script1.rds  (raster paths by date)
#
# Outputs : {site_name}_ndvi_groundtruth_pointlevel_script7.csv/.rds
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

# ---- 3. Function: build Planet NDVI raster from bands + udm2 mask ---------
planet_ndvi_raster <- function(file_path, mask_path) {
  r <- rast(file_path)
  m <- rast(mask_path)
  ndvi <- (r[["nir"]] - r[["red"]]) / (r[["nir"]] + r[["red"]])
  ndvi <- mask(ndvi, m[["clear"]], maskvalues = 0)
  names(ndvi) <- "NDVI"
  ndvi
}

# ---- 4. Function: buffered mean NDVI at each point, for one raster --------
extract_ndvi_at_points <- function(r, points_sf, buffer_m) {
  points_buffered <- points_sf %>% st_buffer(dist = buffer_m)
  exact_extract(r, points_buffered, fun = "mean")
}

# ---- 5. Get raster/mask paths for the matched dates ------------------------
get_path <- function(source_name, target_date, col = "file_path") {
  site_inventory %>% filter(source == source_name, date == target_date) %>% pull(.data[[col]])
}

est_sat_path     <- get_path("satellite", est_sat_date)
bio_sat_path     <- get_path("satellite", bio_sat_date)
est_planet_path  <- get_path("planet", est_planet_date)
bio_planet_path  <- get_path("planet", bio_planet_date)
est_planet_mask  <- get_path("planet", est_planet_date, "mask_path")
bio_planet_mask  <- get_path("planet", bio_planet_date, "mask_path")

# ---- 6. Extract buffered NDVI at every Establishment and Biomass point ----
establishment_pts <- establishment_pts %>%
  mutate(
    ndvi_satellite = extract_ndvi_at_points(rast(est_sat_path) %>% {NAflag(.) <- 0; .}, geometry, buffer_m),
    ndvi_planet    = extract_ndvi_at_points(planet_ndvi_raster(est_planet_path, est_planet_mask), geometry, buffer_m)
  )

biomass_pts <- biomass_pts %>%
  mutate(
    ndvi_satellite = extract_ndvi_at_points(rast(bio_sat_path) %>% {NAflag(.) <- 0; .}, geometry, buffer_m),
    ndvi_planet    = extract_ndvi_at_points(planet_ndvi_raster(bio_planet_path, bio_planet_mask), geometry, buffer_m)
  )

establishment_pts %>% st_drop_geometry() %>%
  select(pt_id, treat, establishment_plants_m2, ndvi_satellite, ndvi_planet)

biomass_pts %>% st_drop_geometry() %>%
  select(pt_id, treat, Biomass_flowering, ndvi_satellite, ndvi_planet)


# ---- 7. Correlation summary --------------------------------------------
tibble(
  variable = c("Establishment", "Biomass_flowering"),
  cor_satellite = c(cor(establishment_pts$establishment_plants_m2, establishment_pts$ndvi_satellite),
                    cor(biomass_pts$Biomass_flowering, biomass_pts$ndvi_satellite)),
  cor_planet = c(cor(establishment_pts$establishment_plants_m2, establishment_pts$ndvi_planet),
                 cor(biomass_pts$Biomass_flowering, biomass_pts$ndvi_planet))
)

# ---- 8. Scatter plots -----------------------------------------------------
ggplot(establishment_pts, aes(x = ndvi_satellite, y = establishment_plants_m2)) +
  geom_point(aes(colour = treat), size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey30") +
  labs(title = "Establishment vs Satellite NDVI", x = "NDVI (satellite)", y = "Establishment (plants/m²)")

ggplot(biomass_pts, aes(x = ndvi_planet, y = Biomass_flowering)) +
  geom_point(aes(colour = treat), size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey30") +
  labs(title = "Biomass vs Planet NDVI", x = "NDVI (Planet)", y = "Biomass_flowering (kg/ha)")

