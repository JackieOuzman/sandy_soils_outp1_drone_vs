# =============================================================================
# Script 3b: Planet Zonal Extraction
# -----------------------------------------------------------------------------
# Purpose : Planet-specific equivalent of Script 3. Calculates NDVI from
#           Planet's Red/NIR bands and applies the udm2 clear-mask (see
#           Script 2b), then runs the SAME three-level zonal extraction as
#           Script 3 (treatment, zone, strip x zone) - reusing identical
#           logic, just with a different raster-preparation step since
#           Planet doesn't arrive as ready-made NDVI like drone/satellite.
#           Rebuilds strips_clean/zones_labelled/strip_zone fresh (these
#           were not saved as objects by Script 3 - only their resulting
#           stats tables were).
#
# Inputs  : {site_name}_site_inventory_script1.rds
#           {site_name}_zones_labelled_script1.rds
#           trial.plan shapefile + treatment names metadata
#
# Outputs : {site_name}_planet_zonal_stats_script3b.csv/.rds            (Level 1)
#           {site_name}_planet_zone_zonal_stats_script3b.csv/.rds       (Level 2)
#           {site_name}_planet_strip_zone_zonal_stats_script3b.csv/.rds (Level 3)
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below.
# =============================================================================

library(dplyr)
library(readr)
library(readxl)
library(sf)
library(terra)
library(exactextractr)

# ============================== SITE CONFIG =================================
site_name     <- "1.Walpeup_MRS125"
base_path     <- "H:/Output-1"
metadata_path <- file.path(base_path, "0.Site-info",
                           "names of treatments per site 2025 metadata and other info.xlsx")

pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"
output_folder         <- file.path(pipeline_output_base, site_name)
# =============================================================================

# ---- 1. Load Script 1 outputs -----------------------------------------------
site_inventory <- readRDS(file.path(output_folder, paste0(site_name, "_site_inventory_script1.rds")))
zones_labelled <- readRDS(file.path(output_folder, paste0(site_name, "_zones_labelled_script1.rds")))
zones_labelled <- st_make_valid(zones_labelled)   # same fix as Script 3, Section 9

planet_rows <- site_inventory %>% filter(source == "planet")

# ---- 2. Rebuild strips_clean (same as Script 3, Sections 2-3) --------------
site_files <- read_excel(metadata_path, sheet = "file location etc") %>%
  filter(Site == site_name)

strips_path <- site_files %>%
  filter(variable == "trial.plan") %>%
  pull(`file path`)

treatment_names <- read_excel(metadata_path, sheet = "treatment names") %>%
  filter(Site == site_name)

strips <- st_read(file.path(base_path, site_name, strips_path))

strips_clean <- strips %>%
  filter(treat %in% treatment_names$treat) %>%
  left_join(treatment_names %>% select(treat, treatment_name = `Shorthand Name`,
                                       plot_order = `Order in Paddock`),
            by = "treat")

# ---- 3. Rebuild strip_zone intersection (same as Script 3, Section 11) ----
strip_zone <- st_intersection(strips_clean, zones_labelled) %>%
  mutate(area_m2 = as.numeric(st_area(.))) %>%
  filter(area_m2 > 5)

# ---- 4. Function: build a masked NDVI raster from Planet bands + udm2 -----
planet_ndvi_raster <- function(file_path, mask_path) {
  r <- rast(file_path)
  m <- rast(mask_path)
  
  ndvi <- (r[["nir"]] - r[["red"]]) / (r[["nir"]] + r[["red"]])
  ndvi <- mask(ndvi, m[["clear"]], maskvalues = 0)
  names(ndvi) <- "NDVI"
  ndvi
}

# ---- 5. Level 1: NDVI per strip (treatment only) ----------------------------
# Same extraction logic as Script 3's extract_zonal(), adapted to build the
# NDVI raster from bands+mask first instead of reading a ready-made file.

extract_planet_zonal <- function(file_path, mask_path, date, strips) {
  ndvi <- planet_ndvi_raster(file_path, mask_path)
  
  exact_extract(ndvi, strips, fun = c("mean", "stdev", "count")) %>%
    bind_cols(strips %>% st_drop_geometry() %>%
                select(plot, treat, treatment_name, plot_order)) %>%
    mutate(date = date, source = "planet")
}

planet_zonal <- planet_rows %>%
  rowwise() %>%
  reframe(extract_planet_zonal(file_path, mask_path, date, strips_clean)) %>%
  ungroup()

nrow(planet_zonal)   # should be 58 dates x 8 strips = 464

write_csv(planet_zonal, file.path(output_folder, paste0(site_name, "_planet_zonal_stats_script3b.csv")))
saveRDS(planet_zonal,  file.path(output_folder, paste0(site_name, "_planet_zonal_stats_script3b.rds")))

# ---- 6. Level 2: NDVI per zone (ignoring treatment) --------------------------

extract_planet_zonal_zone <- function(file_path, mask_path, date, zones) {
  ndvi <- planet_ndvi_raster(file_path, mask_path)
  
  exact_extract(ndvi, zones, fun = c("mean", "stdev", "count")) %>%
    bind_cols(zones %>% st_drop_geometry() %>% select(zone_code, zone_label)) %>%
    mutate(date = date, source = "planet")
}

planet_zone_zonal <- planet_rows %>%
  rowwise() %>%
  reframe(extract_planet_zonal_zone(file_path, mask_path, date, zones_labelled)) %>%
  ungroup()

nrow(planet_zone_zonal)   # should be 58 dates x 3 zones = 174

write_csv(planet_zone_zonal, file.path(output_folder, paste0(site_name, "_planet_zone_zonal_stats_script3b.csv")))
saveRDS(planet_zone_zonal,  file.path(output_folder, paste0(site_name, "_planet_zone_zonal_stats_script3b.rds")))

# ---- 7. Level 3: NDVI per strip x zone intersection --------------------------

extract_planet_zonal_stripzone <- function(file_path, mask_path, date, strip_zone) {
  ndvi <- planet_ndvi_raster(file_path, mask_path)
  
  exact_extract(ndvi, strip_zone, fun = c("mean", "stdev", "count")) %>%
    bind_cols(strip_zone %>% st_drop_geometry() %>%
                select(plot, treat, treatment_name, plot_order, zone_code, zone_label)) %>%
    mutate(date = date, source = "planet")
}

planet_strip_zone_zonal <- planet_rows %>%
  rowwise() %>%
  reframe(extract_planet_zonal_stripzone(file_path, mask_path, date, strip_zone)) %>%
  ungroup()

nrow(planet_strip_zone_zonal)   # should be 58 dates x 24 strip-zone pieces = 1392

write_csv(planet_strip_zone_zonal,
          file.path(output_folder, paste0(site_name, "_planet_strip_zone_zonal_stats_script3b.csv")))
saveRDS(planet_strip_zone_zonal,
        file.path(output_folder, paste0(site_name, "_planet_strip_zone_zonal_stats_script3b.rds")))



nrow(planet_zonal)              # expect 464
nrow(planet_zone_zonal)         # expect 174
nrow(planet_strip_zone_zonal)   # expect 1392

planet_zonal %>% filter(date == as.Date("2025-06-25")) %>% arrange(plot_order)
