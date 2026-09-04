# =============================================================================
# Script 3: Zonal Extraction
# -----------------------------------------------------------------------------
# Purpose : Extract NDVI and NDRE zonal statistics (mean, sd, n_pixels) per
#           date, at three levels:
#             Level 1 - per strip (treatment only)
#             Level 2 - per zone (soil type only)
#             Level 3 - per strip x zone intersection (treatment within zone)
#           Satellite: both NDVI (pre-made file) and NDRE (calculated from
#           the raw 10-band stack's nbart_nir_1/nbart_red_edge_1 - see
#           Script 1's raw_path, Script 2). Drone: NDVI only - no red-edge
#           band available in the single-band drone product, so NDRE
#           columns are NA for all drone rows.
#           exact_extract() names columns differently depending on whether
#           1 layer (drone: plain mean/stdev/count) or 2 layers (satellite:
#           mean.NDVI/mean.NDRE etc.) go in - standardise_index_cols()
#           aligns these so satellite and drone rows combine cleanly.
#           Drone is kept at native resolution (not aggregated, unlike
#           Script 2's paddock-level QC - see DECISIONS_LOG) since preserving
#           fine within-strip detail is the whole point of testing drone's
#           added value at sub-paddock scale.
#
# Inputs  : {site_name}_site_inventory_script1.rds  (satellite rows need
#           both file_path [NDVI] and raw_path [10-band stack, for NDRE])
#           {site_name}_raster_qc_script2.rds
#           {site_name}_zones_labelled_script1.rds
#           trial.plan shapefile (strip polygons) - path from metadata
#           treatment names metadata (per-site treatment codes/labels)
#
# Outputs : {site_name}_zonal_stats_script3.csv/.rds            (Level 1, NDVI+NDRE)
#           {site_name}_zone_zonal_stats_script3.csv/.rds       (Level 2, NDVI+NDRE)
#           {site_name}_strip_zone_zonal_stats_script3.csv/.rds (Level 3, NDVI+NDRE)
#           Columns: mean.NDVI, mean.NDRE, stdev.NDVI, stdev.NDRE, count.NDVI,
#           count.NDRE (NDRE columns are NA for drone rows) plus ID columns
#           for that level.
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below.
# =============================================================================

# install.packages("exactextractr")  # run once if not already installed

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

# ---- 1. Load Script 1/2 outputs --------------------------------------------
site_inventory <- readRDS(file.path(output_folder, paste0(site_name, "_site_inventory_script1.rds")))
raster_qc      <- readRDS(file.path(output_folder, paste0(site_name, "_raster_qc_script2.rds")))

# ---- 2. Read metadata: strip shapefile path + treatment names -------------
site_files <- read_excel(metadata_path, sheet = "file location etc") %>%
  filter(Site == site_name)

strips_path <- site_files %>%
  filter(variable == "trial.plan") %>%
  pull(`file path`)

treatment_names <- read_excel(metadata_path, sheet = "treatment names") %>%
  filter(Site == site_name)

# ---- 3. Read strip polygons, drop untracked codes (e.g. buffer strips) ----
# Any strip code not in this site's treatment_names is excluded. Checked
# per-site rather than hardcoded (e.g. "B") - a code can mean different
# things at different sites (see DECISIONS_LOG, Script 3 #1).
strips <- st_read(file.path(base_path, site_name, strips_path))

untracked_codes <- setdiff(strips$treat, treatment_names$treat)
if (length(untracked_codes) > 0) {
  message("Excluding strip code(s) not in treatment metadata: ",
          paste(untracked_codes, collapse = ", "))
}

strips_clean <- strips %>%
  filter(treat %in% treatment_names$treat) %>%
  left_join(treatment_names %>% select(treat, treatment_name = `Shorthand Name`,
                                       plot_order = `Order in Paddock`),
            by = "treat")


# ---- 3b. Function: build index raster(s) per source ------------------------
# Drone: NDVI only (single band, no red-edge available - see Script 2).
# Satellite: NDVI and NDRE as a named 2-layer stack (NDRE calculated from
# the raw 10-band stack's nbart_nir_1/nbart_red_edge_1 - see Script 1's
# raw_path). The 0-value no-data convention is ASSUMED to apply to the raw
# stack too, not independently verified (see DECISIONS_LOG).

build_index_raster <- function(file_path, raw_path, source) {
  ndvi_r <- rast(file_path)
  NAflag(ndvi_r) <- 0
  
  if (source == "satellite") {
    raw_r  <- rast(raw_path)
    ndre_r <- (raw_r[["nbart_nir_1"]] - raw_r[["nbart_red_edge_1"]]) /
      (raw_r[["nbart_nir_1"]] + raw_r[["nbart_red_edge_1"]])
    NAflag(ndre_r) <- 0
    stack <- c(ndvi_r, ndre_r)
    names(stack) <- c("NDVI", "NDRE")
    return(stack)
  }
  
  names(ndvi_r) <- "NDVI"
  ndvi_r
}

# exact_extract() names columns differently for 1-layer (plain mean/stdev/
# count, from drone) vs 2-layer (mean.NDVI/mean.NDRE etc., from satellite)
# input. Standardise to the 2-layer naming so satellite and drone rows can
# be combined cleanly - drone's NDRE columns become NA.
standardise_index_cols <- function(df) {
  if ("mean" %in% names(df)) {
    df <- df %>%
      rename(mean.NDVI = mean, stdev.NDVI = stdev, count.NDVI = count) %>%
      mutate(mean.NDRE = NA_real_, stdev.NDRE = NA_real_, count.NDRE = NA_real_)
  }
  df
}

# ---- 4. Test zonal extraction on ONE satellite date first -----------------
# Proves the method works before looping over every date/source.

test_row <- site_inventory %>%
  filter(source == "satellite") %>%
  slice(1)

r_test <- rast(test_row$file_path)
NAflag(r_test) <- 0   # same edge/no-data encoding confirmed in Script 2

zonal_test <- exact_extract(r_test, strips_clean, fun = c("mean", "stdev", "count"))

zonal_test <- strips_clean %>%
  st_drop_geometry() %>%
  select(plot, treat, treatment_name, plot_order) %>%
  bind_cols(zonal_test) %>%
  mutate(date = test_row$date, source = test_row$source)

zonal_test

# ---- 5. Loop zonal extraction across every satellite date -----------------

extract_zonal <- function(file_path, raw_path, date, source, strips) {
  idx <- build_index_raster(file_path, raw_path, source)
  
  exact_extract(idx, strips, fun = c("mean", "stdev", "count")) %>%
    standardise_index_cols() %>%
    bind_cols(strips %>% st_drop_geometry() %>%
                select(plot, treat, treatment_name, plot_order)) %>%
    mutate(date = date, source = source)
}

satellite_zonal <- site_inventory %>%
  filter(source == "satellite") %>%
  rowwise() %>%
  reframe(extract_zonal(file_path, raw_path, date, source, strips_clean)) %>%
  ungroup()

nrow(satellite_zonal)   # should be 34 dates x 8 strips = 272
satellite_zonal %>% distinct(date) %>% nrow()  # sanity check: 34 unique dates

names(satellite_zonal)

# ---- 6. Test drone zonal extraction on ONE date first ----------------------

drone_test_row <- site_inventory %>%
  filter(source == "drone") %>%
  slice(1)

r_drone_test <- rast(drone_test_row$file_path)
NAflag(r_drone_test) <- 0   # assumed same encoding as satellite - see DECISIONS_LOG open item

system.time({
  zonal_drone_test <- exact_extract(r_drone_test, strips_clean, fun = c("mean", "stdev", "count"))
})

zonal_drone_test <- strips_clean %>%
  st_drop_geometry() %>%
  select(plot, treat, treatment_name, plot_order) %>%
  bind_cols(zonal_drone_test) %>%
  mutate(date = drone_test_row$date, source = drone_test_row$source)

zonal_drone_test

# ---- 7. Loop zonal extraction across both drone dates ----------------------

drone_zonal <- site_inventory %>%
  filter(source == "drone") %>%
  rowwise() %>%
  reframe(extract_zonal(file_path, raw_path, date, source, strips_clean)) %>%
  ungroup()

nrow(drone_zonal)   # should be 2 dates x 8 strips = 16
drone_zonal %>% distinct(date) %>% nrow()  # sanity check: 2 unique dates

# ---- 8. Combine satellite + drone zonal stats (Level 1: treatment only) ---

zonal_stats <- bind_rows(satellite_zonal, drone_zonal) %>%
  arrange(date, plot_order)

zonal_stats

write_csv(zonal_stats, file.path(output_folder, paste0(site_name, "_zonal_stats_script3.csv")))
saveRDS(zonal_stats,  file.path(output_folder, paste0(site_name, "_zonal_stats_script3.rds")))


# =============================================================================
# ZONE ADDITIONS BELOW - Level 2 (zone only) and Level 3 (strip x zone)
# =============================================================================

# ---- 9. Load zones from Script 1, check CRS/geometry before intersecting --

zones_labelled <- readRDS(file.path(output_folder, paste0(site_name, "_zones_labelled_script1.rds")))

st_crs(strips_clean) == st_crs(zones_labelled)   # should be TRUE
st_is_valid(strips_clean) %>% all()               # should be TRUE
st_is_valid(zones_labelled) %>% all()             # should be TRUE

# If either validity check is FALSE, run this before continuing:
# strips_clean   <- st_make_valid(strips_clean)
# zones_labelled <- st_make_valid(zones_labelled)
# If the CRS check is FALSE, run this before continuing:
# zones_labelled <- st_transform(zones_labelled, st_crs(strips_clean))


# ---- 10. Level 2: NDVI per zone (ignoring treatment) -----------------------
# Same method as Section 5, just pointed at zones instead of strips. Tells us
# how much NDVI variation is driven by soil type alone.

extract_zonal_zone <- function(file_path, raw_path, date, source, zones) {
  idx <- build_index_raster(file_path, raw_path, source)
  
  exact_extract(idx, zones, fun = c("mean", "stdev", "count")) %>%
    standardise_index_cols() %>%
    bind_cols(zones %>% st_drop_geometry() %>% select(zone_code, zone_label)) %>%
    mutate(date = date, source = source)
}

zone_zonal_satellite <- site_inventory %>%
  filter(source == "satellite") %>%
  rowwise() %>%
  reframe(extract_zonal_zone(file_path, raw_path, date, source, zones_labelled)) %>%
  ungroup()

zone_zonal_drone <- site_inventory %>%
  filter(source == "drone") %>%
  rowwise() %>%
  reframe(extract_zonal_zone(file_path, raw_path, date, source, zones_labelled)) %>%
  ungroup()



zone_zonal <- bind_rows(zone_zonal_satellite, zone_zonal_drone)

zone_zonal %>% filter(source == "drone")  # quick look: NDVI by zone, 2 drone dates

write_csv(zone_zonal, file.path(output_folder, paste0(site_name, "_zone_zonal_stats_script3.csv")))
saveRDS(zone_zonal,  file.path(output_folder, paste0(site_name, "_zone_zonal_stats_script3.rds")))


# ---- 11. Level 3: intersect strips with zones (strip x zone polygons) -----
# A strip crossing 3 zones becomes 3 separate polygons, each tagged with both
# its treatment AND its zone. Tiny sliver intersections are dropped as noise.

zones_labelled <- st_make_valid(zones_labelled)

strip_zone <- st_intersection(strips_clean, zones_labelled) %>%
  mutate(area_m2 = as.numeric(st_area(.))) %>%
  filter(area_m2 > 5)   # drop slivers under 5 sqm - adjust if needed once we see the data

as.data.frame(strip_zone %>% st_drop_geometry() %>% count(treat, zone_label))


# ---- 12. Level 3 extraction: NDVI per strip x zone polygon -----------------

extract_zonal_stripzone <- function(file_path, raw_path, date, source, strip_zone) {
  idx <- build_index_raster(file_path, raw_path, source)
  
  exact_extract(idx, strip_zone, fun = c("mean", "stdev", "count")) %>%
    standardise_index_cols() %>%
    bind_cols(strip_zone %>% st_drop_geometry() %>%
                select(plot, treat, treatment_name, plot_order, zone_code, zone_label)) %>%
    mutate(date = date, source = source)
}

stripzone_zonal_satellite <- site_inventory %>%
  filter(source == "satellite") %>%
  rowwise() %>%
  reframe(extract_zonal_stripzone(file_path, raw_path, date, source, strip_zone)) %>%
  ungroup()

stripzone_zonal_drone <- site_inventory %>%
  filter(source == "drone") %>%
  rowwise() %>%
  reframe(extract_zonal_stripzone(file_path, raw_path, date, source, strip_zone)) %>%
  ungroup()



strip_zone_zonal_stats <- bind_rows(stripzone_zonal_satellite, stripzone_zonal_drone) %>%
  arrange(date, plot_order, zone_label)

strip_zone_zonal_stats %>% filter(source == "drone")  # check: treatment split WITHIN each zone


# ---- 13. Save Level 3 output -----------------------------------------------

write_csv(strip_zone_zonal_stats,
          file.path(output_folder, paste0(site_name, "_strip_zone_zonal_stats_script3.csv")))
saveRDS(strip_zone_zonal_stats,
        file.path(output_folder, paste0(site_name, "_strip_zone_zonal_stats_script3.rds")))



# 1. Confirm drone has the same column set, with NDRE as NA
names(drone_zonal)
drone_zonal %>% select(mean.NDVI, mean.NDRE, stdev.NDRE, count.NDRE) %>% head()

# 2. Row counts still check out
nrow(satellite_zonal)   # expect 272
nrow(drone_zonal)       # expect 16
nrow(zone_zonal)        # expect 32 x 2? actually check whatever your zone-level total should be
nrow(strip_zone_zonal_stats)  # expect 288 (34 sat + 2 drone) x 3 zones? check against what Level 3 was before

# 3. NDVI vs NDRE relationship for a few satellite rows
satellite_zonal %>% filter(date == min(date)) %>% select(treat, mean.NDVI, mean.NDRE)
