# =============================================================================
# Script 3: Zonal Extraction
# -----------------------------------------------------------------------------
# Purpose : For each strip polygon (treatment), extract NDVI zonal statistics
#           (mean, sd, n_pixels) per date, for satellite and drone. Drone is
#           kept at native resolution per strip (not aggregated, unlike
#           Script 2's paddock-level QC - see DECISIONS_LOG) since preserving
#           fine within-strip detail is the whole point of testing drone's
#           added value at sub-paddock scale.
#
# Inputs  : {site_name}_site_inventory_script1.rds
#           {site_name}_raster_qc_script2.rds
#           trial.plan shapefile (strip polygons) - path from metadata
#           treatment names metadata (per-site treatment codes/labels)
#
# Outputs : {site_name}_zonal_stats_script3.csv/.rds
#           (one row per strip, per date, per source)
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

extract_zonal <- function(file_path, date, source, strips) {
  r <- rast(file_path)
  NAflag(r) <- 0
  
  exact_extract(r, strips, fun = c("mean", "stdev", "count")) %>%
    bind_cols(strips %>% st_drop_geometry() %>%
                select(plot, treat, treatment_name, plot_order)) %>%
    mutate(date = date, source = source)
}

satellite_zonal <- site_inventory %>%
  filter(source == "satellite") %>%
  rowwise() %>%
  reframe(extract_zonal(file_path, date, source, strips_clean)) %>%
  ungroup()

nrow(satellite_zonal)   # should be 34 dates x 8 strips = 272
satellite_zonal %>% distinct(date) %>% nrow()  # sanity check: 34 unique dates


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
  reframe(extract_zonal(file_path, date, source, strips_clean)) %>%
  ungroup()

nrow(drone_zonal)   # should be 2 dates x 8 strips = 16
drone_zonal %>% distinct(date) %>% nrow()  # sanity check: 2 unique dates

# ---- 8. Combine satellite + drone zonal stats, save for Script 4 ----------

zonal_stats <- bind_rows(satellite_zonal, drone_zonal) %>%
  arrange(date, plot_order)

zonal_stats

write_csv(zonal_stats, file.path(output_folder, paste0(site_name, "_zonal_stats_script3.csv")))
saveRDS(zonal_stats,  file.path(output_folder, paste0(site_name, "_zonal_stats_script3.rds")))
