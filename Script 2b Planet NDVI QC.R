# =============================================================================
# Script 2b: Planet NDVI QC
# -----------------------------------------------------------------------------
# Purpose : Planet-specific equivalent of Script 2. Unlike satellite/drone,
#           Planet delivers raw 8-band surface reflectance (not pre-made
#           NDVI), so NDVI must be calculated from Red/NIR bands first. Also
#           applies Planet's proper per-pixel quality mask (udm2 band 1 =
#           clear), which is a genuine cloud/shadow mask - unlike satellite,
#           which only has a crude 0-value edge mask (see DECISIONS_LOG).
#
# Inputs  : {site_name}_site_inventory_script1.rds  (planet rows: file_path +
#           mask_path)
#
# Outputs : {site_name}_planet_raster_qc_script2b.csv/.rds
#           (one row per Planet date: mean, sd, pct_masked)
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below.
# =============================================================================

library(dplyr)
library(readr)
library(terra)

# ============================== SITE CONFIG =================================
site_name     <- "1.Walpeup_MRS125"
pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"
output_folder         <- file.path(pipeline_output_base, site_name)
# =============================================================================

# ---- 1. Load Script 1's saved inventory ------------------------------------
site_inventory <- readRDS(file.path(output_folder, paste0(site_name, "_site_inventory_script1.rds")))

planet_rows <- site_inventory %>% filter(source == "planet")

# ---- 2. Inspect band order and udm2 structure on ONE file first -----------
sample_path <- planet_rows$file_path[1]
sample_mask_path <- planet_rows$mask_path[1]

r_sample <- rast(sample_path)
nlyr(r_sample)
names(r_sample)   # check if bands are labelled, or just numbered

mask_sample <- rast(sample_mask_path)
nlyr(mask_sample)
names(mask_sample)


# ---- 3. Calculate NDVI and apply the udm2 clear-mask, for one file --------

ndvi_sample <- (r_sample[["nir"]] - r_sample[["red"]]) / (r_sample[["nir"]] + r_sample[["red"]])

clear_mask <- mask_sample[["clear"]]

ndvi_masked <- mask(ndvi_sample, clear_mask, maskvalues = 0)   # 0 = not clear -> becomes NA

global(ndvi_sample, "mean", na.rm = TRUE)    # before masking
global(ndvi_masked, "mean", na.rm = TRUE)    # after masking
global(clear_mask, "mean", na.rm = TRUE)     # proportion of pixels that were clear (0-1)


# ---- 4. Rename the NDVI layer properly, then loop across all 58 Planet dates
# Same edge/no-data check as satellite - Planet's "no coverage" pixels need
# checking too (haven't confirmed this yet - could be NA already, or 0, or
# something else; check before assuming).

names(ndvi_sample) <- "NDVI"

qc_one_planet <- function(file_path, mask_path, date) {
  r <- rast(file_path)
  m <- rast(mask_path)
  
  ndvi <- (r[["nir"]] - r[["red"]]) / (r[["nir"]] + r[["red"]])
  ndvi <- mask(ndvi, m[["clear"]], maskvalues = 0)
  
  n_total <- ncell(ndvi)
  n_valid <- as.numeric(global(!is.na(ndvi), "sum", na.rm = TRUE))
  pct_masked <- round(100 * (n_total - n_valid) / n_total, 1)
  
  tibble(
    date       = date,
    source     = "planet",
    n_pixels   = n_total,
    pct_masked = pct_masked,
    mean_ndvi  = round(as.numeric(global(ndvi, "mean", na.rm = TRUE)), 3),
    sd_ndvi    = round(as.numeric(global(ndvi, "sd", na.rm = TRUE)), 3)
  )
}

# ---- 5. Run across all Planet dates -----------------------------------------
planet_raster_qc <- planet_rows %>%
  rowwise() %>%
  reframe(qc_one_planet(file_path, mask_path, date)) %>%
  ungroup()

planet_raster_qc %>% print(n = Inf)

# ---- 6. Quick check: does the footprint genuinely shrink, or is it just masking?
# Compare total extent (not masked) for a normal date vs a shifted-extent date

r_normal <- rast(planet_rows$file_path[planet_rows$date == as.Date("2025-04-29")])
r_shifted <- rast(planet_rows$file_path[planet_rows$date == as.Date("2025-05-30")])

ext(r_normal)
ext(r_shifted)
# ---- 7. Save Script 2b output -----------------------------------------------
write_csv(planet_raster_qc, file.path(output_folder, paste0(site_name, "_planet_raster_qc_script2b.csv")))
saveRDS(planet_raster_qc,  file.path(output_folder, paste0(site_name, "_planet_raster_qc_script2b.rds")))
