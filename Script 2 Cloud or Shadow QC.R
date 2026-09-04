# =============================================================================
# Script 2: NDVI/NDRE Raster QC
# -----------------------------------------------------------------------------
# Purpose : For each satellite/drone raster, mask no-data/edge pixels and
#           produce a per-date summary (mean, sd, % masked) so implausible
#           or heavily-masked dates are visible before Script 3 does zonal
#           extraction per strip.
#           Satellite: NDVI (from the pre-made NDVI file) AND NDRE
#           (calculated from the raw 10-band stack's nbart_nir_1 and
#           nbart_red_edge_1 bands - see Script 1's raw_path).
#           Drone: NDVI only - no red-edge band available in the
#           single-band drone product, so NDRE is not calculated for drone.
#           NOTE: no per-pixel cloud mask exists for Sentinel (see
#           NDVI_Viewer_Metadata.docx - cloud screening is a whole-scene
#           S2Cloudless statistic applied BEFORE export, not a retained band).
#           This script instead checks for no-data edge encoding (confirmed
#           as value == 0 for the pre-made NDVI file - verified live, not
#           just from docs, since this has changed before. The same
#           convention is ASSUMED, not independently verified, for the raw
#           band stack used for NDRE - see DECISIONS_LOG).
#           Drone rasters are aggregated to ~1m before stats are computed -
#           native drone resolution (~5.6cm) is ~7000x more pixels than
#           satellite, far more than a paddock-mean stat needs and much
#           slower to process at full resolution.
#
# Inputs  : {site_name}_site_inventory_script1.rds  (from Script 1; satellite
#           rows need both file_path and raw_path)
#
# Outputs : {site_name}_raster_qc_script2.csv/.rds
#           (one row per satellite/drone date: mean/sd for NDVI and NDRE
#           [drone NDRE columns are NA], pct_masked)
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

# ---- 2. Functions: load raster(s), mask 0s, return a summary row ----------
# Satellite: NDVI from file_path, NDRE calculated from raw_path (the 10-band
# stack) using nbart_nir_1/nbart_red_edge_1. The 0-value no-data convention
# is ASSUMED to carry over from the pre-made NDVI file to the raw stack -
# not yet independently verified for the raw bands (see DECISIONS_LOG).
# Drone: NDVI only - no red-edge band available in the single-band drone
# product, so NDRE cannot be calculated with current drone data.

qc_satellite <- function(file_path, raw_path, date) {
  ndvi_r <- rast(file_path)
  NAflag(ndvi_r) <- 0
  
  raw_r  <- rast(raw_path)
  ndre_r <- (raw_r[["nbart_nir_1"]] - raw_r[["nbart_red_edge_1"]]) /
    (raw_r[["nbart_nir_1"]] + raw_r[["nbart_red_edge_1"]])
  NAflag(ndre_r) <- 0
  
  n_total  <- ncell(ndvi_r)
  n_valid  <- as.numeric(global(!is.na(ndvi_r), "sum", na.rm = TRUE))
  n_masked <- n_total - n_valid
  
  tibble(
    date        = date,
    source      = "satellite",
    n_pixels    = n_total,
    pct_masked  = round(100 * n_masked / n_total, 1),
    mean_ndvi   = round(as.numeric(global(ndvi_r, "mean", na.rm = TRUE)), 3),
    sd_ndvi     = round(as.numeric(global(ndvi_r, "sd", na.rm = TRUE)), 3),
    mean_ndre   = round(as.numeric(global(ndre_r, "mean", na.rm = TRUE)), 3),
    sd_ndre     = round(as.numeric(global(ndre_r, "sd", na.rm = TRUE)), 3)
  )
}

qc_drone <- function(file_path, date) {
  r <- rast(file_path)
  r <- aggregate(r, fact = 18, fun = "mean", na.rm = TRUE)
  NAflag(r) <- 0
  
  n_total  <- ncell(r)
  n_valid  <- as.numeric(global(!is.na(r), "sum", na.rm = TRUE))
  n_masked <- n_total - n_valid
  
  tibble(
    date        = date,
    source      = "drone",
    n_pixels    = n_total,
    pct_masked  = round(100 * n_masked / n_total, 1),
    mean_ndvi   = round(as.numeric(global(r, "mean", na.rm = TRUE)), 3),
    sd_ndvi     = round(as.numeric(global(r, "sd", na.rm = TRUE)), 3),
    mean_ndre   = NA_real_,
    sd_ndre     = NA_real_
  )
}

# ---- 3. Run it across every satellite + drone date in the inventory -------
# (Planet held back for now - its 0/no-data convention hasn't been checked
# yet and is very likely different, since it's an 8-band SR product, not a
# pre-made NDVI file like satellite/drone.)

satellite_qc <- site_inventory %>%
  filter(source == "satellite", !is.na(file_path)) %>%
  rowwise() %>%
  mutate(qc = list(qc_satellite(file_path, raw_path, date))) %>%
  ungroup() %>%
  pull(qc) %>%
  bind_rows()

drone_qc <- site_inventory %>%
  filter(source == "drone", !is.na(file_path)) %>%
  rowwise() %>%
  mutate(qc = list(qc_drone(file_path, date))) %>%
  ungroup() %>%
  pull(qc) %>%
  bind_rows()

raster_qc <- bind_rows(satellite_qc, drone_qc)

raster_qc


# ---- See the full table ----------------------------------------------------
print(raster_qc, n = Inf)


# ---- Plot mean NDVI over time, by source, with error bars for spread ------
library(ggplot2)

ggplot(raster_qc, aes(x = date, y = mean_ndvi, colour = source)) +
  geom_line(data = raster_qc %>% filter(source == "satellite")) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_ndvi - sd_ndvi, ymax = mean_ndvi + sd_ndvi), width = 2) +
  labs(title = paste("Paddock-mean NDVI over time —", site_name),
       x = NULL, y = "Mean NDVI") +
  theme_minimal()


# ---- 4. Save the QC summary for use by Script 3 ----------------------------
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

write_csv(raster_qc, file.path(output_folder, paste0(site_name, "_raster_qc_script2.csv")))
saveRDS(raster_qc,  file.path(output_folder, paste0(site_name, "_raster_qc_script2.rds")))
