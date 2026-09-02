# =============================================================================
# Script 2: NDVI Raster QC
# -----------------------------------------------------------------------------
# Purpose : For each satellite/drone NDVI raster, mask no-data/edge pixels
#           and produce a per-date summary (mean, sd, % masked) so implausible
#           or heavily-masked dates are visible before Script 3 does zonal
#           extraction per strip.
#           NOTE: no per-pixel cloud mask exists for Sentinel (see
#           NDVI_Viewer_Metadata.docx - cloud screening is a whole-scene
#           S2Cloudless statistic applied BEFORE export, not a retained band).
#           This script instead checks for no-data edge encoding (confirmed
#           as value == 0 for satellite - verified live, not just from docs,
#           since this has changed before).
#           Drone rasters are aggregated to ~1m before stats are computed -
#           native drone resolution (~5.6cm) is ~7000x more pixels than
#           satellite, far more than a paddock-mean stat needs and much
#           slower to process at full resolution.
#
# Inputs  : {site_name}_site_inventory_script1.rds  (from Script 1)
#
# Outputs : {site_name}_raster_qc_script2.csv/.rds
#           (one row per satellite/drone date: mean, sd, pct_masked)
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

# ---- 2. Function: load one NDVI raster, mask 0s, return a summary row -----
qc_one_raster <- function(path, date, source) {
  r <- rast(path)
  
  if (source == "drone") {
    r <- aggregate(r, fact = 18, fun = "mean", na.rm = TRUE)
  }
  
  NAflag(r) <- 0
  
  n_total  <- ncell(r)
  n_valid  <- as.numeric(global(!is.na(r), "sum", na.rm = TRUE))
  n_masked <- n_total - n_valid
  
  tibble(
    date        = date,
    source      = source,
    n_pixels    = n_total,
    pct_masked  = round(100 * n_masked / n_total, 1),
    mean_ndvi   = round(as.numeric(global(r, "mean", na.rm = TRUE)), 3),
    sd_ndvi     = round(as.numeric(global(r, "sd", na.rm = TRUE)), 3)
  )
}

# ---- 3. Run it across every satellite + drone date in the inventory -------
# (Planet held back for now - its 0/no-data convention hasn't been checked
# yet and is very likely different, since it's an 8-band SR product, not a
# pre-made NDVI file like satellite/drone.)

raster_qc <- site_inventory %>%
  filter(source %in% c("satellite", "drone")) %>%
  filter(!is.na(file_path)) %>%
  rowwise() %>%
  mutate(qc = list(qc_one_raster(file_path, date, source))) %>%
  ungroup() %>%
  pull(qc) %>%
  bind_rows()

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
