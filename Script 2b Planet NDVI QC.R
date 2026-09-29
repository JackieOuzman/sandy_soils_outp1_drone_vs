# =============================================================================
# Script 2b: Planet NDVI/NDRE QC
# -----------------------------------------------------------------------------
# Purpose : Planet-specific equivalent of Script 2. Unlike satellite/drone,
#           Planet delivers raw 8-band surface reflectance (not pre-made
#           indices), so NDVI and NDRE must be calculated from Red/NIR and
#           NIR/RedEdge bands respectively. Also applies Planet's proper
#           per-pixel quality mask (udm2 band 1 = clear), which is a genuine
#           cloud/shadow mask - unlike satellite, which only has a crude
#           0-value edge mask (see DECISIONS_LOG).
#           Unlike Sentinel-2, Planet does NOT screen for cloud prior to
#           publication - every captured date is catalogued regardless of
#           cloud cover. For consistency with Sentinel-2's 30% whole-scene
#           exclusion rule, whole dates with >30% of pixels masked as
#           not-clear are flagged for exclusion here, on top of (not
#           instead of) the existing per-pixel masking.
#
# Inputs  : {site_name}_site_inventory_script1.rds  (planet rows: file_path +
#           mask_path)
#
# Outputs : {site_name}_planet_raster_qc_script2b.csv/.rds
#           (one row per Planet date: mean/sd for NDVI and NDRE, pct_masked,
#           excluded_cloud flag)
#           {site_name}_cloud_exclusion_script2b.png (Figure, Section 3.1)
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below.
# =============================================================================

library(dplyr)
library(readr)
library(terra)
library(readxl)
library(ggplot2)

# ============================== SITE CONFIG =================================
#site_name     <- "1.Walpeup_MRS125"
site_name     <- "2.Crystal_Brook_Brians_House"
base_path     <- "H:/Output-1"
metadata_path <- file.path(base_path, "0.Site-info",
                           "names of treatments per site 2025 metadata and other info.xlsx")
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
  ndre <- (r[["nir"]] - r[["rededge"]]) / (r[["nir"]] + r[["rededge"]])
  ndvi <- mask(ndvi, m[["clear"]], maskvalues = 0)
  ndre <- mask(ndre, m[["clear"]], maskvalues = 0)
  
  n_total <- ncell(ndvi)
  # Pixels outside Planet's image footprint are NA in the SR bands and 0 in
  # udm2 "clear" - so count only pixels WITH image data, or no-coverage gets
  # counted as cloud (at Brians House ~44% of every clip is no-coverage).
  n_data  <- as.numeric(global(!is.na(r[["nir"]]), "sum"))
  n_valid <- as.numeric(global(!is.na(ndvi), "sum", na.rm = TRUE))
  pct_masked <- round(100 * (n_data - n_valid) / n_data, 1)   # % of imaged pixels not clear
  
  tibble(
    date       = date,
    source     = "planet",
    n_pixels   = n_total,
    pct_masked = pct_masked,
    mean_ndvi  = round(as.numeric(global(ndvi, "mean", na.rm = TRUE)), 3),
    sd_ndvi    = round(as.numeric(global(ndvi, "sd", na.rm = TRUE)), 3),
    mean_ndre  = round(as.numeric(global(ndre, "mean", na.rm = TRUE)), 3),
    sd_ndre    = round(as.numeric(global(ndre, "sd", na.rm = TRUE)), 3)
  )
}

# ---- 5. Run across all Planet dates -----------------------------------------
cloud_threshold_pct <- 30

planet_raster_qc <- planet_rows %>%
  rowwise() %>%
  reframe(qc_one_planet(file_path, mask_path, date)) %>%
  ungroup() %>%
  mutate(excluded_cloud = pct_masked > cloud_threshold_pct)

planet_raster_qc %>% filter(excluded_cloud) %>% select(date, pct_masked)

planet_raster_qc %>% print(n = Inf)

# ---- 6. Quick check: does the footprint genuinely shrink, or is it just masking?
# Compare total extent (not masked) for a normal date vs a shifted-extent date

#r_normal <- rast(planet_rows$file_path[planet_rows$date == as.Date("2025-04-29")])#site 1
#r_shifted <- rast(planet_rows$file_path[planet_rows$date == as.Date("2025-05-30")])#site 1
#ext(r_normal)
#ext(r_shifted)

#replaced with this...
r <- rast(planet_rows$file_path[1])
m <- rast(planet_rows$mask_path[1])

ncell(r)                                               # total pixels in the clip
global(is.na(r[["nir"]]), "sum")                       # pixels with no data (NA)
global(r[["nir"]] == 0, "sum", na.rm = TRUE)           # pixels coded 0
freq(m[["clear"]])                                     # udm2 clear band: counts of 0 and 1
global(is.na(m[["clear"]]), "sum")                     # NA in the mask


# ---- 7. Save Script 2b output -----------------------------------------------
write_csv(planet_raster_qc, file.path(output_folder, paste0(site_name, "_planet_raster_qc_script2b.csv")))
saveRDS(planet_raster_qc,  file.path(output_folder, paste0(site_name, "_planet_raster_qc_script2b.rds")))


# ---- 8. Plot: Planet dates used vs excluded, alongside Sentinel-2 ----------
# Sentinel-2 is pre-filtered upstream of this pipeline (see Script 1 caption)
# so every Sentinel-2 date shown here was already accepted before it reached
# us; only Planet has a genuine used/excluded split computable from our data.

season_2025 <- read_excel(metadata_path, sheet = "seasons") %>%
  filter(Site == site_name, Year == 2025)

sowing_date  <- as.Date(season_2025$`Sowing date`)
harvest_date <- as.Date(season_2025$`Harvest date`)

cloud_plot_data <- bind_rows(
  planet_raster_qc %>%
    transmute(date, source = "Planet",
              status = if_else(excluded_cloud, "Excluded (>30% masked)", "Used")),
  site_inventory %>%
    filter(source == "satellite") %>%
    transmute(date, source = "Sentinel-2", status = "Used (pre-filtered)")
) %>%
  mutate(source = factor(source, levels = c("Sentinel-2", "Planet")))

cloud_plot <- ggplot(cloud_plot_data, aes(x = date, y = source, colour = status, shape = status)) +
  geom_vline(xintercept = sowing_date, linetype = "dotted", colour = "darkgreen") +
  geom_vline(xintercept = harvest_date, linetype = "dotted", colour = "sienna") +
  geom_point(size = 3) +
  annotate("text", x = sowing_date, y = Inf, label = "Sowing",
           angle = 90, vjust = -0.5, hjust = 1.1, size = 3, colour = "darkgreen") +
  annotate("text", x = harvest_date, y = Inf, label = "Harvest",
           angle = 90, vjust = -0.5, hjust = 1.1, size = 3, colour = "sienna") +
  scale_colour_manual(values = c("Used" = "steelblue", "Excluded (>30% masked)" = "grey70",
                                 "Used (pre-filtered)" = "steelblue")) +
  scale_shape_manual(values = c("Used" = 16, "Excluded (>30% masked)" = 4,
                                "Used (pre-filtered)" = 16)) +
  labs(title = paste("Cloud-based date exclusion —", site_name),
       subtitle = "Planet dates excluded by the 30% masked-pixel rule, vs Sentinel-2",
       x = NULL, y = NULL, colour = NULL, shape = NULL,
       caption = paste(
         "Sentinel-2 dates were pre-filtered for cloud cover (<30%) before reaching this",
         "pipeline, so all dates shown here were already accepted; the pre-filtering step",
         "itself is not auditable from this pipeline's data. Planet dates were not",
         "pre-filtered, so the used/excluded split shown here (30% masked-pixel threshold)",
         "is fully computed within this pipeline.",
         sep = "\n"
       )) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0, size = 8, colour = "grey30"))

cloud_plot

ggsave(file.path(output_folder, paste0(site_name, "_cloud_exclusion_script2b.png")),
       cloud_plot, width = 9, height = 4, dpi = 300)


