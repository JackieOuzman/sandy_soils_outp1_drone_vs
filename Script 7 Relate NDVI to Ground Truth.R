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
#           {site_name}_biomass_vs_ndvi_script7.png
#           {site_name}_harvest_groundtruth_pointlevel_script7.csv/.rds
#           {site_name}_harvest_correlations_adjusted_script7.csv/.rds
#           (same tests re-fitted within treatment, and within treatment + zone)
#           (harvest variables vs FLOWERING-date NDVI/NDRE - the crop is senescent
#           at the 26 Nov harvest sampling, so same-day NDVI says little about yield)
#           {site_name}_biomass_vs_ndvi_script7.png
#           {site_name}_planet_raster_qc_script2b.rds  (excluded_cloud flag)
#           {site_name}_groundtruth_correlation_pvalues_script7.csv/.rds
#           
#           
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

# Drop Planet dates flagged as too cloudy in Script 2b (same 30% rule as Script 3b)
planet_raster_qc <- readRDS(file.path(output_folder, paste0(site_name, "_planet_raster_qc_script2b.rds")))
excluded_dates   <- planet_raster_qc %>% filter(excluded_cloud) %>% pull(date)

site_inventory <- site_inventory %>%
  filter(!(source == "planet" & date %in% excluded_dates))

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

# ---- 7b. p-values for the correlations above (two-sided cor.test) ---------
correlation_pvalues <- tibble(
  variable = rep(c("Establishment", "Biomass_flowering"), each = 2),
  metric   = rep(c("NDVI", "NDRE"), times = 2),
  p_satellite = c(
    cor.test(establishment_pts$establishment_plants_m2, establishment_pts$ndvi_satellite)$p.value,
    cor.test(establishment_pts$establishment_plants_m2, establishment_pts$ndre_satellite)$p.value,
    cor.test(biomass_pts$Biomass_flowering, biomass_pts$ndvi_satellite)$p.value,
    cor.test(biomass_pts$Biomass_flowering, biomass_pts$ndre_satellite)$p.value
  ),
  p_planet = c(
    cor.test(establishment_pts$establishment_plants_m2, establishment_pts$ndvi_planet)$p.value,
    cor.test(establishment_pts$establishment_plants_m2, establishment_pts$ndre_planet)$p.value,
    cor.test(biomass_pts$Biomass_flowering, biomass_pts$ndvi_planet)$p.value,
    cor.test(biomass_pts$Biomass_flowering, biomass_pts$ndre_planet)$p.value
  )
)

correlation_pvalues

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

write_csv(correlation_pvalues,
          file.path(output_folder, paste0(site_name, "_groundtruth_correlation_pvalues_script7.csv")))
saveRDS(correlation_pvalues,
        file.path(output_folder, paste0(site_name, "_groundtruth_correlation_pvalues_script7.rds")))


# ---- 10. Harvest variables vs flowering-date NDVI/NDRE ---------------------
# Predictor = the flowering-matched rasters already built in Section 5
# (satellite 24 Sep, Planet 18 Sep), extracted at the 48 harvest points.
# Pre-specified set: Grain_yield and Biomass_maturity (2 variables x 2 indices
# x 2 sources = 8 tests, Bonferroni threshold 0.05/8 = 0.00625).
# Thousand_grain_weight and Harvest_index are exploratory.
# Drone not included here.

harvest_pts <- readRDS(file.path(output_folder, paste0(site_name, "_harvest_points_geo_script6.rds")))

harv_sat_idx    <- extract_indices_at_points(satellite_indices_raster(bio_sat_path, bio_sat_raw),
                                             harvest_pts$geometry, buffer_m)
harv_planet_idx <- extract_indices_at_points(planet_indices_raster(bio_planet_path, bio_planet_mask),
                                             harvest_pts$geometry, buffer_m)

harvest_pts <- harvest_pts %>%
  mutate(
    ndvi_satellite = harv_sat_idx$mean.NDVI,
    ndre_satellite = harv_sat_idx$mean.NDRE,
    ndvi_planet    = harv_planet_idx$mean.NDVI,
    ndre_planet    = harv_planet_idx$mean.NDRE
  )

# Check: 48 points, no NAs in any index
nrow(harvest_pts)
harvest_pts %>% st_drop_geometry() %>%
  summarise(across(c(ndvi_satellite, ndre_satellite, ndvi_planet, ndre_planet),
                   ~ sum(is.na(.x))))

# ---- 10b. Correlations (r, p, n) for every variable x source x index -------
cor_row <- function(variable, source, metric) {
  x  <- harvest_pts[[paste0(tolower(metric), "_", source)]]
  y  <- harvest_pts[[variable]]
  ct <- cor.test(x, y)
  tibble(variable, source, metric,
         n = sum(complete.cases(x, y)),
         r = unname(ct$estimate),
         p = ct$p.value)
}

harvest_correlations <- tidyr::expand_grid(
  variable = c("Grain_yield", "Biomass_maturity", "Thousand_grain_weight", "Harvest_index"),
  source   = c("satellite", "planet"),
  metric   = c("NDVI", "NDRE")
) %>%
  purrr::pmap(cor_row) %>%
  bind_rows() %>%
  mutate(set = if_else(variable %in% c("Grain_yield", "Biomass_maturity"),
                       "pre-specified", "exploratory")) %>%
  arrange(desc(set), variable, source, metric)

print(harvest_correlations, n = Inf)

# ---- 11. Save harvest outputs ----------------------------------------------
write_csv(harvest_pts %>% st_drop_geometry(),
          file.path(output_folder, paste0(site_name, "_harvest_groundtruth_pointlevel_script7.csv")))
saveRDS(harvest_pts,
        file.path(output_folder, paste0(site_name, "_harvest_groundtruth_pointlevel_script7.rds")))

write_csv(harvest_correlations,
          file.path(output_folder, paste0(site_name, "_harvest_correlations_script7.csv")))
saveRDS(harvest_correlations,
        file.path(output_folder, paste0(site_name, "_harvest_correlations_script7.rds")))


# ---- 12. Do the harvest correlations survive removing treatment differences?
# Section 10b correlates each index with yield/biomass across all 48 points, so
# part of the relationship could just be treatment (Spade treatments have lower
# yield AND lower NDVI). Each pre-specified test is refitted as
#   variable ~ idx + treat                  (within-treatment)
#   variable ~ idx + treat + zone_label     (within-treatment and within-zone)
# and the t-test on the idx coefficient is reported. partial_r is recovered
# from the t-statistic: r = t / sqrt(t^2 + df). Needs harvest_pts and
# harvest_correlations from Section 10, so run Script 7 from the top.

adjusted_row <- function(variable, source, metric, adjust_for) {
  df_pts <- harvest_pts %>%
    st_drop_geometry() %>%
    mutate(idx = .data[[paste0(tolower(metric), "_", source)]],
           y   = .data[[variable]])
  
  form <- if (adjust_for == "treatment") y ~ idx + treat else y ~ idx + treat + zone_label
  fit  <- lm(form, data = df_pts)
  co   <- summary(fit)$coefficients["idx", ]
  dof  <- fit$df.residual
  
  tibble(variable, source, metric, adjusted_for = adjust_for,
         n = nrow(df_pts), df = dof,
         partial_r = unname(co["t value"] / sqrt(co["t value"]^2 + dof)),
         p = unname(co["Pr(>|t|)"]))
}

harvest_adjusted <- tidyr::expand_grid(
  variable   = c("Grain_yield", "Biomass_maturity"),
  source     = c("satellite", "planet"),
  metric     = c("NDVI", "NDRE"),
  adjust_for = c("treatment", "treatment + zone")
) %>%
  purrr::pmap(adjusted_row) %>%
  bind_rows()

# Check the degrees of freedom: expect 39 (treatment) and 37 (treatment + zone)
harvest_adjusted %>% distinct(adjusted_for, df)

# ---- 12b. Side-by-side: raw vs adjusted (Bonferroni threshold 0.05/8 = 0.00625)
harvest_adjusted_compare <- harvest_correlations %>%
  filter(set == "pre-specified") %>%
  select(variable, source, metric, r_raw = r, p_raw = p) %>%
  left_join(
    harvest_adjusted %>% filter(adjusted_for == "treatment") %>%
      select(variable, source, metric, r_treat = partial_r, p_treat = p),
    by = c("variable", "source", "metric")) %>%
  left_join(
    harvest_adjusted %>% filter(adjusted_for == "treatment + zone") %>%
      select(variable, source, metric, r_treat_zone = partial_r, p_treat_zone = p),
    by = c("variable", "source", "metric")) %>%
  mutate(bonf_ok_treat      = p_treat < 0.00625,
         bonf_ok_treat_zone = p_treat_zone < 0.00625) %>%
  arrange(variable, source, metric)

print(harvest_adjusted_compare, n = Inf)

# ---- 12c. Save ---------------------------------------------------------------
write_csv(harvest_adjusted_compare,
          file.path(output_folder, paste0(site_name, "_harvest_correlations_adjusted_script7.csv")))
saveRDS(harvest_adjusted_compare,
        file.path(output_folder, paste0(site_name, "_harvest_correlations_adjusted_script7.rds")))

