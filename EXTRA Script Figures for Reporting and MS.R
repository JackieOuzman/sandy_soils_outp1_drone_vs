# =============================================================================
# Script: Figures for Reporting & MS
# -----------------------------------------------------------------------------
# Purpose : Standalone figures for the manuscript/report, built from already-
#           saved pipeline outputs rather than duplicating processing logic.
#           Kept separate from the numbered pipeline (Scripts 1-7) since these
#           are presentation figures, not data processing steps, and may need
#           reworking (e.g. combining across sites) without touching the
#           pipeline itself.
#
#           Figure 3.1 - Observation timeline, all four sources, one figure.
#           Combines Script 1's site_inventory (Field, Drone, Sentinel-2 -
#           all "used" since Sentinel-2 is pre-filtered for cloud upstream
#           of this pipeline - see caption) with Script 2b's planet_raster_qc
#           (Planet, split into used/excluded by the 30% masked-pixel rule -
#           the only source where that split is actually computable from our
#           own data, since Sentinel-2's cloud filtering happens in the QGIS
#           export step and isn't visible to us).
#
# Inputs  : {site_name}_site_inventory_script1.rds
#           {site_name}_planet_raster_qc_script2b.rds
#           metadata workbook, sheet "seasons" (sowing/harvest dates)
#
# Outputs : {site_name}_fig_observation_timeline_MS.png  (Figure 3.1)
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below.
# =============================================================================
library(dplyr)
library(readxl)
library(ggplot2)
library(stringr)   
library(sf)        
library(terra)     

# ============================== SITE CONFIG =================================
#site_name     <- "1.Walpeup_MRS125"
#site_name     <- "2.Crystal_Brook_Brians_House"
site_name     <- "3.Wynarka_Mervs_West"


base_path     <- "H:/Output-1"
metadata_path <- file.path(base_path, "0.Site-info",
                           "names of treatments per site 2025 metadata and other info.xlsx")
pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"
output_folder         <- file.path(pipeline_output_base, site_name)
# =============================================================================

# ---- 1. Load Script 1 and Script 2b outputs --------------------------------
site_inventory   <- readRDS(file.path(output_folder, paste0(site_name, "_site_inventory_script1.rds")))
planet_raster_qc <- readRDS(file.path(output_folder, paste0(site_name, "_planet_raster_qc_script2b.rds")))

season_2025 <- read_excel(metadata_path, sheet = "seasons") %>%
  filter(Site == site_name, Year == 2025)

sowing_date  <- as.Date(season_2025$`Sowing date`)
harvest_date <- as.Date(season_2025$`Harvest date`)


# ---- 2. Figure 3.1: combined observation timeline, all four sources -------
n_planet_total    <- nrow(planet_raster_qc)
n_planet_retained <- sum(!planet_raster_qc$excluded_cloud)

timeline_data <- bind_rows(
  site_inventory %>%
    filter(source %in% c("field", "drone"), !is.na(date)) %>%
    transmute(date, source),
  site_inventory %>%
    filter(source == "satellite") %>%
    transmute(date, source),
  planet_raster_qc %>%
    filter(!excluded_cloud) %>%
    transmute(date, source = "planet")
) %>%
  mutate(source = factor(source, levels = c("field", "drone", "planet", "satellite"),
                         labels = c("Field", "Drone", "Planet", "Sentinel-2")))

caption_text <- str_wrap(paste0(
  "Sentinel-2 dates were pre-filtered for cloud cover (<30%). Planet dates were ",
  "filtered in the R pipeline using a 30% masked-pixel threshold; ",
  n_planet_retained, " of ", n_planet_total, " Planet images were retained."
), width = 90)

# Labels nudged inward from their line (sowing forward in time, harvest back)
# so both sit clear of the line and away from the plot edges.
fig_timeline <- ggplot(timeline_data, aes(x = date, y = source, colour = source)) +
  geom_vline(xintercept = sowing_date, linetype = "dotted", colour = "darkgreen") +
  geom_vline(xintercept = harvest_date, linetype = "dotted", colour = "sienna") +
  geom_point(size = 4) +
  annotate("text", x = sowing_date, y = Inf, label = "Sowing",
           angle = 90, vjust = -0.5, hjust = 1.1, size = 4, colour = "darkgreen") +
  annotate("text", x = harvest_date, y = Inf, label = "Harvest",
           angle = 90, vjust = 1.3, hjust = 1.1, size = 4, colour = "sienna") +
  scale_x_date(expand = expansion(mult = c(0.02, 0.06))) +
  scale_colour_manual(values = c(
    "Field"      = "#9ECAE1",
    "Drone"      = "#4292C6",
    "Planet"     = "#084594",
    "Sentinel-2" = "#084594"
  )) +
  labs(x = NULL, y = NULL, colour = "Source") +   # title/caption go in Word, not the image
  theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        plot.caption = element_text(hjust = 0, size = 12, colour = "grey30"),
        plot.caption.position = "plot")

fig_timeline

ggsave(file.path(output_folder, paste0(site_name, "_fig_observation_timeline_MS.png")),
       fig_timeline, width = 9, height = 4.5, dpi = 300)

# ---- 3. Table 3.1: data summary per source ---------------------------------
# One row per source. Strip coverage was checked (every retained image covered
# 100% of the treatment strips once per-image resolution was used), so no
# masked-pixel column is reported - see DECISIONS_LOG 2b.2.
# Sentinel-2 images_captured = images exported AFTER the upstream QGIS cloud
# filter, so excluded_cloud is NA (not known). Drone has no cloud rule (NA).

table_3_1 <- site_inventory %>%
  filter(source %in% c("satellite", "planet", "drone")) %>%
  left_join(planet_raster_qc %>%
              transmute(date, source = "planet", excluded_cloud),
            by = c("source", "date")) %>%
  rowwise() %>%
  mutate(res_m = res(rast(file_path))[1]) %>%   # header only, fast
  ungroup() %>%
  
  mutate(is_excluded  = coalesce(excluded_cloud, FALSE),
         is_in_season = date >= sowing_date & date <= harvest_date) %>%
  group_by(source) %>%
  summarise(res_min_m       = min(res_m),
            res_max_m       = max(res_m),
            #images_captured = n(),
            in_season       = sum(is_in_season),
            excluded_cloud  = sum(is_in_season & is_excluded)) %>% 
           # used            = sum(is_in_season & !is_excluded)) %>%
  
  mutate(excluded_cloud = if_else(source == "planet", excluded_cloud, NA_integer_),
         source = factor(source, levels = c("drone", "planet", "satellite"),
                         labels = c("Drone", "Planet", "Sentinel-2"))) %>%
  arrange(source)

table_3_1

readr::write_csv(table_3_1,
                 file.path(output_folder, paste0(site_name, "_table_3_1_data_summary_MS.csv")))


# ---- 4. Figure 2: treatment effect over the season (Results 3.2) ----------
# Built from Script 5's saved ANOVA results (value ~ treat + zone, per date/
# source/index). Same approach as Script 5's plot, but no title/caption in
# the image (caption goes in Word), sowing-to-harvest window only.

anova_results <- readRDS(file.path(output_folder, paste0(site_name, "_anova_results_script5.rds")))

anova_plot_data <- anova_results %>%
  filter(date >= sowing_date, date <= harvest_date) %>%
  mutate(source = factor(source, levels = c("drone", "planet", "satellite"),
                         labels = c("Drone", "Planet", "Sentinel-2")))

fig_anova <- ggplot(anova_plot_data, aes(x = date, y = neg_log10_p, colour = source)) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = sowing_date, linetype = "dotted", colour = "darkgreen") +
  geom_vline(xintercept = harvest_date, linetype = "dotted", colour = "sienna") +
  geom_line(data = ~ filter(.x, source != "Drone"), alpha = 0.5) +   # no line for 2 drone dates
  geom_point(size = 2.5) +
  facet_wrap(~ metric, ncol = 1) +
  scale_colour_manual(values = c("Drone"      = "#E6550D",
                                 "Planet"     = "#084594",
                                 "Sentinel-2" = "#6BAED6")) +
  labs(x = NULL, y = expression(-log[10](italic(p))), colour = NULL) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom")

fig_anova

ggsave(file.path(output_folder, paste0(site_name, "_fig_anova_trend_MS.png")),
       fig_anova, width = 9, height = 6, dpi = 300)

# ---- 5. Table: treatment-effect summary (Results 3.2) ---------------------
# One row per source x index: dates tested (sowing to harvest), dates with a
# significant treatment effect (p < 0.05), and which dates were not
# significant. Uses the same filtered data as Figure 2 (anova_plot_data).
# Full date-by-date results = Script 5's anova_results_script5.csv
# (supplementary table).

table_3_2 <- anova_plot_data %>%
  group_by(source, metric) %>%
  summarise(dates_tested = n(),
            dates_sig    = sum(p.value < 0.05),
            nonsig_dates = paste(format(date[p.value >= 0.05], "%d %b"), collapse = ", "),
            .groups = "drop") %>%
  arrange(metric, source)

table_3_2

readr::write_csv(table_3_2,
                 file.path(output_folder, paste0(site_name, "_table_3_2_anova_summary_MS.csv")))



# Supplementary Table S1: full date-by-date ANOVA results, sowing to harvest
# (same filtered data as Figure 2 / Table 4). Treatment df = 7 and residual
# df = 14 on every date, so they go in the caption rather than as columns.
table_s1 <- anova_plot_data %>%
  transmute(Date   = date,
            Source = source,
            Index  = metric,
            F      = round(statistic, 2),
            p      = signif(p.value, 3)) %>%
  arrange(desc(Index), Source, Date)   # NDVI first, then NDRE

table_s1

readr::write_csv(table_s1,
                 file.path(output_folder, paste0(site_name, "_table_S1_anova_full_MS.csv")))


######### STOP ##################################################################

################################################################################
### Below this is site 1 specific analysis / reporting #########################


# ---- 6. Table: Spade vs non-Spade NDVI by zone (Results 3.3) --------------
# Both drone flight dates, plus the nearest Planet and Sentinel-2 image to
# each flight. Spade group = treatment codes containing "S" (S, SR, SRL, SL)
# - WALPEUP-SPECIFIC, check treatment codes before reusing at other sites.
# gap = non-Spade minus Spade (positive = Spade strips lower NDVI).

strip_zone_zonal_stats  <- readRDS(file.path(output_folder, paste0(site_name, "_strip_zone_zonal_stats_script3.rds")))
planet_strip_zone_zonal <- readRDS(file.path(output_folder, paste0(site_name, "_planet_strip_zone_zonal_stats_script3b.rds")))

all_stripzone <- bind_rows(
  strip_zone_zonal_stats  %>% select(date, source, treat, zone_label, mean.NDVI),
  planet_strip_zone_zonal %>% select(date, source, treat, zone_label, mean.NDVI)
)

# 6a. Match each source's nearest image date to each drone flight
drone_dates <- sort(unique(all_stripzone$date[all_stripzone$source == "drone"]))

nearest_to <- function(src, d) {
  src_dates <- unique(all_stripzone$date[all_stripzone$source == src])
  src_dates[which.min(abs(src_dates - d))]
}

matched_dates <- tidyr::expand_grid(flight = drone_dates,
                                    source = c("drone", "planet", "satellite")) %>%
  rowwise() %>%
  mutate(date = nearest_to(source, flight)) %>%
  ungroup()

matched_dates   # check: 6 rows, drone date = flight date

# 6b. Spade vs non-Spade mean NDVI per zone, on the matched dates (full version, saved)
table_3_3 <- all_stripzone %>%
  inner_join(matched_dates, by = c("source", "date")) %>%
  mutate(group = if_else(grepl("S", treat), "Spade", "NoSpade")) %>%
  group_by(flight, source, date, zone_label, group) %>%
  summarise(ndvi = round(mean(mean.NDVI), 3), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = group, values_from = ndvi) %>%
  mutate(gap    = round(NoSpade - Spade, 3),
         period = if_else(flight == min(flight), "Early season", "Later season"),
         source = factor(source, levels = c("drone", "planet", "satellite"),
                         labels = c("Drone", "Planet", "Sentinel-2"))) %>%
  select(period, source, date, zone_label, NoSpade, Spade, gap) %>%
  arrange(period, source, zone_label)

print(table_3_3, n = Inf)

readr::write_csv(table_3_3,
                 file.path(output_folder, paste0(site_name, "_table_3_3_spade_gap_by_zone_MS.csv")))

# 6c. Compact view for Word: gap only, one column per source (saved)
table_3_3_compact <- table_3_3 %>%
  select(period, zone_label, source, gap) %>%
  tidyr::pivot_wider(names_from = source, values_from = gap)

table_3_3_compact

readr::write_csv(table_3_3_compact,
                 file.path(output_folder, paste0(site_name, "_table_3_3_spade_gap_compact_MS.csv")))


# ---- 7. Figure 3: NDVI difference by zone and source, with F (Results 3.4) -
# Shows why the drone's larger differences did not give a stronger test: its
# differences vary more between zones. Uses table_3_3 (Section 6),
# matched_dates (Section 6) and anova_results (Section 4).

# 7a. F-statistic for each source on its matched date (treatment ANOVA, NDVI)
f_labels <- matched_dates %>%
  inner_join(anova_results %>%
               filter(metric == "NDVI") %>%
               select(source, date, statistic),
             by = c("source", "date")) %>%
  mutate(period = if_else(flight == min(flight), "Early season", "Later season"),
         source = factor(source, levels = c("drone", "planet", "satellite"),
                         labels = c("Drone", "Planet", "Sentinel-2")),
         label  = paste0("F = ", round(statistic, 1)))

f_labels   # check: 6 rows, F values match the 3.4 text

# 7b. Plot
fig_resolution <- ggplot(table_3_3, aes(x = source, y = gap, colour = zone_label)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_point(size = 4, position = position_dodge(width = 0.4)) +
  geom_text(data = f_labels, aes(x = source, y = Inf, label = label),
            inherit.aes = FALSE, vjust = 1.5, size = 4.5) +
  facet_wrap(~ period) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +   # room for F labels
  scale_colour_manual(values = c("Dune"       = "#E6AB02",
                                 "Swale"      = "#1B9E77",
                                 "Transition" = "#7570B3")) +
  labs(x = NULL, y = "NDVI difference\n(non-Spade − Spade)", colour = "Zone") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom")

fig_resolution

ggsave(file.path(output_folder, paste0(site_name, "_fig_resolution_by_zone_MS.png")),
       fig_resolution, width = 10, height = 5.5, dpi = 300)



################################################################################
#### is can be used for multiple sites ########################################

# ---- 8. Figure 4: field measurements vs NDVI, Planet and Sentinel-2 (Results 3.5)
# One panel per field variable x source, points coloured Spade vs non-Spade.
# Built from Script 7's saved point-level tables (1 m buffered NDVI at each
# sample point, nearest image date to each sampling). Drone excluded - see
# Script 7 header.

requireNamespace("patchwork")

gt_est  <- readRDS(file.path(output_folder, paste0(site_name, "_establishment_groundtruth_pointlevel_script7.rds"))) %>% sf::st_drop_geometry()
gt_bio  <- readRDS(file.path(output_folder, paste0(site_name, "_biomass_groundtruth_pointlevel_script7.rds")))       %>% sf::st_drop_geometry()
gt_harv <- readRDS(file.path(output_folder, paste0(site_name, "_harvest_groundtruth_pointlevel_script7.rds")))       %>% sf::st_drop_geometry()

var_levels <- c("Establishment (plants/m²)", "Flowering biomass (kg/ha)", "Grain yield (kg/ha)")

gt_long <- bind_rows(
  gt_est  %>% transmute(treat, treatment_name, variable = var_levels[1], value = establishment_plants_m2,
                        Planet = ndvi_planet, `Sentinel-2` = ndvi_satellite, Drone = ndvi_drone),
  gt_bio  %>% transmute(treat, treatment_name, variable = var_levels[2], value = Biomass_flowering,
                        Planet = ndvi_planet, `Sentinel-2` = ndvi_satellite, Drone = ndvi_drone),
  gt_harv %>% transmute(treat, treatment_name, variable = var_levels[3], value = Grain_yield,
                        Planet = ndvi_planet, `Sentinel-2` = ndvi_satellite)   # no drone for harvest
) %>%
  tidyr::pivot_longer(c(Planet, `Sentinel-2`, Drone), names_to = "source", values_to = "ndvi") %>%
  filter(!is.na(ndvi)) %>%   # drops Drone wherever no flight within 7 days (all of Walpeup)
  mutate(group    = if (site_name == "1.Walpeup_MRS125") {
    if_else(grepl("S", treat), "Spade", "Non-Spade")    # Walpeup factorial (log 5.6)
  } else treatment_name,                                 # other sites: each treatment
  source   = factor(source, levels = c("Planet", "Sentinel-2", "Drone")),
  variable = factor(variable, levels = var_levels))

# r per panel (should match Script 7's saved correlations)
r_labels <- gt_long %>%
  group_by(variable, source) %>%
  summarise(r = cor(ndvi, value, use = "complete.obs"), .groups = "drop") %>%
  mutate(label = paste0("r = ", sprintf("%.2f", r)))

r_labels   # check: 6 rows

# One-line panel title with r built in, e.g. "Grain yield (kg/ha) — Planet (r = 0.51)"

# Panel titles: source + r only - variable name and units go on the y-axis
gt_long <- gt_long %>%
  left_join(r_labels %>% select(variable, source, label), by = c("variable", "source")) %>%
  mutate(panel = paste0(c("Establishment", "Flowering biomass", "Grain yield")[as.integer(variable)],
                        " — ", source, " (", label, ")"),   # short name, no units
         panel = factor(panel, levels = unique(panel[order(variable, source)])))
y_titles <- c("plants/m²", "kg/ha", "kg/ha")

# One row per field variable, with its own y-axis title; Planet and
# Sentinel-2 side by side, each with its own NDVI range
# Point colours: Walpeup = Spade vs non-Spade; other sites = each treatment,
# using its colour from the metadata "treatment names" sheet
group_colours <- if (site_name == "1.Walpeup_MRS125") {
  c("Non-Spade" = "#6BAED6", "Spade" = "#E6550D")
} else {
  tn <- read_excel(metadata_path, sheet = "treatment names") %>% filter(Site == site_name)
  setNames(tn$Hex, tn$`Shorthand Name`)
}

make_gt_row <- function(i) {
  ggplot(filter(gt_long, variable == var_levels[i]), aes(x = ndvi, y = value)) +
    geom_smooth(method = "lm", se = TRUE, colour = "grey30", linewidth = 0.6) +
    geom_point(aes(colour = group), size = 2.5, alpha = 0.8) +
    facet_wrap(~ panel, scales = "free_x", nrow = 1) +   # one row per variable (3 panels if drone)
    scale_colour_manual(values = group_colours) +
    labs(x = if (i == 3) "NDVI" else NULL, y = y_titles[i], colour = NULL) +
    theme_minimal(base_size = 14)
}

# Stack the three rows, one shared legend at the bottom
fig_groundtruth <- patchwork::wrap_plots(lapply(1:3, make_gt_row), ncol = 1) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

fig_groundtruth

ggsave(file.path(output_folder, paste0(site_name, "_fig_groundtruth_vs_ndvi_MS.png")),
       fig_groundtruth, width = 9, height = 10, dpi = 300)



# ---- 9. Figure: treatment means by source at each drone date, with F (Results 3.4)
# Works at any site. For each drone flight: treatment mean NDVI (strip means)
# from the drone and from the nearest Planet and Sentinel-2 image (Script 4
# matches), with each source's treatment F-statistic on its date (Script 5,
# strip x zone ANOVA). Wider spread of points = larger treatment differences.

m_sat <- readRDS(file.path(output_folder, paste0(site_name, "_treatment_NDVI_matched_drone_satellite_script4.rds")))
m_pla <- readRDS(file.path(output_folder, paste0(site_name, "_treatment_NDVI_matched_drone_planet_script4.rds")))

res_means <- bind_rows(
  m_sat %>% transmute(date_drone, treatment_name, source = "Drone",      date = date_drone,     ndvi = mean_drone),
  m_sat %>% transmute(date_drone, treatment_name, source = "Sentinel-2", date = date_satellite, ndvi = mean_satellite),
  m_pla %>% transmute(date_drone, treatment_name, source = "Planet",     date = date_planet,    ndvi = mean_planet)
) %>%
  mutate(flight = paste("Drone flight", format(date_drone, "%d %b")))

# F-statistic for each source on its matched date (NDVI, from Section 4's anova_results)
res_F <- res_means %>%
  distinct(flight, source, date) %>%
  left_join(anova_results %>%
              filter(metric == "NDVI") %>%
              mutate(source = recode(source, drone = "Drone", planet = "Planet", satellite = "Sentinel-2")) %>%
              select(source, date, statistic),
            by = c("source", "date")) %>%
  mutate(label = paste0("F = ", sprintf("%.1f", statistic)))   # always one decimal (23.0, 9.0)

res_F   # check: one row per flight x source, with F values

src_levels <- c("Drone", "Planet", "Sentinel-2")
res_means  <- res_means %>% mutate(source = factor(source, levels = src_levels))
res_F      <- res_F     %>% mutate(source = factor(source, levels = src_levels))

# Treatment colours from the metadata "treatment names" sheet
tn_all      <- read_excel(metadata_path, sheet = "treatment names") %>% filter(Site == site_name)
trt_colours <- setNames(tn_all$Hex, tn_all$`Shorthand Name`)

fig_res_means <- ggplot(res_means, aes(x = source, y = ndvi, colour = treatment_name)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.6)) +
  geom_text(data = res_F, aes(x = source, y = Inf, label = label),
            inherit.aes = FALSE, vjust = 1.5, size = 4.5) +
  facet_wrap(~ flight) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +   # room for F labels
  scale_colour_manual(values = trt_colours) +
  labs(x = NULL, y = "Treatment mean NDVI", colour = NULL) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom")

fig_res_means

ggsave(file.path(output_folder, paste0(site_name, "_fig_treatment_means_by_source_MS.png")),
       fig_res_means, width = 9, height = 6, dpi = 300)
