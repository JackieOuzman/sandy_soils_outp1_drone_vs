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

# ============================== SITE CONFIG =================================
site_name     <- "1.Walpeup_MRS125"
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
  labs(title = paste("Observation timeline —", site_name),
       x = NULL, y = NULL, colour = "Source",
       caption = caption_text) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        plot.caption = element_text(hjust = 0, size = 12, colour = "grey30"),
        plot.caption.position = "plot")

fig_timeline

ggsave(file.path(output_folder, paste0(site_name, "_fig_observation_timeline_MS.png")),
       fig_timeline, width = 9, height = 4.5, dpi = 300)
