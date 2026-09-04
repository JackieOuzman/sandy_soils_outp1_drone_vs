# =============================================================================
# Script 5: Comparison & Benefit Analysis
# -----------------------------------------------------------------------------
# Purpose : Job A - test whether treatment differences in NDVI AND NDRE are
#           statistically real, accounting for zone (soil type) as a
#           blocking factor - the trial design is a randomized block design
#           (8 treatments x 3 zones, one strip-zone piece per combination),
#           analysed with the classic additive ANOVA for this design
#           (treatment + zone, residual = the treatment x zone interaction,
#           standard when there's no true replication within each cell).
#           Run separately per date/source/metric. Drone has no NDRE (see
#           Script 2/3) - those combinations are dropped before the ANOVA
#           runs (no data to test), rather than causing an error.
#           Job B (relating NDVI/NDRE to field ground truth) comes after -
#           needs field data extracted from Excel first, not yet done.
#
# Inputs  : {site_name}_strip_zone_zonal_stats_script3.rds       (sat+drone, NDVI+NDRE)
#           {site_name}_planet_strip_zone_zonal_stats_script3b.rds (planet, NDVI+NDRE)
#           metadata workbook, sheet "seasons" (sowing/harvest dates for the plot)
#
# Outputs : {site_name}_anova_results_script5.csv/.rds
#           (one row per date/source/metric: treatment effect F-stat, p-value,
#           neg_log10_p)
#           {site_name}_anova_trend_script5.png
#           (faceted by metric: NDVI, NDRE)
#           {site_name}_drone_satellite_fstat_summary_script5.csv/.rds
#           (NDVI only - drone has no matching NDRE data)
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below.
# =============================================================================

library(dplyr)
library(readr)
library(tidyr)

# ============================== SITE CONFIG =================================
site_name     <- "1.Walpeup_MRS125"
pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"
output_folder         <- file.path(pipeline_output_base, site_name)
# =============================================================================



# ---- 1. Load Script 3/3b's raw strip x zone extraction (all 3 sources) ----
strip_zone_zonal_stats  <- readRDS(file.path(output_folder, paste0(site_name, "_strip_zone_zonal_stats_script3.rds")))
planet_strip_zone_zonal <- readRDS(file.path(output_folder, paste0(site_name, "_planet_strip_zone_zonal_stats_script3b.rds")))

# ---- 2. Combine into one long table: one row per plot x zone x source x date
# x metric (NDVI/NDRE). Drone has no NDRE (all NA) - those rows are dropped
# before the ANOVA runs, so drone/NDRE simply doesn't appear in the results
# rather than causing an error.


long_data <- bind_rows(
  strip_zone_zonal_stats  %>% select(plot, treat, zone_code, zone_label, date, source, mean.NDVI, mean.NDRE),
  planet_strip_zone_zonal %>% select(plot, treat, zone_code, zone_label, date, source, mean.NDVI, mean.NDRE)
) %>%
  pivot_longer(cols = c(mean.NDVI, mean.NDRE), names_to = "metric", values_to = "value") %>%
  mutate(metric = sub("mean\\.", "", metric)) %>%
  filter(!is.na(value))

long_data



# ---- 3. Fit the block ANOVA (NDVI ~ treatment + zone) for each date/source -
# This tests: after accounting for zone, is there still a real treatment
# effect? Run separately per date/source combination (4 total: 2 drone dates
# x drone+matched-satellite).

run_anova <- function(df) {
  model <- aov(value ~ treat + zone_label, data = df)
  broom::tidy(model) %>% filter(term == "treat")
}

anova_results <- long_data %>%
  group_by(date, source, metric) %>%
  group_modify(~ run_anova(.x)) %>%
  ungroup() %>%
  select(date, source, metric, df, statistic, p.value) %>%
  mutate(neg_log10_p = -log10(p.value))

anova_results


# ---- 4. Save Job A output ---------------------------------------------------
write_csv(anova_results, file.path(output_folder, paste0(site_name, "_anova_results_script5.csv")))
saveRDS(anova_results,  file.path(output_folder, paste0(site_name, "_anova_results_script5.rds")))

# ---- Check the "seasons" sheet for sowing date -----------------------------
library(dplyr)
library(readxl)

site_name     <- "1.Walpeup_MRS125"
base_path     <- "H:/Output-1"
metadata_path <- file.path(base_path, "0.Site-info",
                           "names of treatments per site 2025 metadata and other info.xlsx")

seasons <- read_excel(metadata_path, sheet = "seasons") %>%
  filter(Site == site_name)




# ---- Visualise ANOVA results across the season -----------------------------
library(ggplot2)

# ---- Pull season details for the plot --------------------------------------
season_2025 <- seasons %>% filter(Year == 2025)

sowing_date  <- as.Date(season_2025$`Sowing date`)
harvest_date <- as.Date(season_2025$`Harvest date`)
crop         <- season_2025$Crop
variety      <- season_2025$Variety

plot_data <- anova_results %>% filter(date >= sowing_date)

# ---- Save the ANOVA trend plot ----------------------------------------------
# Faceted by metric (NDVI/NDRE) since drone has no NDRE data and the two
# indices' significance patterns shouldn't be visually conflated on one panel.
anova_plot <- ggplot(plot_data, aes(x = date, y = neg_log10_p, colour = source)) +
  geom_line(data = plot_data %>% filter(source == "planet"), alpha = 0.4) +
  geom_line(data = plot_data %>% filter(source == "satellite"), alpha = 0.4) +
  geom_point(size = 2.5) +
  facet_wrap(~metric, ncol = 1) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = sowing_date, linetype = "dotted", colour = "darkgreen") +
  geom_vline(xintercept = harvest_date, linetype = "dotted", colour = "sienna") +
  annotate("text", x = sowing_date, y = max(plot_data$neg_log10_p) * 0.95,
           label = "Sowing", angle = 90, vjust = -0.5, hjust = 1, size = 3, colour = "darkgreen") +
  annotate("text", x = harvest_date, y = max(plot_data$neg_log10_p) * 0.95,
           label = "Harvest", angle = 90, vjust = -0.5, hjust = 1, size = 3, colour = "sienna") +
  annotate("text", x = min(plot_data$date), y = -log10(0.05) + 0.15,
           label = "p = 0.05 threshold", hjust = 0, size = 3, colour = "grey40") +
  labs(
    title = paste("Treatment effect strength over the season —", site_name),
    subtitle = paste0("Higher = stronger, more significant treatment effect\n",
                      crop, " (", variety, "), sown ", format(sowing_date, "%d %b %Y")),
    x = NULL, y = "-log10(p-value)",
    caption = "Treatment effect tested with a randomized block ANOVA (NDVI ~ treatment + zone),\nzone (soil type) included as a blocking factor. Run per date/source on strip x zone NDVI (Script 5).\nDashed line = p = 0.05 significance threshold. Drone points reflect only 2 available flight dates."
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 8, colour = "grey30"))

ggsave(file.path(output_folder, paste0(site_name, "_anova_trend_script5.png")),
       anova_plot, width = 10, height = 6, dpi = 300)

# ---- Build the drone vs nearest-satellite F-statistic summary table -------
drone_dates <- anova_results %>% filter(source == "drone") %>% pull(date)

drone_satellite_summary <- anova_results %>%
  filter(source %in% c("drone", "satellite")) %>%
  rowwise() %>%
  filter(source == "drone" | date %in% (drone_dates[which.min(abs(drone_dates - date))])) %>%
  ungroup() %>%
  filter(source == "drone" | sapply(date, function(d) any(abs(d - drone_dates) <= 3))) %>%
  select(date, source, metric, statistic, p.value) %>%
  arrange(metric, date, desc(source == "drone"))

drone_satellite_summary

write_csv(drone_satellite_summary,
          file.path(output_folder, paste0(site_name, "_drone_satellite_fstat_summary_script5.csv")))
saveRDS(drone_satellite_summary,
        file.path(output_folder, paste0(site_name, "_drone_satellite_fstat_summary_script5.rds")))
