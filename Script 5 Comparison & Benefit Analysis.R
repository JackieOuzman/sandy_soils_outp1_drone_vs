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
#site_name     <- "1.Walpeup_MRS125"
#site_name     <- "2.Crystal_Brook_Brians_House"
#site_name     <- "3.Wynarka_Mervs_West"
site_name     <- "4.Wharminda_Woodys"


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

#site_name     <- "1.Walpeup_MRS125"
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
anova_plot

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


# ---- 5. Factorial ANOVA: Spade + Rip + Lime + zone -------------------------
# Walpeup's 8 treatments are a 2 x 2 x 2 factorial (Spade, Rip, Lime), so the
# treatment effect can be split into three main effects. Used to justify the
# Spade vs non-Spade grouping in Results 3.3/3.4 (see DECISIONS_LOG).
# WALPEUP-SPECIFIC: factors are coded from the treatment letters. Other sites
# are not factorials - check the design before reusing (Script 8).

factorial_results <- long_data %>%
  mutate(spade = grepl("S", treat),     # S, SL, SR, SRL
         rip   = grepl("R", treat),     # R, RL, SR, SRL
         lime  = grepl("L", treat)) %>% # L, RL, SL, SRL
  group_by(date, source, metric) %>%
  group_modify(~ broom::tidy(aov(value ~ spade + rip + lime + zone_label, data = .x))) %>%
  ungroup() %>%
  filter(term %in% c("spade", "rip", "lime")) %>%
  select(date, source, metric, term, df, statistic, p.value)

# Summary: in-season dates where each factor was significant (p < 0.05)
factorial_results %>%
  filter(date >= sowing_date, date <= harvest_date) %>%
  group_by(metric, source, term) %>%
  summarise(n_dates  = n(),
            n_sig    = sum(p.value < 0.05),
            median_F = round(median(statistic), 1),
            .groups  = "drop") %>%
  arrange(metric, source, desc(median_F)) %>%
  print(n = Inf)

write_csv(factorial_results,
          file.path(output_folder, paste0(site_name, "_factorial_anova_script5.csv")))
saveRDS(factorial_results,
        file.path(output_folder, paste0(site_name, "_factorial_anova_script5.rds")))


# ---- 6. Does accounting for zone change the conclusions? -------------------
# Same treatment test as Section 3, fitted WITHOUT zone (value ~ treat) and
# WITH zone (value ~ treat + zone). If zone matters, the treatment result
# changes between the two. Also records the zone effect itself (from the
# with-zone model). Answers research question 2.

zone_compare <- long_data %>%
  group_by(date, source, metric) %>%
  group_modify(~ {
    no_zone   <- broom::tidy(aov(value ~ treat, data = .x))
    with_zone <- broom::tidy(aov(value ~ treat + zone_label, data = .x))
    tibble(
      F_treat_nozone   = no_zone$statistic[no_zone$term == "treat"],
      p_treat_nozone   = no_zone$p.value[no_zone$term == "treat"],
      F_treat_withzone = with_zone$statistic[with_zone$term == "treat"],
      p_treat_withzone = with_zone$p.value[with_zone$term == "treat"],
      F_zone           = with_zone$statistic[with_zone$term == "zone_label"],
      p_zone           = with_zone$p.value[with_zone$term == "zone_label"]
    )
  }) %>%
  ungroup()

# Summary: in-season dates, per index and source
zone_compare %>%
  filter(date >= sowing_date, date <= harvest_date) %>%
  group_by(metric, source) %>%
  summarise(n_dates           = n(),
            sig_nozone        = sum(p_treat_nozone < 0.05),
            sig_withzone      = sum(p_treat_withzone < 0.05),
            zone_sig          = sum(p_zone < 0.05),
            median_F_nozone   = round(median(F_treat_nozone), 1),
            median_F_withzone = round(median(F_treat_withzone), 1),
            median_F_zone     = round(median(F_zone), 1),
            .groups = "drop") %>%
  arrange(metric, source) %>%
  print(n = Inf)

write_csv(zone_compare,
          file.path(output_folder, paste0(site_name, "_zone_compare_script5.csv")))
saveRDS(zone_compare,
        file.path(output_folder, paste0(site_name, "_zone_compare_script5.rds")))

