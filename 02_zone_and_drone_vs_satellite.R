# =============================================================================
# 02_zone_and_drone_vs_satellite.R
# -----------------------------------------------------------------------------
# PURPOSE   Cross-site tables and figures for
#             MS 3.3  Soil zone effects        -> Table 8 (with vs without zone),
#                                                 Table 9 (Walpeup Spade gap by zone)
#             MS 3.4  Drone vs satellite       -> Table 10 (F and spread on drone
#                                                 dates), Figure 4 + Suppl. S4-S7
#           plus text checks used in 3.3 and 3.4 (section 7).
#
# INPUTS    Per site, in <pipeline_output_base>/<site_id>/ :
#             <site_id>_zone_compare_script5.csv                (F, p with/without zone)
#             <site_id>_table_S1_anova_full_MS.csv              (used only for a check)
#             <site_id>_stripzone_NDVI_matched_drone_{satellite,planet}_script4.csv
#             1.Walpeup_MRS125_table_3_3_spade_gap_by_zone_MS.csv   (Table 9)
#
# OUTPUTS   Table_8_zone_effect_all_sites_MS.csv
#           Table_10_drone_vs_satellite_MS.csv
#           Fig_dvs_MervsWest_MS.png                (main text, Figure 4)
#           Fig_S_dvs_{BriansHouse,Randals,Woodys,Gums}.png  (supplementary)
#
# RERUN     After the per-site pipeline is re-run (Future strips dropped, full
#           Planet / Sentinel-2 ranges, confirmed harvest dates), update the
#           dates in section 2 and run top to bottom. Row counts in section 3
#           and the text checks in section 7 will change; update the MS text.
#
# AUTHOR    Jackie Ouzman        LAST UPDATED  2026-10-06
# =============================================================================

# ==== 1. SETUP ===============================================================
library(tidyverse)
library(lubridate)
library(patchwork)

pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"
out <- function(f) file.path(pipeline_output_base, f)

# ==== 2. SITE METADATA =======================================================
site_levels <- c("Walpeup MRS125", "Brians House", "Mervs West",
                 "Woodys", "Gums", "Randals")

site_meta <- tribble(
  ~site_id,                        ~site,            ~sowing,       ~harvest,
  "1.Walpeup_MRS125",              "Walpeup MRS125", "2025-04-27",  "2025-12-07",
  "2.Crystal_Brook_Brians_House",  "Brians House",   "2025-05-29",  "2025-11-26",
  "3.Wynarka_Mervs_West",          "Mervs West",     "2025-06-03",  "2025-12-30",
  "4.Wharminda_Woodys",            "Woodys",         "2025-06-04",  "2025-12-06",
  "5.Walpeup_Gums",                "Gums",           "2025-05-01",  "2025-11-10",
  "6.Crystal_Brook_Randals",       "Randals",        "2025-06-05",  "2025-12-05"
) |>
  mutate(across(c(sowing, harvest), ymd),
         site = factor(site, levels = site_levels))

# ==== 3. LOAD ZONE-COMPARE RESULTS (sowing to harvest only) ==================
zc_files <- file.path(pipeline_output_base, site_meta$site_id,
                      paste0(site_meta$site_id, "_zone_compare_script5.csv"))

zone_cmp <- map_dfr(zc_files, \(f) read_csv(f, show_col_types = FALSE) |>
                      mutate(site_id = basename(dirname(f)))) |>
  mutate(date = as.Date(date)) |>
  left_join(site_meta, by = "site_id") |>
  filter(date >= sowing, date <= harvest) |>
  mutate(source = recode(source, planet = "Planet",
                         satellite = "Sentinel-2", drone = "Drone"),
         zone_sig = p_zone < 0.05,
         t_nozone = p_treat_nozone < 0.05,
         t_zone   = p_treat_withzone < 0.05)

# Check 1: row counts = dates tested x sources x indices.
# Current values: Walpeup 158, Brians 59, Mervs 83, Woodys 122, Gums 150,
# Randals 57 (total 629). These change after the rerun.
zone_cmp |> count(site)

# Check 2: with-zone treatment p-values must equal the ANOVA table's p-values
anova_chk <- map_dfr(site_meta$site_id, \(sid)
                     read_csv(file.path(pipeline_output_base, sid,
                                        paste0(sid, "_table_S1_anova_full_MS.csv")),
                              show_col_types = FALSE) |> mutate(site_id = sid)) |>
  transmute(site_id, date = as.Date(Date), source = Source,
            metric = Index, p_anova = p)

chk <- zone_cmp |>
  mutate(source = as.character(source)) |>
  inner_join(anova_chk, by = c("site_id", "source", "metric", "date"))
cat("rows matched:", nrow(chk), "of", nrow(zone_cmp),
    "| max |p difference|:", max(abs(chk$p_anova - chk$p_treat_withzone)), "\n")

# ==== 4. TABLE 8: WITH vs WITHOUT ZONE, ALL SITES ============================
# Date counts are NDVI; median F is the range of medians across Planet and
# Sentinel-2 and across NDVI and NDRE.
fmt_rng <- \(x) paste(unique(signif(range(x), 2)), collapse = "–")

counts <- zone_cmp |>
  filter(metric == "NDVI", source != "Drone") |>
  group_by(site, source) |>
  summarise(N = n(), zone = sum(zone_sig),
            nz = sum(t_nozone), wz = sum(t_zone), .groups = "drop") |>
  mutate(zone_txt  = paste0(zone, "/", N),
         treat_txt = paste0(nz, " → ", wz, " of ", N)) |>
  select(site, source, zone_txt, treat_txt) |>
  pivot_wider(names_from = source, values_from = c(zone_txt, treat_txt))

medians <- zone_cmp |>
  filter(source != "Drone") |>
  group_by(site, source, metric) |>
  summarise(zF = median(F_zone), nzF = median(F_treat_nozone),
            wzF = median(F_treat_withzone), .groups = "drop") |>
  group_by(site) |>
  summarise(zone_F  = fmt_rng(zF),
            treat_F = paste0(fmt_rng(nzF), " → ", fmt_rng(wzF)))

tab8 <- left_join(counts, medians, by = "site") |>
  transmute(Site = site,
            `Zone significant: Planet`     = zone_txt_Planet,
            `Zone significant: Sentinel-2` = `zone_txt_Sentinel-2`,
            `Median zone F`                = zone_F,
            `Treatment significant, Planet (without → with zone)`     = treat_txt_Planet,
            `Treatment significant, Sentinel-2 (without → with zone)` = `treat_txt_Sentinel-2`,
            `Median treatment F (without → with zone)`                = treat_F)

print(tab8, n = Inf, width = Inf)
write_csv(tab8, out("Table_8_zone_effect_all_sites_MS.csv"))

# ==== 5. TABLE 9: WALPEUP SPADE vs NON-SPADE GAP BY ZONE =====================
# Already built by the Walpeup pipeline; read for checking against the MS table
tab9 <- read_csv(out("1.Walpeup_MRS125/1.Walpeup_MRS125_table_3_3_spade_gap_by_zone_MS.csv"),
                 show_col_types = FALSE)
print(tab9, n = Inf, width = Inf)

# ==== 6. TABLE 10: DRONE vs SATELLITE ON MATCHED DATES =======================
read_matched <- function(site_id, other) {          # other = "satellite" | "planet"
  f <- file.path(pipeline_output_base, site_id,
                 paste0(site_id, "_stripzone_NDVI_matched_drone_", other, "_script4.csv"))
  read_csv(f, show_col_types = FALSE) |> mutate(site_id = site_id)
}

sat <- map_dfr(site_meta$site_id, read_matched, other = "satellite")
pl  <- map_dfr(site_meta$site_id, read_matched, other = "planet")

# strip x zone means for the drone and its matched Sentinel-2 / Planet images
pieces <- bind_rows(
  sat |> transmute(site_id, drone_date = date_drone, treatment_name,
                   source = "Drone", date = date_drone,
                   gap = 0, mean = mean_drone, count = count_drone),
  sat |> transmute(site_id, drone_date = date_drone, treatment_name,
                   source = "Sentinel-2", date = date_satellite,
                   gap = day_gap, mean = mean_satellite, count = count_satellite),
  pl  |> transmute(site_id, drone_date = date_drone, treatment_name,
                   source = "Planet", date = date_planet,
                   gap = day_gap, mean = mean_planet, count = count_planet)
)

# treatment (strip) means: pixel-count-weighted across the zones each strip crosses
trt_means <- pieces |>
  group_by(site_id, drone_date, source, date, gap, treatment_name) |>
  summarise(ndvi = weighted.mean(mean, count, na.rm = TRUE), .groups = "drop")

# spread of treatment means = max minus min strip-mean NDVI
spread <- trt_means |>
  group_by(site_id, drone_date, source, date, gap) |>
  summarise(min_ndvi = min(ndvi), max_ndvi = max(ndvi),
            range_ndvi = max_ndvi - min_ndvi, .groups = "drop")

# F and p for each source on its own date (NDVI, zone-blocked model)
fstats <- zone_cmp |>
  filter(metric == "NDVI") |>
  select(site_id, source, date, F = F_treat_withzone, p = p_treat_withzone)

tab10 <- spread |>
  left_join(fstats, by = c("site_id", "source", "date")) |>
  left_join(site_meta |> select(site_id, site), by = "site_id") |>
  mutate(source = factor(source, levels = c("Drone", "Planet", "Sentinel-2"))) |>
  arrange(site, drone_date, source) |>
  transmute(Site = site, `Drone date` = drone_date, Source = source,
            `Image date` = date, `Gap (days)` = gap,
            F = round(F, 1), p = signif(p, 2),
            `Min NDVI` = round(min_ndvi, 3), `Max NDVI` = round(max_ndvi, 3),
            Range = round(range_ndvi, 3))

print(tab10, n = Inf, width = Inf)
write_csv(tab10, out("Table_10_drone_vs_satellite_MS.csv"))

# ==== 7. CHECKS BEHIND THE MS TEXT (3.3, 3.4) ================================
# (a) 3.3: NDRE counts, same layout as Table 8 ("NDRE showed the same pattern")
zone_cmp |>
  filter(metric == "NDRE", source != "Drone") |>
  group_by(site, source) |>
  summarise(N = n(), zone_sig = sum(zone_sig),
            treat_nozone = sum(t_nozone), treat_withzone = sum(t_zone),
            .groups = "drop") |>
  arrange(site, source) |>
  print(n = Inf)

# (b) 3.3: Randals dates significant with or without zone
zone_cmp |>
  filter(site == "Randals", t_nozone | t_zone) |>
  select(date, source, metric, p_treat_nozone, p_treat_withzone) |>
  print(n = Inf)

# (c) 3.4: Gums treatment ranking, 26 Jun vs 1 Oct (crossover)
gums <- trt_means |>
  filter(site_id == "5.Walpeup_Gums") |>
  mutate(group = case_when(str_detect(treatment_name, "Bednar")  ~ "Bednar",
                           str_detect(treatment_name, "Control") ~ "Control",
                           str_detect(treatment_name, "Rip")     ~ "Rip",
                           TRUE                                  ~ "Horsch"))

gums |>                                    # treatment order, highest NDVI first
  arrange(drone_date, source, desc(ndvi)) |>
  group_by(drone_date, source) |>
  summarise(order = paste(treatment_name, collapse = " > "), .groups = "drop") |>
  print(width = Inf)

gums |>                                    # group means
  group_by(drone_date, source, group) |>
  summarise(mean_ndvi = round(mean(ndvi), 3), .groups = "drop") |>
  pivot_wider(names_from = group, values_from = mean_ndvi) |>
  print()

gums |>                                    # Spearman, expect strongly negative
  select(source, drone_date, treatment_name, ndvi) |>
  pivot_wider(names_from = drone_date, values_from = ndvi, names_prefix = "d") |>
  group_by(source) |>
  summarise(spearman_jun_vs_oct = cor(`d2025-06-26`, `d2025-10-01`,
                                      method = "spearman"))

# ==== 8. FIGURES: TREATMENT MEANS ON EACH DRONE DATE =========================
make_dvs <- function(site_label, dd) {
  sid <- site_meta$site_id[site_meta$site == site_label]
  d <- trt_means |>
    filter(site_id == sid, drone_date == as.Date(dd)) |>
    mutate(source = factor(source, levels = c("Drone", "Planet", "Sentinel-2")))
  fl <- tab10 |>
    filter(Site == site_label, `Drone date` == as.Date(dd)) |>
    transmute(source = Source, label = paste0("F = ", sprintf("%.1f", F)))
  
  trts <- sort(unique(d$treatment_name))
  pal  <- setNames(scales::hue_pal(l = 62, c = 55)(length(trts)), trts)
  pal[names(pal) == "Control"] <- "grey55"
  
  ggplot(d, aes(source, ndvi, colour = treatment_name)) +
    geom_point(position = position_dodge(width = 0.7), size = 2.6) +
    geom_text(data = fl, aes(source, Inf, label = label), inherit.aes = FALSE,
              vjust = 1.6, size = 3.3) +
    scale_colour_manual(values = pal, name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(x = NULL, y = "Treatment mean NDVI",
         subtitle = paste0(site_label, ", drone flight ",
                           format(as.Date(dd), "%d %b"))) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}

# Main text (Figure 4): Mervs West, where the drone gave clearly stronger evidence
fig_dvs_mervs <- make_dvs("Mervs West", "2025-07-11")
fig_dvs_mervs
ggsave(out("Fig_dvs_MervsWest_MS.png"), fig_dvs_mervs, width = 7, height = 4.5, dpi = 300)

# Supplementary (S4-S7): remaining sites, two dates side by side where there are two
supp <- list(
  Brians  = make_dvs("Brians House", "2025-07-07"),
  Randals = make_dvs("Randals",      "2025-07-07"),
  Woodys  = make_dvs("Woodys", "2025-08-01") | make_dvs("Woodys", "2025-10-14"),
  Gums    = make_dvs("Gums",   "2025-06-26") | make_dvs("Gums",   "2025-10-01")
)
ggsave(out("Fig_S_dvs_BriansHouse.png"), supp$Brians,  width = 7,  height = 4.5, dpi = 300)
ggsave(out("Fig_S_dvs_Randals.png"),     supp$Randals, width = 7,  height = 4.5, dpi = 300)
ggsave(out("Fig_S_dvs_Woodys.png"),      supp$Woodys,  width = 12, height = 4.5, dpi = 300)
ggsave(out("Fig_S_dvs_Gums.png"),        supp$Gums,    width = 12, height = 4.5, dpi = 300)