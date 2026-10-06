# ==== 1. SETUP ===============================================================
library(tidyverse)
library(lubridate)

pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"

# Same site metadata as coverage_all_sites.R (copied so this script stands alone)
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

# ==== 2. LOAD ALL SITES ======================================================
load_anova <- function(site_id) {
  f <- file.path(pipeline_output_base, site_id,
                 paste0(site_id, "_table_S1_anova_full_MS.csv"))
  read_csv(f, show_col_types = FALSE) |> mutate(site_id = site_id)
}

anova_raw <- map_dfr(site_meta$site_id, load_anova)

# ---- inspect (paste this output back to me) ----
glimpse(anova_raw)
anova_raw |> count(site_id)

# distinct values of every text column (shows how source / index / term are coded)
anova_raw |>
  select(where(is.character), -site_id) |>
  map(\(x) head(unique(x), 12))

# ==== 3. STANDARDISE COLUMNS =================================================
# EDIT these three lines to your real column names (from the glimpse above):
anova_all <- anova_raw |>
  rename(date = Date, source = Source, index = Index) |>
  mutate(date    = as.Date(date),
         source  = factor(source, levels = c("Planet", "Sentinel-2", "Drone")),
         index   = factor(index,  levels = c("NDVI", "NDRE")),
         neglogp = -log10(p)) |>
  left_join(site_meta, by = "site_id") |>
  filter(date >= sowing, date <= harvest)       # analysis window only

# quick check against Table 7 (dates tested and dates significant per cell)
anova_all |>
  group_by(site, source, index) |>
  summarise(tested = n(), significant = sum(p < 0.05), .groups = "drop") |>
  arrange(site, index, source)

# ==== 4. COMBINED TREATMENT-EFFECT FIGURE ====================================
fig_effect <- ggplot(anova_all,
                     aes(x = date, y = neglogp, colour = source)) +
  geom_vline(data = site_meta, aes(xintercept = sowing),
             linetype = "dotted", colour = "darkgreen", linewidth = 0.6) +
  geom_vline(data = site_meta, aes(xintercept = harvest),
             linetype = "dotted", colour = "sienna", linewidth = 0.6) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "grey40") +
  geom_line(aes(group = source), alpha = 0.5) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c("Planet"     = "#084594",
                                 "Sentinel-2" = "#6BAED6",
                                 "Drone"      = "#E6550D"), name = NULL) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  coord_cartesian(xlim = c(as.Date("2025-04-15"), as.Date("2026-01-05"))) +
  facet_grid(site ~ index, switch = "y") +
  labs(x = NULL, y = expression(-log[10](italic(p)))) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
        strip.text.x = element_text(face = "bold"),
        panel.spacing = unit(0.6, "lines"),
        panel.border = element_rect(colour = "grey80", fill = NA))

fig_effect

# ==== 4b. TREATMENT-EFFECT FIGURES (one per index) ============================
angle_months <- TRUE                                   # FALSE for flat labels
y_max <- ceiling(max(anova_all$neglogp, na.rm = TRUE)) # same y-range for both

make_effect_plot <- function(idx) {
  d <- filter(anova_all, index == idx)
  
  ggplot(d, aes(x = date, y = neglogp, colour = source)) +
    geom_vline(data = site_meta, aes(xintercept = sowing),
               linetype = "dotted", colour = "darkgreen", linewidth = 0.6) +
    geom_vline(data = site_meta, aes(xintercept = harvest),
               linetype = "dotted", colour = "sienna", linewidth = 0.6) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "grey40") +
    # lines for the two satellite series only; drone = isolated flights
    geom_line(data = filter(d, source != "Drone"),
              aes(group = source), alpha = 0.5) +
    geom_point(size = 1.8) +
    scale_colour_manual(values = c("Planet"     = "#084594",
                                   "Sentinel-2" = "#6BAED6",
                                   "Drone"      = "#E6550D"), name = NULL) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    coord_cartesian(xlim = c(as.Date("2025-04-15"), as.Date("2026-01-05")),
                    ylim = c(0, y_max)) +
    facet_grid(site ~ ., switch = "y") +
    labs(x = NULL, y = expression(-log[10](italic(p)))) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
          panel.spacing = unit(0.6, "lines"),
          panel.border = element_rect(colour = "grey80", fill = NA),
          axis.text.x = element_text(angle = if (angle_months) 45 else 0,
                                     hjust = if (angle_months) 1 else 0.5))
}

fig_effect_ndvi <- make_effect_plot("NDVI")
fig_effect_ndre <- make_effect_plot("NDRE")

fig_effect_ndvi
fig_effect_ndre

ggsave(file.path(pipeline_output_base, "Fig_treatment_effect_NDVI_all_sites_MS.png"),
       fig_effect_ndvi, width = 8, height = 9, dpi = 300)
ggsave(file.path(pipeline_output_base, "Fig_treatment_effect_NDRE_all_sites_MS.png"),
       fig_effect_ndre, width = 8, height = 9, dpi = 300)
