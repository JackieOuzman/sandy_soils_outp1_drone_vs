# =============================================================================
# 04_groundtruth_all_sites.R
# -----------------------------------------------------------------------------
# PURPOSE   Cross-site tables and figures for MS Section 3.5 (NDVI/NDRE vs field
#           ground truth): Table 11 (r by site, variable, source), Table 12 (the
#           8 pre-specified tests and adjusted versions), supporting figures.
#
# INPUTS    Per site, in <pipeline_output_base>/<site_id>/ :
#             <site_id>_groundtruth_correlation_summary_script7.csv
#             <site_id>_groundtruth_correlation_pvalues_script7.csv
#             <site_id>_harvest_correlations_script7.csv
#             <site_id>_harvest_correlations_adjusted_script7.csv
#
# OUTPUTS   (added as the script is built)
#
# AUTHOR    Jackie Ouzman        LAST UPDATED  2026-10-06
# =============================================================================

# ==== 1. SETUP ===============================================================
library(tidyverse)

pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"
out <- function(f) file.path(pipeline_output_base, f)

# ==== 2. SITE LIST ===========================================================
site_levels <- c("Walpeup MRS125", "Brians House", "Mervs West",
                 "Woodys", "Gums", "Randals")
site_meta <- tribble(
  ~site_id,                        ~site,
  "1.Walpeup_MRS125",              "Walpeup MRS125",
  "2.Crystal_Brook_Brians_House",  "Brians House",
  "3.Wynarka_Mervs_West",          "Mervs West",
  "4.Wharminda_Woodys",            "Woodys",
  "5.Walpeup_Gums",                "Gums",
  "6.Crystal_Brook_Randals",       "Randals"
) |> mutate(site = factor(site, levels = site_levels))

# ==== 3. LOAD (read as text while inspecting, so layouts can differ) =========
read_gt <- function(suffix) {
  map_dfr(site_meta$site_id, \(sid) {
    f <- out(file.path(sid, paste0(sid, "_", suffix, ".csv")))
    if (!file.exists(f)) { message("MISSING: ", f); return(NULL) }
    read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c")) |>
      mutate(site_id = sid)
  })
}

gt_summary  <- read_gt("groundtruth_correlation_summary_script7")
gt_pvalues  <- read_gt("groundtruth_correlation_pvalues_script7")
gt_harvest  <- read_gt("harvest_correlations_script7")
gt_adjusted <- read_gt("harvest_correlations_adjusted_script7")

# ==== 4. INSPECT (temporary: paste this output back, then delete) ============
for (nm in c("gt_summary", "gt_pvalues", "gt_harvest", "gt_adjusted")) {
  d <- get(nm)
  cat("\n=====", nm, "=====\n")
  print(count(d, site_id))
  glimpse(d)
  print(map(select(d, where(is.character), -site_id),
            \(x) head(unique(x), 12)))
}
# ==== 4. HELPERS =============================================================
num   <- \(x) suppressWarnings(as.numeric(x))
stars <- \(p) case_when(is.na(p) ~ "", p < 0.001 ~ "***", p < 0.01 ~ "**",
                        p < 0.05 ~ "*", TRUE ~ "")
neg   <- \(x) sub("^-", "\u2212", x)                       # proper minus sign
fmt_r <- \(r, p) ifelse(is.na(r), "–", neg(paste0(sprintf("%.2f", r), stars(p))))
rng2  <- \(x) {                                            # "0.48 to 0.51"
  x <- range(x, na.rm = TRUE)
  if (round(x[1], 2) == round(x[2], 2)) neg(sprintf("%.2f", x[1]))
  else neg(paste0(sprintf("%.2f", x[1]), " to ", sprintf("%.2f", x[2])))
}
src_lab   <- c(planet = "Planet", satellite = "Sentinel-2", drone = "Drone")
col_order <- c("Planet NDVI", "Planet NDRE", "Sentinel-2 NDVI", "Sentinel-2 NDRE",
               "Drone NDVI")

# ==== 5. TABLE 11: ESTABLISHMENT AND FLOWERING BIOMASS =======================
# r with stars (* p<0.05, ** p<0.01, *** p<0.001). A dash = no value (drone only
# where a flight fell within 7 days of sampling). n per site: MS Table 5.
tab11 <- gt_summary |>
  left_join(gt_pvalues, by = c("site_id", "variable", "metric")) |>
  mutate(across(c(starts_with("cor_"), starts_with("p_")), num)) |>
  pivot_longer(c(starts_with("cor_"), starts_with("p_")),
               names_to = c(".value", "source"), names_pattern = "(cor|p)_(.*)") |>
  mutate(col = paste(src_lab[source], metric), cell = fmt_r(cor, p)) |>
  filter(col %in% col_order) |>
  left_join(site_meta, by = "site_id") |>
  mutate(Variable = recode(variable, Biomass_flowering = "Flowering biomass")) |>
  select(Site = site, Variable, col, cell) |>
  pivot_wider(names_from = col, values_from = cell) |>
  select(Site, Variable, all_of(col_order)) |>
  arrange(Site, Variable)

print(tab11, n = Inf, width = Inf)
write_csv(tab11, out("Table_11_groundtruth_establishment_flowering_MS.csv"))

# ==== 6. TABLE 12: HARVEST-TIME VARIABLES, 8 PRE-SPECIFIED TESTS =============
# Tests: grain yield and maturity biomass x NDVI/NDRE x Planet/Sentinel-2,
# Bonferroni threshold 0.05/8 = 0.00625. "Adjusted" = partial correlation.
bonf <- 0.05 / 8

n_yield <- gt_harvest |>
  filter(variable == "Grain_yield", metric == "NDVI") |>
  transmute(site_id, source, n = num(n))

tab12 <- gt_adjusted |>
  mutate(across(c(r_raw, p_raw, r_treat_zone), num),
         across(starts_with("bonf_ok"), \(x) x == "TRUE"),
         pass_raw = p_raw < bonf) |>
  group_by(site_id, source) |>
  summarise(`Grain yield r`      = rng2(r_raw[variable == "Grain_yield"]),
            `Maturity biomass r` = rng2(r_raw[variable == "Biomass_maturity"]),
            `Passed, unadjusted`              = paste0(sum(pass_raw), "/4"),
            `Passed, adjusted for treatment`  = paste0(sum(bonf_ok_treat), "/4"),
            `Passed, adjusted for treatment + zone` = paste0(sum(bonf_ok_treat_zone), "/4"),
            `Partial r (treatment + zone)`    = rng2(r_treat_zone),
            .groups = "drop") |>
  left_join(n_yield, by = c("site_id", "source")) |>
  left_join(site_meta, by = "site_id") |>
  mutate(Source = src_lab[source]) |>
  arrange(site, match(source, c("planet", "satellite"))) |>
  select(Site = site, Source, n, everything(), -site_id, -source)

print(tab12, n = Inf, width = Inf)
write_csv(tab12, out("Table_12_groundtruth_harvest_prespecified_MS.csv"))

# ==== 7. TABLE 13: EXPLORATORY (HARVEST INDEX, THOUSAND GRAIN WEIGHT) ========
tab13 <- gt_harvest |>
  filter(set == "exploratory") |>
  mutate(across(c(r, p), num),
         col  = paste(src_lab[source], metric),
         cell = fmt_r(r, p)) |>
  left_join(site_meta, by = "site_id") |>
  mutate(Variable = recode(variable, Harvest_index = "Harvest index",
                           Thousand_grain_weight = "Thousand grain weight")) |>
  select(Site = site, Variable, col, cell) |>
  pivot_wider(names_from = col, values_from = cell) |>
  select(Site, Variable, all_of(col_order[1:4])) |>
  arrange(Site, Variable)

print(tab13, n = Inf, width = Inf)
write_csv(tab13, out("Table_13_groundtruth_exploratory_MS.csv"))
