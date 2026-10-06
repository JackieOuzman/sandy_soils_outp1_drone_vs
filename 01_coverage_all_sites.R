
# =============================================================================
# 01_coverage_all_sites.R
# -----------------------------------------------------------------------------
# PURPOSE   Combined timeline of field sampling and remote sensing observations
#           (Sentinel-2, Planet, drone) for all six 2025 sites, as one figure
#           with sites as facets down the page.
#           Feeds MS Section 3.1: Figure 1 and Table 6 (image counts).
#
# INPUTS    Per site, in <pipeline_output_base>/<site_id>/ :
#             <site_id>_site_inventory_script1.rds     (field, drone, Sentinel-2 dates)
#             <site_id>_planet_raster_qc_script2b.rds  (Planet dates + cloud QC)
#           Metadata spreadsheet: sheet "seasons" (sowing / harvest dates)
#
# OUTPUTS   <pipeline_output_base>/Fig_coverage_all_sites_MS.png
#           Console: image counts per site, source and window (check Table 6)
#
# RERUN     After re-downloading Planet / Sentinel-2 and re-running the per-site
#           pipeline scripts, re-run this script top to bottom. If sowing or
#           harvest dates change, edit them in the spreadsheet AND in section 2
#           (section 3 warns if the two disagree).
#
# AUTHOR    Jackie Ouzman        LAST UPDATED  2026-10-06
# =============================================================================

# ==== 1. SETUP ===============================================================
library(tidyverse)
library(lubridate)
library(readxl)

base_path            <- "H:/Output-1"
metadata_path        <- file.path(base_path, "0.Site-info",
                                  "names of treatments per site 2025 metadata and other info.xlsx")
pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"

show_pre_sowing <- FALSE   # TRUE = also show Planet images from Nov/Dec 2024

# ==== 2. SITE METADATA =======================================================
# Facet order down the page = order of site_levels
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

# ==== 3. CHECK DATES AGAINST THE SPREADSHEET =================================
seasons <- read_excel(metadata_path, sheet = "seasons") |>
  filter(Year == 2025, Site %in% site_meta$site_id) |>
  transmute(site_id = Site,
            sowing_xl  = as.Date(`Sowing date`),
            harvest_xl = as.Date(`Harvest date`))

date_check <- site_meta |>
  left_join(seasons, by = "site_id") |>
  transmute(site, sowing_ok = sowing == sowing_xl, harvest_ok = harvest == harvest_xl)

if (!all(date_check$sowing_ok, date_check$harvest_ok)) {
  warning("Sowing/harvest dates in section 2 differ from the spreadsheet - check:")
  print(date_check)
}

# ==== 4. LOAD OBSERVATION DATES FOR ALL SITES ================================
# Same logic as the single-site timeline script, applied to each site
load_site_images <- function(site_id) {
  out <- file.path(pipeline_output_base, site_id)
  inv <- readRDS(file.path(out, paste0(site_id, "_site_inventory_script1.rds")))
  pl  <- readRDS(file.path(out, paste0(site_id, "_planet_raster_qc_script2b.rds")))
  bind_rows(
    inv |> filter(source %in% c("field", "drone", "satellite"), !is.na(date)) |>
      transmute(date = as.Date(date), source),
    pl  |> filter(!excluded_cloud) |>
      transmute(date = as.Date(date), source = "planet")
  ) |> mutate(site_id = site_id)
}

images <- map_dfr(site_meta$site_id, load_site_images) |>
  left_join(site_meta |> select(site_id, site, sowing, harvest), by = "site_id") |>
  mutate(source = factor(source,
                         levels = c("field", "drone", "planet", "satellite"),
                         labels = c("Field", "Drone", "Planet", "Sentinel-2")),
         in_window = date >= sowing & date <= harvest)

# Image counts per site and source: compare with Table 6 in the manuscript
# (outside_window for Planet = images captured before sowing)
image_counts <- images |>
  count(site, source, in_window) |>
  pivot_wider(names_from = in_window, values_from = n, values_fill = 0) |>
  rename(any_of(c(outside_window = "FALSE", in_window = "TRUE"))) |>
  arrange(site, source)
print(image_counts, n = Inf)

# ==== 5. COMBINED COVERAGE FIGURE ============================================
x_min <- if (show_pre_sowing) min(images$date) else as.Date("2025-04-01")
x_max <- as.Date("2026-01-05")

fig_coverage <- ggplot(images, aes(x = date, y = source, colour = source)) +
  geom_vline(data = site_meta, aes(xintercept = sowing),
             linetype = "dotted", colour = "darkgreen", linewidth = 0.6) +