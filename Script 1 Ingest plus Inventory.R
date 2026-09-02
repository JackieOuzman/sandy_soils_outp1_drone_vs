# =============================================================================
# Script 1: Ingest & Inventory
# -----------------------------------------------------------------------------
# Purpose : For a single site, read the master metadata workbook and pull
#           together everything Stages 2-6 will need: boundary shapefile path,
#           trial strip shapefile path, drone image dates/paths, satellite
#           dates/paths, and field observation dates/paths.
#           Outputs one "inventory" tibble so gaps are visible before any
#           spatial analysis starts.
#
# Inputs  : "H:/Output-1/0.Site-info/names of treatments per site 2025
#            metadata and other info.xlsx"  -- sheet "file location etc"
#           Sentinel-2 and drone image folders (see SITE CONFIG below)
#
# Outputs : site_inventory  (tibble, one row per date/source/variable)
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below. Everything
# else in this script derives from that one value.
# =============================================================================

library(dplyr)
library(readxl)
library(stringr)
library(readr)


# ============================== SITE CONFIG =================================
site_name     <- "1.Walpeup_MRS125"
base_path     <- "H:/Output-1"
metadata_path <- file.path(base_path, "0.Site-info",
                           "names of treatments per site 2025 metadata and other info.xlsx")

sentinel_folder <- file.path(base_path, site_name,
                             "7.In_Season_data/25/8.Sentinel_QGIS_Jackie")
drone_folder    <- file.path(base_path, site_name,
                             "7.In_Season_data/25/3.Drone_Imagery/Drone_NDVI_all")
planet_folder <- file.path(base_path, site_name,
                           "7.In_Season_data/25/2.Satellite_Imagery/Planet/PSScene")

pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"
output_folder         <- file.path(pipeline_output_base, site_name)
# =============================================================================


# ---- 1. Read the file-location lookup sheet, filtered to this site --------
site_files <- read_excel(metadata_path, sheet = "file location etc") %>%
  filter(Site == site_name) %>%
  select(Site, variable, file_name = `file name`, file_path = `file path`,
         other_details = `other details`)

# ---- 2. Satellite inventory: list NDVI tifs, parse date from filename -----
satellite_inventory <- tibble(
  file_path = list.files(sentinel_folder, pattern = "_NDVI_10m\\.tif$",
                         full.names = TRUE)
) %>%
  mutate(
    file_name = basename(file_path),
    date      = as.Date(str_extract(file_name, "\\d{4}-\\d{2}-\\d{2}")),
    source    = "satellite",
    variable  = "NDVI"
  ) %>%
  select(date, source, variable, file_name, file_path) %>%
  arrange(date)

# ---- 3. Drone inventory: list NDVI tifs, parse date from filename ---------
# NOTE: swap for read_excel() + filter(Site == site_name) once the "Drone
# file location" metadata sheet is finished.
drone_inventory <- tibble(
  file_path = list.files(drone_folder, pattern = "(?i)ndvi.*\\.tif$",
                         full.names = TRUE)
) %>%
  mutate(
    file_name = basename(file_path),
    date      = as.Date(str_extract(file_name, "\\d{8}"), format = "%Y%m%d"),
    source    = "drone",
    variable  = "NDVI"
  ) %>%
  select(date, source, variable, file_name, file_path)

# ---- 3b. Planet inventory: list SR images, pair each with its udm2 mask ---
# Filenames don't include NDVI (unlike Sentinel/drone) since Planet delivers
# raw 8-band surface reflectance - NDVI gets calculated in Script 2/3 from
# the Red and NIR bands, not read directly from a file here.

planet_images <- tibble(
  file_path = list.files(planet_folder, pattern = "_AnalyticMS_SR_8b_clip\\.tif$",
                         full.names = TRUE)
) %>%
  mutate(
    file_name = basename(file_path),
    date      = as.Date(str_extract(file_name, "^\\d{8}"), format = "%Y%m%d")
  )

planet_masks <- tibble(
  mask_path = list.files(planet_folder, pattern = "_udm2_clip\\.tif$",
                         full.names = TRUE)
) %>%
  mutate(mask_name = basename(mask_path),
         date      = as.Date(str_extract(mask_name, "^\\d{8}"), format = "%Y%m%d"))

planet_inventory <- planet_images %>%
  left_join(planet_masks, by = "date") %>%
  mutate(source = "planet", variable = "SR_8band") %>%
  select(date, source, variable, file_name, file_path, mask_path)

planet_inventory

# ---- 4. Field observation inventory, pulled from site_files ---------------
# Dates and file paths live on separate rows in the metadata (e.g.
# "Establishment date collected" vs "Establishment data file"), so join them
# together on the shared variable stem. "CV" variables are dropped since
# they're a summary stat of the same collection event, not a separate date.

field_dates <- site_files %>%
  filter(str_detect(variable, "date collected"),
         !str_detect(variable, "CV")) %>%
  transmute(
    variable = str_remove(variable, " date collected"),
    date     = as.Date(as.numeric(other_details), origin = "1899-12-30")
  )

field_files <- site_files %>%
  filter(str_detect(variable, "data file")) %>%
  transmute(
    variable = str_remove(variable, " data file"),
    file_name, file_path
  )

field_inventory <- field_dates %>%
  left_join(field_files, by = "variable") %>%
  mutate(source = "field") %>%
  select(date, source, variable, file_name, file_path)

field_inventory


# ---- 5. Combine all sources into one Site 1 inventory ----------------------
site_inventory <- bind_rows(satellite_inventory, drone_inventory, planet_inventory, field_inventory) %>%
  mutate(site = site_name) %>%
  arrange(date)

site_inventory
# ---- 6. Quick visual check: timeline of all observation dates -------------
library(ggplot2)

ggplot(site_inventory %>% filter(!is.na(date)),
       aes(x = date, y = source, colour = source)) +
  geom_point(size = 3) +
  labs(title = paste("Observation timeline —", site_name),
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")


# ---- 7. Save the inventory for use by downstream scripts -------------------
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

write_csv(site_inventory, file.path(output_folder, paste0(site_name, "_site_inventory_script1.csv")))
saveRDS(site_inventory,  file.path(output_folder, paste0(site_name, "_site_inventory_script1.rds")))
