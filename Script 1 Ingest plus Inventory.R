# =============================================================================
# Script 1: Ingest & Inventory
# -----------------------------------------------------------------------------
# Purpose : For a single site, read the master metadata workbook and pull
#           together everything Stages 2-6 will need: boundary shapefile path,
#           trial strip shapefile path, drone image dates/paths, satellite
#           dates/paths, field observation dates/paths, and zone (soil type)
#           polygons with real zone labels attached.
#           Outputs one "inventory" tibble so gaps are visible before any
#           spatial analysis starts.
#
# Inputs  : "H:/Output-1/0.Site-info/names of treatments per site 2025
#            metadata and other info.xlsx"  -- sheets "file location etc",
#            "zone_details"
#           Sentinel-2, drone, and Planet image folders (see SITE CONFIG below)
#
# Outputs : site_inventory     (tibble, one row per date/source/variable)
#           zones_labelled     (sf polygons, zone code + real zone label)
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below. Everything
# else in this script derives from that one value.
# =============================================================================

library(dplyr)
library(readxl)
library(stringr)
library(readr)
library(sf)


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

# Zone shapefile's column name is site-specific and NOT reliable from the
# metadata sheet ("zone names clm heading name" field has drifted out of
# sync with the actual shapefiles) - hardcoded lookup instead, carried over
# from the NDVI Viewer app's existing site handling. See DECISIONS_LOG.
zone_field <- case_when(
  site_name == "1.Walpeup_MRS125"             ~ "gridcode",
  site_name == "2.Crystal_Brook_Brians_House"  ~ "cluster",
  site_name == "3.Wynarka_Mervs_West"          ~ "fcl_mdl",
  site_name == "4.Wharminda_Woodys"            ~ "fcl_mdl",
  site_name == "5.Walpeup_Gums"                ~ "cluster3",
  site_name == "6.Crystal_Brook_Randals"       ~ "cluster",
  site_name == "7.Wharminda_Bonanza"           ~ "cluster",  # "DN"?
  site_name == "8.Wynarka_Tanks"               ~ "zone",
  TRUE ~ NA_character_
)
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

# ---- 4b. Zone shapefile: read polygons, attach real zone labels -----------
# Zone codes (1, 2, 3...) mean different things at different sites - labels
# come from metadata ("zone_details" sheet), joined onto the shapefile so
# downstream scripts get real names (e.g. "Swale") not just numeric codes.

zones_path <- site_files %>%
  filter(variable == "location of zone shp") %>%
  pull(file_path)

zones_raw <- st_read(file.path(base_path, site_name, zones_path))

zone_labels <- read_excel(metadata_path, sheet = "zone_details") %>%
  filter(Site == site_name) %>%
  select(zone_code = `zone names`, zone_label = `zone label names`) %>%
  mutate(zone_code = as.character(zone_code),
         zone_label = str_extract(zone_label, "(?<=\\=).*"))

zones_labelled <- zones_raw %>%
  rename(zone_code = !!zone_field) %>%
  mutate(zone_code = as.character(zone_code)) %>%
  left_join(zone_labels, by = "zone_code") %>%
  select(zone_code, zone_label)

zones_labelled %>% st_drop_geometry() %>% distinct()

# Record the zone shapefile itself in the inventory (no date - it's a static
# layer, not a time-series observation), consistent with how other sources
# are tracked.
zone_inventory <- tibble(
  date = as.Date(NA), source = "zone", variable = "zone_shapefile",
  file_name = basename(zones_path), file_path = zones_path
)

# ---- 5. Combine all sources into one Site 1 inventory ----------------------
site_inventory <- bind_rows(satellite_inventory, drone_inventory, planet_inventory,
                            field_inventory, zone_inventory) %>%
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

# ---- 7. Save the inventory + zone polygons for use by downstream scripts ---
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

write_csv(site_inventory, file.path(output_folder, paste0(site_name, "_site_inventory_script1.csv")))
saveRDS(site_inventory,  file.path(output_folder, paste0(site_name, "_site_inventory_script1.rds")))
saveRDS(zones_labelled,  file.path(output_folder, paste0(site_name, "_zones_labelled_script1.rds")))