# =============================================================================
# Script 1: Ingest & Inventory
# -----------------------------------------------------------------------------
# Purpose : For a single site, read the master metadata workbook and pull
#           together everything Stages 2-6 will need: boundary shapefile path,
#           trial strip shapefile path, drone image dates/paths, satellite
#           dates/paths (NDVI file + raw 10-band stack for NDRE), field
#           observation dates/paths, and zone (soil type) polygons with real
#           zone labels attached.
#           Outputs one "inventory" tibble so gaps are visible before any
#           spatial analysis starts.
#
# Inputs  : "H:/Output-1/0.Site-info/names of treatments per site 2025
#            metadata and other info.xlsx"  -- sheets "file location etc",
#            "zone_details"
#           Sentinel-2, drone, and Planet image folders (see SITE CONFIG below)
#
# Outputs : site_inventory     (tibble, one row per date/source/variable;
#           satellite rows include both file_path [pre-made NDVI] and
#           raw_path [10-band stack, needed for NDRE]; Planet rows include
#           mask_path [udm2])
#           zones_labelled     (sf polygons, zone code + real zone label)
#           {site_name}_observation_timeline_script1.png (Figure, Section 3.1)
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
#site_name     <- "2.Crystal_Brook_Brians_House"
#site_name     <- "3.Wynarka_Mervs_West"
#site_name     <- "4.Wharminda_Woodys"
#site_name     <- "5.Walpeup_Gums"


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

season_year <- 2025   # which Year to read from the "Plant sampling notes" and "seasons" sheets

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

# ---- 2. Satellite inventory: list NDVI tifs + raw 10-band stack, by date --
# raw_path (the "_10m.tif" file WITHOUT "NDVI" in the name) is needed for
# NDRE, which requires the red-edge band - not present in the pre-made NDVI
# file. Matched to the NDVI file by date rather than assumed to always
# exist, since a missing raw stack for a given date is a real possibility
# worth catching rather than silently producing NA downstream.

satellite_ndvi_files <- tibble(
  file_path = list.files(sentinel_folder, pattern = "_NDVI_10m\\.tif$",
                         full.names = TRUE)
) %>%
  mutate(
    file_name = basename(file_path),
    date      = as.Date(str_extract(file_name, "\\d{4}-\\d{2}-\\d{2}"))
  )

satellite_raw_files <- tibble(
  raw_path = list.files(sentinel_folder, pattern = "_10m\\.tif$", full.names = TRUE)
) %>%
  filter(!str_detect(raw_path, "NDVI")) %>%
  mutate(
    raw_name = basename(raw_path),
    date     = as.Date(str_extract(raw_name, "\\d{4}-\\d{2}-\\d{2}"))
  )

satellite_inventory <- satellite_ndvi_files %>%
  left_join(satellite_raw_files %>% select(date, raw_path), by = "date") %>%
  mutate(
    source    = "satellite",
    variable  = "NDVI"
  ) %>%
  select(date, source, variable, file_name, file_path, raw_path) %>%
  arrange(date)

# Check every NDVI date has a matching raw stack - flag if not, rather than
# silently carrying an NA raw_path into downstream NDRE calculations
sum(is.na(satellite_inventory$raw_path))

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


# ---- 4. Field observation inventory, from the "Plant sampling notes" sheet -
# One row per sampling event in the metadata (date + Excel data file),
# expanded to one row per field variable via the "units" sheet's
# sampling_event column. "CV" variables are dropped (a summary stat of the
# same collection event). Events with no data file yet are skipped.

plant_files <- read_excel(metadata_path, sheet = "Plant sampling notes") %>%
  filter(Site == site_name, Year == season_year, !is.na(data_file))

field_inventory <- read_excel(metadata_path, sheet = "units") %>%
  filter(!str_detect(variable_clm_name, "CV")) %>%
  select(variable = variable_clm_name, sampling_event) %>%
  inner_join(plant_files, by = "sampling_event") %>%
  transmute(date      = as.Date(date),
            source    = "field",
            variable,
            file_name = basename(data_file),
            file_path = data_file)

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

# ---- 6. Observation timeline: all sources, saved for the manuscript -------
library(ggplot2)

timeline_data <- site_inventory %>%
  filter(!is.na(date)) %>%
  mutate(source = factor(source, levels = c("field", "drone", "planet", "satellite"),
                         labels = c("Field", "Drone", "Planet", "Sentinel-2")))

# Sowing/harvest dates for reference lines (same source as Script 5's ANOVA plot)
season_2025 <- read_excel(metadata_path, sheet = "seasons") %>%
  filter(Site == site_name, Year == season_year)

sowing_date  <- as.Date(season_2025$`Sowing date`)
harvest_date <- as.Date(season_2025$`Harvest date`)

timeline_plot <- ggplot(timeline_data, aes(x = date, y = source, colour = source)) +
  geom_vline(xintercept = sowing_date, linetype = "dotted", colour = "darkgreen") +
  geom_vline(xintercept = harvest_date, linetype = "dotted", colour = "sienna") +
  geom_point(size = 3) +
  annotate("text", x = sowing_date, y = Inf, label = "Sowing",
           angle = 90, vjust = -0.5, hjust = 1.1, size = 3, colour = "darkgreen") +
  annotate("text", x = harvest_date, y = Inf, label = "Harvest",
           angle = 90, vjust = -0.5, hjust = 1.1, size = 3, colour = "sienna") +
  labs(title = paste("Observation timeline —", site_name),
       subtitle = "Dates with usable imagery or field sampling, by source",
       x = NULL, y = NULL,
       caption = paste(
         "Shows all data captured or downloaded, before any cloud-based exclusion.",
         "Sentinel-2 dates were pre-filtered for cloud cover (<30%) at export, upstream",
         "of this pipeline, so excluded Sentinel-2 dates are not recorded and cannot be",
         "shown. Planet dates shown here have not yet been screened for cloud cover;",
         "see Script 2b / Figure [X] for Planet dates excluded by the 30% rule.",
         sep = "\n"
       )) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0, size = 8, colour = "grey30"))

timeline_plot

ggsave(file.path(output_folder, paste0(site_name, "_observation_timeline_script1.png")),
       timeline_plot, width = 9, height = 4, dpi = 300)

# ---- 7. Save the inventory + zone polygons for use by downstream scripts ---
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

write_csv(site_inventory, file.path(output_folder, paste0(site_name, "_site_inventory_script1.csv")))
saveRDS(site_inventory,  file.path(output_folder, paste0(site_name, "_site_inventory_script1.rds")))
saveRDS(zones_labelled,  file.path(output_folder, paste0(site_name, "_zones_labelled_script1.rds")))


# What was found, per source
site_inventory %>% count(source, variable)

# Zone codes and labels
zones_labelled %>% st_drop_geometry() %>% distinct()



site_inventory %>% count(source, variable)
zones_labelled %>% st_drop_geometry() %>% distinct()

site_inventory %>%
  filter(source %in% c("drone", "field")) %>%
  select(date, source, variable) %>%
  arrange(date)

site_inventory %>%
  filter(source %in% c("planet", "satellite")) %>%
  group_by(source) %>%
  summarise(first = min(date), last = max(date), n = n(),
            in_season = sum(date >= sowing_date & date <= harvest_date))

