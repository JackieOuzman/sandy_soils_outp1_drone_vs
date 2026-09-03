# =============================================================================
# Script 6: Field Ground Truth Extraction
# -----------------------------------------------------------------------------
# Purpose : Extract per-point Biomass_flowering (kg/ha) from the field
#           biomass workbook, spatially join to strips_clean and
#           zones_labelled (by point location, NOT the shapefile's own
#           treat/cluster attributes - see Script 6 notes), producing one
#           row per point with treatment and zone attached.
#
#          
#
# Inputs  : Biomass Excel file (path from metadata: "Biomass_flowering data file")
#           trial.plan shapefile + treatment names metadata
#           zones_labelled (Script 1)
#
# Outputs : {site_name}_biomass_flowering_ground_truth_script6.csv/.rds
#           (one row per point, with treat + zone attached)
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below.
# =============================================================================

library(dplyr)
library(readr)
library(readxl)
library(sf)

# ============================== SITE CONFIG =================================
site_name     <- "1.Walpeup_MRS125"
base_path     <- "H:/Output-1"
metadata_path <- file.path(base_path, "0.Site-info",
                           "names of treatments per site 2025 metadata and other info.xlsx")

pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"
output_folder         <- file.path(pipeline_output_base, site_name)


row_spacing_m <- 0.3   # confirmed by Enqi - applies to both

cut_length_biomass_m       <- 4     # from the Biomass file itself - matches its own stated kg/ha exactly
cut_length_establishment_m <- 0.5   # per Enqi: default protocol is 4 rows x 0.5m


establishment_date <- as.Date("2025-05-19")
biomass_flowering_date <- as.Date("2025-09-22")
# =============================================================================


# ---- 1. Rebuild strips_clean and zones_labelled (same as Script 3) --------
site_files <- read_excel(metadata_path, sheet = "file location etc") %>%
  filter(Site == site_name)

treatment_names <- read_excel(metadata_path, sheet = "treatment names") %>%
  filter(Site == site_name)

strips_path <- site_files %>%
  filter(variable == "trial.plan") %>%
  pull(`file path`)

strips <- st_read(file.path(base_path, site_name, strips_path))

strips_clean <- strips %>%
  filter(treat %in% treatment_names$treat) %>%
  left_join(treatment_names %>% select(treat, treatment_name = `Shorthand Name`,
                                       plot_order = `Order in Paddock`),
            by = "treat")

zones_labelled <- readRDS(file.path(output_folder, paste0(site_name, "_zones_labelled_script1.rds")))
zones_labelled <- st_make_valid(zones_labelled)


# ---- 2. Read the trusted per-point Biomass_flowering values ----------------
biomass_path <- site_files %>%
  filter(variable == "Biomass_flowering data file") %>%
  pull(`file path`)

biomass_pts_data <- read_excel(file.path(base_path, site_name, biomass_path),
                               sheet = "Jackie_MRS125")

biomass_pts_data
# ---- 3. Check for a Biomass_flowering shapefile in metadata ---------------
site_files %>% filter(str_detect(variable, "Biomass_flowering"))


# ---- 4. Read the Biomass point shapefile and join Excel values by pt_id ---
biomass_shp_path <- site_files %>%
  filter(variable == "Biomass_flowering shp file") %>%
  pull(`file path`)

biomass_pts <- st_read(file.path(base_path, site_name, biomass_shp_path))

names(biomass_pts)
st_geometry_type(biomass_pts) %>% table()

# Join the trusted Excel values onto the point geometry by pt_id
biomass_pts_joined <- biomass_pts %>%
  select(pt_id, geometry) %>%
  left_join(biomass_pts_data, by = "pt_id")

biomass_pts_joined %>% st_drop_geometry() %>% head(10)

# Check: did every point get a matching biomass value?
sum(is.na(biomass_pts_joined$Biomass_flowering))


# ---- 5. Spatially join to strips_clean and zones_labelled (by location) ---
biomass_joined <- biomass_pts_joined %>%
  st_join(strips_clean %>% select(treat, treatment_name, plot_order)) %>%
  st_join(zones_labelled %>% select(zone_code, zone_label))

biomass_joined %>% st_drop_geometry()

# Check: did every point land inside a strip and a zone?
sum(is.na(biomass_joined$treat))
sum(is.na(biomass_joined$zone_code))


# ---- 6. Establishment: derive density using assumed row spacing/cut length


establishment_shp_path <- site_files %>%
  filter(variable == "Establishment shp file") %>%
  pull(`file path`)

establishment_pts <- st_read(file.path(base_path, site_name, establishment_shp_path))

establishment_derived <- establishment_pts %>%
  select(pt_id, Row1, Row2, Row3, Row4, geometry) %>%
  mutate(
    row_total       = Row1 + Row2 + Row3 + Row4,
    mean_per_row    = row_total / 4,
    establishment_plants_m2 = mean_per_row / (row_spacing_m * cut_length_establishment_m)
  ) %>%
  st_join(strips_clean %>% select(treat, treatment_name, plot_order)) %>%
  st_join(zones_labelled %>% select(zone_code, zone_label))

establishment_derived %>% st_drop_geometry() %>%
  select(pt_id, treat, zone_label, row_total, establishment_plants_m2) %>%
  arrange(desc(establishment_plants_m2))

# Sanity check: does this look like a plausible wheat establishment rate?
summary(establishment_derived$establishment_plants_m2)


# ---- 8. Combine Biomass and Establishment into one field observations df --


biomass_long <- biomass_joined %>%
  st_drop_geometry() %>%
  transmute(
    pt_id, treat, treatment_name, plot_order, zone_code, zone_label,
    variable      = "Biomass_flowering",
    value         = Biomass_flowering,
    units         = "kg/ha",
    date_sampled  = biomass_flowering_date,
    standardised  = TRUE
  )

establishment_long <- establishment_derived %>%
  st_drop_geometry() %>%
  transmute(
    pt_id, treat, treatment_name, plot_order, zone_code, zone_label,
    variable      = "Establishment",
    value         = establishment_plants_m2,
    units         = "plants/m2",
    date_sampled  = establishment_date,
    standardised  = TRUE
  )
field_observations <- bind_rows(biomass_long, establishment_long) %>%
  arrange(variable, plot_order, zone_label)

field_observations %>% count(variable, date_sampled, standardised)



# ---- 9. Save the combined field observations table -------------------------
write_csv(field_observations,
          file.path(output_folder, paste0(site_name, "_field_observations_script6.csv")))
saveRDS(field_observations,
        file.path(output_folder, paste0(site_name, "_field_observations_script6.rds")))


# ---- Save the point-level sf objects (WITH geometry) for Script 7 ---------
# field_observations (below) flattens to a table with no coordinates - these
# two keep the actual point geometry, so Script 7 can extract NDVI at each
# point's real location rather than rebuilding this from scratch.

saveRDS(biomass_joined,
        file.path(output_folder, paste0(site_name, "_biomass_points_geo_script6.rds")))
saveRDS(establishment_derived,
        file.path(output_folder, paste0(site_name, "_establishment_points_geo_script6.rds")))
