# =============================================================================
# Script 6: Field Ground Truth Extraction
# -----------------------------------------------------------------------------
# Purpose : Extract per-point field measurements - Establishment (plants/m2),
#           Biomass_flowering (kg/ha), and the four harvest-time variables
#           (Biomass_maturity kg/ha, Grain_yield kg/ha, Thousand_grain_weight
#           g/1000 grains, Harvest_index %) - and attach treatment and zone by
#           POINT LOCATION (spatial join to strips_clean/zones_labelled), NOT
#           the shapefiles' own treat/cluster attributes.
#           ASSUMPTION: harvest samples sit at the same locations as the
#           Establishment points (metadata points all four harvest variables
#           at the Establishment shapefile), joined by pt_id.
#
# Inputs  : Biomass (flowering) Excel file, Establishment shapefile, Harvest
#           Index workbook ("Jackie" sheet) - paths from metadata
#           trial.plan shapefile + treatment names metadata
#           zones_labelled (Script 1)
#
# Outputs : {site_name}_field_observations_script6.csv/.rds
#           (long table, one row per point per variable, with treat + zone)
#           {site_name}_biomass_points_geo_script6.rds
#           {site_name}_establishment_points_geo_script6.rds
#           {site_name}_harvest_points_geo_script6.rds   (sf, with geometry)
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below.
# =============================================================================

library(dplyr)
library(readr)
library(readxl)
library(sf)
library(stringr)

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
maturity_date <- as.Date("2025-11-26")   # all four harvest variables (per paddock report)
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

# ---- 7. Harvest-time variables: maturity biomass, yield, TGW, harvest index
# All four come from the "Jackie" sheet of the Harvest Index workbook: one row
# per point (48), already in kg/ha (biomass, yield), g/1000 grains (TGW) and %
# (harvest index). Zeros would be treated as real, but this sheet has none.
# Location -> treatment/zone is reused from establishment_derived (same points).

harvest_path <- site_files %>%
  filter(variable == "Biomass_maturity data file") %>%
  pull(`file path`)

harvest_data <- read_excel(file.path(base_path, site_name, harvest_path),
                           sheet = "Jackie") %>%
  rename_with(trimws) %>%    # "Biomass_maturity " has a trailing space
  select(pt_id,
         Biomass_maturity,
         Grain_yield           = `Grain yield`,
         Thousand_grain_weight = `Thousand grain weight`,
         Harvest_index         = `Harvest index`)

harvest_derived <- establishment_derived %>%
  select(pt_id, treat, treatment_name, plot_order, zone_code, zone_label, geometry) %>%
  left_join(harvest_data, by = "pt_id")

# Checks: all 48 points matched, no NAs in any variable
nrow(harvest_derived)
harvest_derived %>% st_drop_geometry() %>%
  summarise(across(c(Biomass_maturity, Grain_yield, Thousand_grain_weight, Harvest_index),
                   ~ sum(is.na(.x))))

# Plausibility check on the pt_id -> location assumption (see notes below)
harvest_derived %>% st_drop_geometry() %>%
  group_by(zone_label, treat) %>%
  summarise(mean_yield_kg_ha = round(mean(Grain_yield)), .groups = "drop") %>%
  arrange(zone_label, mean_yield_kg_ha) %>%
  print(n = Inf)


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
make_long <- function(df, col, var_name, unit_label, sample_date) {
  df %>%
    st_drop_geometry() %>%
    transmute(
      pt_id, treat, treatment_name, plot_order, zone_code, zone_label,
      variable     = var_name,
      value        = .data[[col]],
      units        = unit_label,
      date_sampled = sample_date,
      standardised = TRUE
    )
}

harvest_long <- bind_rows(
  make_long(harvest_derived, "Biomass_maturity",      "Biomass_maturity",      "kg/ha",         maturity_date),
  make_long(harvest_derived, "Grain_yield",           "Grain_yield",           "kg/ha",         maturity_date),
  make_long(harvest_derived, "Thousand_grain_weight", "Thousand_grain_weight", "g/1000 grains", maturity_date),
  make_long(harvest_derived, "Harvest_index",         "Harvest_index",         "%",             maturity_date)
)

field_observations <- bind_rows(biomass_long, establishment_long, harvest_long) %>%
  arrange(variable, plot_order, zone_label)

field_observations %>% count(variable, date_sampled, standardised)



# ---- 9. Save the combined field observations table -------------------------
write_csv(field_observations,
          file.path(output_folder, paste0(site_name, "_field_observations_script6.csv")))
saveRDS(establishment_derived,
        file.path(output_folder, paste0(site_name, "_establishment_points_geo_script6.rds")))
saveRDS(harvest_derived,
        file.path(output_folder, paste0(site_name, "_harvest_points_geo_script6.rds")))


# ---- Save the point-level sf objects (WITH geometry) for Script 7 ---------
# field_observations (below) flattens to a table with no coordinates - these
# two keep the actual point geometry, so Script 7 can extract NDVI at each
# point's real location rather than rebuilding this from scratch.

saveRDS(biomass_joined,
        file.path(output_folder, paste0(site_name, "_biomass_points_geo_script6.rds")))
saveRDS(establishment_derived,
        file.path(output_folder, paste0(site_name, "_establishment_points_geo_script6.rds")))
