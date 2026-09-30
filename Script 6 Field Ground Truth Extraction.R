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
#site_name     <- "2.Crystal_Brook_Brians_House"
base_path     <- "H:/Output-1"
metadata_path <- file.path(base_path, "0.Site-info",
                           "names of treatments per site 2025 metadata and other info.xlsx")

pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"
output_folder         <- file.path(pipeline_output_base, site_name)


# No row spacing, cut lengths or dates here any more:
# - values come already converted (plants/m2, kg/ha, ...) from each file's
#   checked "Jackie" sheet
# - sampling dates come from the metadata via Script 1's inventory (Section 2)

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

# ---- 2. Field variables for this site, from the metadata ------------------
# One row per variable: Excel data file + sheet, point shapefile, units and
# sampling date. Sheet defaults to "Jackie" unless the metadata's "sheet name"
# column says otherwise (only needed for workbooks shared between sites).
# Variable names come from the metadata "units" sheet and must match the
# Jackie sheet column headings exactly.

units_lookup <- read_excel(metadata_path, sheet = "units") %>%
  select(variable = variable_clm_name, units = variable_units) %>%
  filter(variable != "Establishment CV")          # not used downstream

field_dates <- readRDS(file.path(output_folder, paste0(site_name, "_site_inventory_script1.rds"))) %>%
  filter(source == "field") %>%
  distinct(variable, date_sampled = date)

path_for <- function(var, suffix, col = "file path") {
  x <- site_files[[col]][site_files$variable == paste(var, suffix)]
  if (length(x) == 0) NA_character_ else as.character(x[1])
}

field_vars <- units_lookup %>%
  rowwise() %>%
  mutate(data_path = path_for(variable, "data file"),
         sheet     = path_for(variable, "data file", col = "sheet name"),
         shp_path  = path_for(variable, "shp file")) %>%
  ungroup() %>%
  mutate(sheet = coalesce(sheet, "Jackie")) %>%
  left_join(field_dates, by = "variable") %>%
  filter(!is.na(data_path), !is.na(shp_path))    # e.g. no Protein at Walpeup

field_vars %>% select(variable, sheet, date_sampled, units)


# ---- 3. Reader: one variable = Jackie sheet value + point location ---------
# Values come already converted (plants/m2, kg/ha, ...) - no calculation here.
# Treatment and zone are assigned by POINT LOCATION (spatial join), not by
# the shapefile's own attributes.

read_field_variable <- function(var, data_path, sheet, shp_path, date_sampled, units) {
  values <- read_excel(file.path(base_path, site_name, data_path), sheet = sheet) %>%
    rename_with(trimws) %>%
    select(pt_id, value = all_of(var)) %>%
    filter(!is.na(pt_id)) %>%
    mutate(value = as.numeric(value))
  
  st_read(file.path(base_path, site_name, shp_path), quiet = TRUE) %>%
    select(pt_id) %>%
    left_join(values, by = "pt_id") %>%
    st_join(strips_clean %>% select(treat, treatment_name, plot_order)) %>%
    st_join(zones_labelled %>% select(zone_code, zone_label)) %>%
    mutate(variable = var, units = units, date_sampled = date_sampled)
}


# ---- 4. Read every variable and check ---------------------------------------
field_points <- do.call(rbind, lapply(seq_len(nrow(field_vars)), function(i) {
  v <- field_vars[i, ]
  read_field_variable(v$variable, v$data_path, v$sheet, v$shp_path, v$date_sampled, v$units)
}))

# Checks per variable: points, missing values, zeros, points outside a
# strip/zone, value range, date and units
field_points %>%
  st_drop_geometry() %>%
  group_by(variable) %>%
  summarise(n_points  = n(),
            n_missing = sum(is.na(value)),
            n_zero    = sum(value == 0, na.rm = TRUE),
            no_strip  = sum(is.na(treat)),
            no_zone   = sum(is.na(zone_code)),
            min       = round(min(value, na.rm = TRUE), 1),
            median    = round(median(value, na.rm = TRUE), 1),
            max       = round(max(value, na.rm = TRUE), 1),
            date      = first(date_sampled),
            units     = first(units),
            .groups   = "drop") %>%
  print(width = Inf)



# ---- 5. Point files for Script 7 (wide, one row per point, with geometry) --
# Same object and column names as the earlier version of this script, so
# Script 7 reads them unchanged.

make_wide <- function(vars) {
  vars <- intersect(vars, unique(field_points$variable))        # skip any not at this site
  geo  <- field_points %>%
    filter(variable == vars[1]) %>%
    select(pt_id, treat, treatment_name, plot_order, zone_code, zone_label)
  vals <- field_points %>%
    st_drop_geometry() %>%
    filter(variable %in% vars) %>%
    select(pt_id, variable, value) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value)
  left_join(geo, vals, by = "pt_id")
}

establishment_derived <- make_wide("Establishment") %>%
  rename(establishment_plants_m2 = Establishment)

biomass_joined <- make_wide("Biomass_flowering")

harvest_derived <- make_wide(c("Biomass_maturity", "Grain yield",
                               "Thousand grain weight", "Harvest index", "Protein")) %>%
  rename(Grain_yield           = `Grain yield`,
         Thousand_grain_weight = `Thousand grain weight`,
         Harvest_index         = `Harvest index`)

# Checks: one row per point in each file (a point in two polygons would duplicate)
c(establishment = nrow(establishment_derived),
  biomass       = nrow(biomass_joined),
  harvest       = nrow(harvest_derived))
c(establishment = anyDuplicated(establishment_derived$pt_id),
  biomass       = anyDuplicated(biomass_joined$pt_id),
  harvest       = anyDuplicated(harvest_derived$pt_id))   # all should be 0

# ---- 6. Long table: one row per point per variable (no geometry) ----------
# Variable names use underscores (Grain_yield etc.), as in earlier versions.
field_observations <- field_points %>%
  st_drop_geometry() %>%
  transmute(pt_id, treat, treatment_name, plot_order, zone_code, zone_label,
            variable = gsub(" ", "_", variable),
            value, units, date_sampled) %>%
  arrange(variable, plot_order, zone_label)

field_observations %>% count(variable, date_sampled, units)


# ---- 7. Save -----------------------------------------------------------------
# Long table (csv + rds) and the three point files WITH geometry for Script 7
write_csv(field_observations,
          file.path(output_folder, paste0(site_name, "_field_observations_script6.csv")))
saveRDS(field_observations,
        file.path(output_folder, paste0(site_name, "_field_observations_script6.rds")))

saveRDS(establishment_derived,
        file.path(output_folder, paste0(site_name, "_establishment_points_geo_script6.rds")))
saveRDS(biomass_joined,
        file.path(output_folder, paste0(site_name, "_biomass_points_geo_script6.rds")))
saveRDS(harvest_derived,
        file.path(output_folder, paste0(site_name, "_harvest_points_geo_script6.rds")))
