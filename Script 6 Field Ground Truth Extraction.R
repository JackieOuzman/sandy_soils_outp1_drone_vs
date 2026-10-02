# =============================================================================
# Script 6: Field Ground Truth Extraction
# -----------------------------------------------------------------------------
# Purpose : Assemble per-point field measurements for one site - Establishment
#           (plants/m2), Biomass_flowering (kg/ha), Biomass_maturity (kg/ha),
#           Grain yield (kg/ha), Thousand grain weight (g/1000 grains),
#           Harvest index (%) and Protein (%) where available - and attach
#           treatment and zone by POINT LOCATION (spatial join to
#           strips_clean / zones_labelled), NOT the shapefiles' own attributes.
#
# Approach: Metadata-driven, identical code for every site. For each variable
#           the metadata ("file location etc") gives an Excel data file and a
#           point shapefile. Values are read from each file's checked "Jackie"
#           sheet (already in final units - NO row spacing, cut length or unit
#           conversion in R), joined to point locations by pt_id.
#           Rewritten 30 Sep 2026; reproduces the earlier Walpeup outputs
#           exactly (all values identical - see DECISIONS_LOG).
#
# Requires: Each Jackie sheet: header in row 1, a "pt_id" column, and value
#           columns named exactly as in the metadata "units" sheet (e.g.
#           "Establishment", "Grain yield"). Extra columns are ignored.
#           Zeros are real values; blanks = missing (never 0 for missing).
#           Sheet name defaults to "Jackie"; for a workbook shared between
#           sites, add a "sheet name" column to "file location etc" and fill
#           it for that row only (e.g. "Jackie GUMS", "Jackie Randals").
#
# Inputs  : metadata: "file location etc" (data file + shp file per variable,
#           trial.plan), "units", "treatment names"
#           {site_name}_site_inventory_script1.rds (sampling dates)
#           {site_name}_zones_labelled_script1.rds
#
# Outputs : {site_name}_field_observations_script6.csv/.rds
#           (long table, one row per point per variable, with treat + zone)
#           {site_name}_establishment_points_geo_script6.rds
#           {site_name}_biomass_points_geo_script6.rds
#           {site_name}_harvest_points_geo_script6.rds   (sf, with geometry,
#           same column names as before, read by Script 7)
#
# Site notes (see DECISIONS_LOG for detail):
#   1. Walpeup MRS125 - harvest samples at the establishment points (same
#      shapefile); pt 28 harvest = mean of two samples (28.1, 28.2).
#   2. Brians House  - harvest has its own shapefile (same locations as
#      establishment); flowering biomass n = 18 (pt 33 excluded as
#      implausible; Excel pt 11 has no match in the sampling-plan shapefile,
#      which has pt 111 - left unmatched); pt 20 yield + HI blank (grain
#      spill); row spacing 0.3 m TO BE CONFIRMED.
#
# TO RUN A DIFFERENT SITE: change site_name in SITE CONFIG below, then check
# the Section 4 table: points, missing values, zeros, and no_strip/no_zone
# (points outside the analysed strips/zones) for every variable.
# =============================================================================

library(dplyr)
library(readr)
library(readxl)
library(sf)
library(stringr)

# ============================== SITE CONFIG =================================
site_name     <- "1.Walpeup_MRS125"
#site_name     <- "2.Crystal_Brook_Brians_House"
#site_name     <- "3.Wynarka_Mervs_West"
#site_name     <- "4.Wharminda_Woodys"
#site_name     <- "5.Walpeup_Gums"

base_path     <- "H:/Output-1"
metadata_path <- file.path(base_path, "0.Site-info",
                           "names of treatments per site 2025 metadata and other info.xlsx")

pipeline_output_base <- "H:/Output-1/Jackie notes processing etc/Drone_Vs_Satellite"
output_folder         <- file.path(pipeline_output_base, site_name)

season_year <- 2025   # which Year to read from the "Plant sampling notes" sheet


# No row spacing, cut lengths or dates here any more:
# - values come already converted (plants/m2, kg/ha, ...) from each file's
#   checked "Jackie" sheet
# - sampling dates come from the metadata via Script 1's inventory (Section 2)


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
# Files and dates come from the "Plant sampling notes" sheet: one row per
# Site x Year x sampling_event (Establishment, Biomass_flowering, Harvest),
# giving the Excel data file, its sheet, the point shapefile and the date.
# The "units" sheet maps each variable to its sampling_event and units.
# Variable names must match the Jackie sheet column headings exactly; a
# variable whose column isn't in that sheet is skipped (Section 3).

plant_files <- read_excel(metadata_path, sheet = "Plant sampling notes") %>%
  filter(Site == site_name, Year == season_year) %>%
  transmute(sampling_event,
            date_sampled = as.Date(date),
            data_path    = data_file,
            sheet        = coalesce(sheet_name, "Jackie"),
            shp_path     = shp_file)

units_lookup <- read_excel(metadata_path, sheet = "units") %>%
  select(variable = variable_clm_name, units = variable_units, sampling_event) %>%
  filter(variable != "Establishment CV")          # not used downstream

field_vars <- units_lookup %>%
  left_join(plant_files, by = "sampling_event") %>%
  filter(!is.na(data_path), !is.na(shp_path))    # skip events with no files yet

field_vars %>% select(variable, sampling_event, sheet, date_sampled, units)
# ---- 3. Reader: one variable = Jackie sheet value + point location ---------
# Values come already converted (plants/m2, kg/ha, ...) - no calculation here.
# Treatment and zone are assigned by POINT LOCATION (spatial join), not by
# the shapefile's own attributes.

read_field_variable <- function(var, data_path, sheet, shp_path, date_sampled, units) {
  raw <- read_excel(file.path(base_path, site_name, data_path), sheet = sheet) %>%
    rename_with(trimws)
  if (!var %in% names(raw)) {                      # e.g. no Protein column at Walpeup
    message("Skipping ", var, ": no column of that name in ", basename(data_path))
    return(NULL)
  }
  values <- raw %>%
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
field_points <- do.call(rbind, Filter(Negate(is.null), lapply(seq_len(nrow(field_vars)), function(i) {
  v <- field_vars[i, ]
  read_field_variable(v$variable, v$data_path, v$sheet, v$shp_path, v$date_sampled, v$units)
})))   # Filter() drops variables skipped by the reader

# Points outside the analysed treatment strips are excluded from all
# point-level analysis (e.g. Mervs West: 21 points in the original control
# strip, which was disturbed and moved in 2025; no points were sampled in the
# new control). No effect at Walpeup or Brians House (all points in strips).
field_points %>% st_drop_geometry() %>% filter(is.na(treat)) %>% count(variable, name = "n_excluded")
field_points <- field_points %>% filter(!is.na(treat))

# Variables with no values at all at this site are dropped, with a message
# (e.g. Walpeup Gums: TGW and Protein columns are kept, empty, in the Excel
# file to show they were not measured). Avoids empty variables reaching Script 7.
empty_vars <- field_points %>%
  st_drop_geometry() %>%
  group_by(variable) %>%
  summarise(all_na = all(is.na(value)), .groups = "drop") %>%
  filter(all_na) %>%
  pull(variable)
if (length(empty_vars) > 0) message("Dropping variables with no values at this site: ",
                                    paste(empty_vars, collapse = ", "))
field_points <- field_points %>% filter(!variable %in% empty_vars)

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
  rename(any_of(c(Grain_yield           = "Grain yield",              # any_of(): skip any
                  Thousand_grain_weight = "Thousand grain weight",    # variable not measured
                  Harvest_index         = "Harvest index")))          # at this site (e.g. no TGW at Woodys)

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



field_points %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  group_by(variable) %>%
  summarise(n_points  = n(),
            n_missing = sum(is.na(value)),
            n_zero    = sum(value == 0, na.rm = TRUE),
            no_strip  = sum(is.na(treat)),
            no_zone   = sum(is.na(zone_code)),
            median    = round(median(value, na.rm = TRUE), 1),
            .groups   = "drop") %>%
  print(width = Inf)

