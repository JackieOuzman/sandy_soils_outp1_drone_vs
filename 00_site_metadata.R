# 00_site_metadata.R - sourced by scripts 01-04. EDIT DATES HERE ONLY
# (and in the metadata spreadsheet, "seasons" sheet; script 01 warns if they differ)
library(tidyverse)
library(lubridate)

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