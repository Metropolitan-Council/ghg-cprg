# Process and clean sector-level data for municipal utilities
source("R/_load_pkgs.R")

raw_7610_elecMunis <- readRDS(here("_energy", "data", "combined_MNelecMunis_elecByClass_raw7610.RDS")) %>%
  # Remove munis operating outside the core metro
  filter(!utility %in% c(
    "Elk River Municipal Utilities",
    "New Prague Utilities Commission",
    "North Branch Municipal Water & Light",
    "Princeton Public Utilities"
  )) %>%
  mutate(
    sector_mapped = case_when(
      grepl("(?i)non[- ]?farm[- ]?residential", sector) ~ "residential",
      grepl("(?i)commercial", sector) ~ "commercial",
      grepl("(?i)industrial", sector) ~ "industrial",
      grepl("(?i)street.*highway.*lighting", sector) ~ "commercial",
      grepl("(?i)all other", sector) ~ "residential", # strongest assumption, includes "residential with space heating", "irrigation" and "other (includes municipals)"
      TRUE ~ NA_character_
    ) # Assign NA for unknown categories
  ) %>%
  filter(!is.na(sector_mapped)) %>% # functionally removes the "Entered Total" records only
  # manual entry of relevant CTU name and CTU class
  mutate(
    ctu_name = case_when(
      utility == "City of Anoka" ~ "Anoka",
      utility == "City of Chaska" ~ "Chaska",
      utility == "City of North St Paul" ~ "North Saint Paul",
      utility == "Shakopee Public Utilities" ~ "Shakopee"
    ),
    county_name = case_when(
      utility == "City of Anoka" ~ "Anoka",
      utility == "City of Chaska" ~ "Chaska",
      utility == "City of North St Paul" ~ "North Saint Paul",
      utility == "Shakopee Public Utilities" ~ "Shakopee"
    ),
    source = "Electricity",
    ctu_class = "CITY"
  )

write_rds(raw_7610_elecMunis, here("_energy", "data", "MNelecMunis_activityData_2014_2023.RDS"))
