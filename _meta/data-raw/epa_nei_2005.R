# EPA NEI pre-2008 data -----
# # !IMPORTANT! This script is left as is for potential future use.
# Note that this 2005 data does NOT include CO2 or note distinct
# GHGs. 
# 
# NEI pre-2008 data were downloaded individually from ___
# Downloads are in .mdb (Microsoft Access) format, which is not easily opened
# on macOS. 
# Each download was opened in MS Access and each Table was
# exported as a text file (.txt)
# These were then transferred to our MS Teams/OneDrive location
# and then transferred to local machine

source("R/_load_pkgs.R")

# Source Classification Codes (SCCs) categories -----
# quick download from https://sor-scc-api.epa.gov/sccwebservices/sccsearch/
scc_codes <- data.table::fread("_meta/data-raw/epa/SCCDownload-2024-0718-132502.csv",
                               header = TRUE,
                               colClasses = "character") %>% 
  clean_names()

scc_onroad_county <- data.table::fread(
  "_meta/data-raw/epa/nei_2005/SCC onroad/SCC County.txt",
  sep = ",",
  header = TRUE,
  colClasses = c("character",
                 "character",
                 "character",
                 "numeric")) %>% 
  clean_names()

scc_nonroad <-  data.table::fread(
  "_meta/data-raw/epa/nei_2005/SCC nonroad/SCC County 1.txt",
  sep = ",",
  header = TRUE,
  colClasses = c("character",
                 "character",
                 "character",
                 "numeric")) %>% 
  clean_names()

scc_point <-  data.table::fread(
  "_meta/data-raw/epa/nei_2005/SCC point/POINT05_V2_SCC_SUMMARY_COUNTY.txt",
  sep = ",",
  header = TRUE,
  colClasses = c(rep("character", 13),
                 "numeric")) %>% 
  clean_names()

scc_nonpoint <-  data.table::fread( "_meta/data-raw/epa/nei_2005/SCC nonpoint/NONPOINT05_V2_SCC_SUMMARY_COUNTY.txt",
                                    sep = ",",
                                    header = TRUE,
                                    colClasses = c(rep("character", 12),
                                                   "numeric",
                                                   rep("character", 2))) %>% 
  clean_names()

# by tiers -----
tier1_county  <- data.table::fread( "_meta/data-raw/epa/nei_2005/Tier1/Tier1 County.txt",
                                    sep = ",",
                                    header = TRUE,
                                    colClasses = c(rep("character", 9),
                                                   "numeric")) %>% 
  clean_names()

tier2_county  <- data.table::fread(
  "_meta/data-raw/epa/nei_2005/Tier2/Tier2 County (00-30).txt",
  sep = ",",
  header = TRUE,
  colClasses = c(rep("character", 12),
                 "numeric")) %>% 
  rbind(
    data.table::fread(
      "_meta/data-raw/epa/nei_2005/Tier2/Tier2 County (31-78).txt",
      sep = ",",
      header = TRUE,
      colClasses = c(rep("character", 12),
                     "numeric"))
  ) %>% 
  clean_names()

tier3_county <- data.table::fread(
  "_meta/data-raw/epa/nei_2005/Tier3/Tier3 County (00-30).txt",
  sep = ",",
  header = TRUE,
  colClasses = c(rep("character", 14),
                 "numeric")) %>% 
  rbind(data.table::fread(
    "_meta/data-raw/epa/nei_2005/Tier3/Tier3 County (31-78).txt",
    sep = ",",
    header = TRUE,
    colClasses = c(rep("character", 14),
                   "numeric"))) %>% 
  clean_names()

# filter to our states, review -----

# get tier descriptions

scc_pollutant_desc <- 
  scc_onroad_county %>% 
  select(pollutant_code) %>% 
  bind_rows(scc_nonroad
  )

tier_desc <- tier3_county %>% 
  select(tier1_code, tier1_description,
         tier2_code, tier2_description,
         tier3_code, tier3_description) %>% 
  unique()

pollutant_desc <- tier3_county %>% 
  select(pollutant_code, 
         pollutant_code_description) %>% 
  bind_rows(tier1_county %>% 
              select(pollutant_code, pollutant_code_description)) %>% 
  bind_rows(tier2_county %>% 
              select(pollutant_code, pollutant_code_description)) %>% 
  unique()


mn_wi_scc_onroad <- scc_onroad_county %>% 
  filter(state_county_fips %in% county_geography$GEOID) %>% 
  left_join(county_geography %>% 
              select(GEOID, NAME, STATE) %>% 
              sf::st_drop_geometry(),
            by = c("state_county_fips" = "GEOID")) %>% 
  left_join(scc_codes, by = "scc") %>% 
  mutate(nei_inventory_year = 2005,
         GEOID = state_county_fips)

mn_wi_scc_nonroad <- scc_nonroad %>% 
  filter(state_county_fips %in% county_geography$GEOID) %>% 
  left_join(county_geography %>% 
              select(GEOID, NAME, STATE) %>% 
              sf::st_drop_geometry(),
            by = c("state_county_fips" = "GEOID")) %>% 
  left_join(scc_codes, by = "scc") %>% 
  mutate(nei_inventory_year = 2005,
         GEOID = state_county_fips)

sectors

onroad <- mn_wi_scc_onroad %>% 
  left_join(pollutant_desc,
            by = "pollutant_code") %>% 
  select(nei_inventory_year, 
         NAME, GEOID, scc, 
         pollutant_code,
         pollutant_code_description,
         data_category, 
         sector,
         starts_with("scc"),
         starts_with("tier"),
         annual_emissions
  )


nei_county_multi_year 
