# Script to import ancillary housing data from MN geospatial commons
source("R/_load_pkgs.R")

cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS")

# fetch parcel data from MN Geospatial Commons
# need to modify import code from councilR to get multiple layers

import_from_gpkg_all_layers <- function(link, save_file = FALSE, save_path = getwd(), .crs = 4326, 
                                        keep_temp = FALSE, .quiet = TRUE) {
  requireNamespace("rlang", quietly = TRUE)
  requireNamespace("sf", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  
  purrr::map(c(link), rlang:::check_string)
  purrr::map(c(save_file, keep_temp, .quiet), rlang:::check_bool)
  rlang:::check_string(save_path)
  rlang:::check_number_whole(.crs)
  
  temp <- tempfile()
  download.file(link, temp, quiet = .quiet)
  file_names <- strsplit(link, split = "/")
  file_name <- tail(file_names[[1]], 1) %>% 
    gsub(pattern = "gpkg_", replacement = "") %>% 
    gsub(pattern = ".zip", replacement = "")
  
  # Unzip the GeoPackage file
  gpkg_path <- unzip(temp, paste0(file_name, ".gpkg"))
  
  # Get a list of all layer names
  layers <- sf::st_layers(gpkg_path)$name
  
  # Read each layer and transform CRS
  layer_data <- purrr::map(layers, function(layer) {
    sf::read_sf(gpkg_path, layer = layer, quiet = .quiet) %>% 
      sf::st_transform(crs = .crs)
  })
  
  # Combine all layers into one data frame
  out_sf <- dplyr::bind_rows(layer_data, .id = "layer_name")
  
  if (keep_temp == FALSE) {
    fs::file_delete(gpkg_path)
  }
  
  if (save_file == TRUE) {
    saveRDS(out_sf, paste0(save_path, "/", file_name, ".RDS"))
  }
  
  return(out_sf)
}

mn_parcel <- import_from_gpkg_all_layers(
  "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metrogis/plan_regonal_parcels_2021/gpkg_plan_regonal_parcels_2021.zip") %>% 
    st_drop_geometry()


mn_parcel %>% distinct(DWELL_TYPE) %>% arrange() %>% 
  print(n=200) # approximately 1/2 assigned

# large possibility for error in classification. Attempting to structure so overriding of earlier
# cases happens as necessary
mn_parcel <- mn_parcel %>%
  mutate(
    mc_classification = case_when(
      ## RESIDENTIAL
      # Apartments
      grepl("apartment|condo|apt|nursing|astd|eldry|fraternity|sorority",DWELL_TYPE, ignore.case = TRUE) ~ "apartment",
      grepl("Apartment|APARMENT|Apt|Elderly Liv Fac|Housing - Low Income > 3 Units|HRA|Nursing|Sr Citizens", USECLASS1, ignore.case = TRUE) ~ "apartment",
      grepl("APT|condo", HOME_STYLE, ignore.case = TRUE) ~ "apartment",
      grepl("Apartment|APARMENT|Apt|Elderly Liv Fac|Housing - Low Income > 3 Units|HRA|Nursing|Sr Citizens", USECLASS2, ignore.case = TRUE) ~ "apartment",
      
      
      # Mobile Homes
      grepl("mobile|manufactured",DWELL_TYPE, ignore.case = TRUE) ~ "manufactured_home",
      grepl("manufactured|MH", USECLASS1, ignore.case = TRUE) ~ "manufactured_home",
      grepl("manufactured|MH", HOME_STYLE, ignore.case = TRUE) ~ "manufactured_home",
      grepl("manufactured|MH", USECLASS2, ignore.case = TRUE) ~ "manufactured_home",
      
      # multifamily
      grepl("townh|duplex|triplex|two-family|two family|three family|two residences|twin|multi res",DWELL_TYPE, ignore.case = TRUE) ~ "multifamily_home",
      grepl("Res 2-3|Double Bungalow|Duplex|Apartment|Low Income < 4 Units|Townh|Triplex", USECLASS1, ignore.case = TRUE) ~ "multifamily_home",
      grepl("Quad|Townh|duplex", HOME_STYLE, ignore.case = TRUE) ~ "multifamily_home",
      grepl("Res 2-3|Double Bungalow|Duplex|Apartment|Low Income < 4 Units|Townh|Triplex", USECLASS2, ignore.case = TRUE) ~ "multifamily_home",

      
      # Single-Family Homes
      grepl("Frame|Cabin|BUNGALOW|SPLIT|Rambler|Log", HOME_STYLE, ignore.case = TRUE) ~ "single_family_home",
      grepl("single|s.fam",DWELL_TYPE, ignore.case = TRUE) ~ "single_family_home",
      grepl("Res 1 unit|CABIN|Residential|Zero Lot Line", USECLASS1, ignore.case = TRUE) ~ "single_family_home",
      grepl("Res 1 unit|CABIN|Residential|Zero Lot Line", USECLASS2, ignore.case = TRUE) ~ "single_family_home",
      TRUE ~ NA)) %>% 
    
    ### COMM/IND/PUBLIC/AG
    mutate(
      mc_classification = case_when(
      # Commercial
      grepl("store",DWELL_TYPE, ignore.case = TRUE) ~ "commercial",

      DWELL_TYPE %in% c(
        "RETAIL STR", "RESTAURANT", "REST FSTFD", "BAR/TAVERN", "MARKET",
        "DISCNT STR",  "AUTO SHWRM", "AUTO CENTR", "POST OFFIC",
        "DAYCARECTR", "CLASSROOM", "THEATER", "BANK", "CARWASH", "BWLNGALLEY",
        "LAUNDROMAT", "MORTUARY", "COUNTRYCLB", "HAIRSALON", "CLUBHOUSE",
        "HEALTH CLB", "STABLE", "BED & BREAKFAST", 
        "PRKNG STRC", "OFC,MD/DTL", "OFFICE", "SHPCTR, COM", "SHPCTR,NBH",
        "SERVC GAR", "VET HSPTL", "CHURCH", "CONV STORE", "SERVC STN",
        "HOTEL", "MOTEL", "OFC,CORPTE","HOSPITALS","CREAMERY", "DEPT STORE"
      ) ~ "commercial",
      
      grepl("Commercial|NON-PROFIT COMM|Com Ma & Pa|College|Skyways|Comm Services|SERVICE STATION|Golf Course|Condo|Cooperative|Restaurant|Marina|Arena|church", USECLASS1, ignore.case = TRUE) ~ "commercial",
      grepl("Commercial|Arena|Charit", USECLASS2, ignore.case = TRUE) ~ "commercial",
      grepl("church|Inst|Colleges|Private|Apprenticeship Training Facilities|hospitals", XUSECLASS1, ignore.case = TRUE) ~ "commercial",
      
      # Industrial
      DWELL_TYPE %in% c(
        "INDL,MANFG", "MFG/PROCES", "INDUSTRIAL IMPROVED", "SHED,UTIL",
        "INDUSTRIAL VACANT", "GREENHOUSE", "UTILITIES", "UTIL,TELCM",
        "GARG/STRG","HANGAR/MTC", "SHED,EQUIP"
      ) ~ "industrial",
      grepl("Industrial|INDUSTIAL|Machinery|Utilit|Railroad|El Gen Mach|Utilities|Pub Util", USECLASS1, ignore.case = TRUE) ~ "industrial",
      grepl("Industrial", USECLASS2, ignore.case = TRUE) ~ "industrial",
      grepl("railroad|airport", XUSECLASS1, ignore.case = TRUE) ~ "industrial",
      
      ##public buildings
      grepl("School|Public|Municipal|County|State Property|FEDERAL|state", USECLASS1, ignore.case = TRUE) ~ "public_building",
      grepl("Cities|state|hwy dept|county|waste control|township|federal property|public schools|Commission", XUSECLASS1, ignore.case = TRUE) ~ "public_building",
      
      
      #agricultural
      grepl("Agricultural|Farm|AG|Green Acres|Preserve|Managed Forrest", USECLASS1, ignore.case = TRUE) ~ "agriculture",
      grepl("Ag", USECLASS2, ignore.case = TRUE) ~ "agriculture",
      grepl("farm", XUSECLASS1, ignore.case = TRUE) ~ "agriculture",
      
      TRUE ~ mc_classification)) %>% 
      
      ### EMPTY LAND
      mutate(
        mc_classification = case_when(
      # Vacant/Exempt
      grepl("vacant|forfeit",DWELL_TYPE, ignore.case = TRUE) ~ "vacant",
      
      #open space
      grepl("Vacant|Res V Land|VAC LAND|Unimproved|Wetlands|Forest|HUNTING|Open Space", USECLASS1, ignore.case = TRUE) ~ "no_building",
      grepl("Vacant|Wetlands|Common", USECLASS2, ignore.case = TRUE) ~ "no_building",
      grepl("street|park|dnr|cemetary", XUSECLASS1, ignore.case = TRUE) ~ "no_building",

      TRUE ~ mc_classification)
    )


mn_parcel_assigned <- mn_parcel %>%
  #remaining properties are EXEMPT - implying non-profit status, putting in commercial
  mutate(mc_classification = ifelse(is.na(mc_classification) & FIN_SQ_FT > 0,
                                   "commercial",
                                   mc_classification)) %>% 
  filter(!is.na(mc_classification))

tapply(mn_parcel_assigned$FIN_SQ_FT, mn_parcel_assigned$mc_classification, "median")
big_building <- mn_parcel_assigned %>% filter(FIN_SQ_FT > 50000, mc_classification == "single_family_home")
test <- mn_parcel_assigned %>% filter(grepl("park", XUSECLASS1, ignore.case = TRUE), mc_classification != "no_building")

mn_parcel_assigned %>% filter(FIN_SQ_FT != 0) %>% count(mc_classification)

data_status <- mn_parcel_assigned %>%
  group_by(CO_NAME, mc_classification) %>%
  summarise(
    total_buildings = n(),
    zero_sq_ft = sum(EMV_BLDG == 0, na.rm = TRUE),
    non_zero_sq_ft = sum(EMV_BLDG != 0, na.rm = TRUE),
    pct_zero_sq_ft = (zero_sq_ft / total_buildings) * 100
  )


### fill in 0 data for mc_classification - first based on ctu_name when avaiable, and county_name where ctu is also blank

mn_parcel_predict <- mn_parcel_assigned %>%
  # Calculate mc_classification averages by CTU_NAME and CO_NAME
  group_by(mc_classification, CTU_ID_TXT) %>%
  mutate(
    mean_ctu_sqft = if_else(all(FIN_SQ_FT == 0, na.rm = TRUE), NA_real_, mean(FIN_SQ_FT[FIN_SQ_FT > 0], na.rm = TRUE)),
    mean_ctu_emv = if_else(all(EMV_BLDG == 0, na.rm = TRUE), NA_real_, mean(EMV_BLDG[EMV_BLDG > 0], na.rm = TRUE)),
    mean_ctu_year = if_else(all(YEAR_BUILT == 0, na.rm = TRUE), NA_real_, mean(YEAR_BUILT[YEAR_BUILT > 0], na.rm = TRUE))
  ) %>%
  ungroup() %>%
  group_by(mc_classification, CO_NAME) %>%
  mutate(
    mean_co_sqft = if_else(all(FIN_SQ_FT == 0, na.rm = TRUE), NA_real_, mean(FIN_SQ_FT[FIN_SQ_FT > 0], na.rm = TRUE)),
    mean_co_emv = if_else(all(EMV_BLDG == 0, na.rm = TRUE), NA_real_, mean(EMV_BLDG[EMV_BLDG > 0], na.rm = TRUE)),
    mean_co_year = if_else(all(YEAR_BUILT == 0, na.rm = TRUE), NA_real_, mean(YEAR_BUILT[YEAR_BUILT > 0], na.rm = TRUE))
  ) %>%
  ungroup() %>%
  # In-fill zeros using the calculated means
  mutate(
    FIN_SQ_FT = if_else(FIN_SQ_FT == 0, coalesce(mean_ctu_sqft, mean_co_sqft, FIN_SQ_FT), FIN_SQ_FT),
    EMV_BLDG = if_else(EMV_BLDG == 0, coalesce(mean_ctu_emv, mean_co_emv, EMV_BLDG), EMV_BLDG),
    YEAR_BUILT = if_else(YEAR_BUILT == 0, coalesce(mean_ctu_year, mean_co_year, YEAR_BUILT), YEAR_BUILT)
  ) %>%
  # select columns
  select(CO_NAME, CTU_NAME,CTU_ID_TXT, FIN_SQ_FT, EMV_BLDG, YEAR_BUILT, mc_classification)

mn_parcel_map <- mn_parcel_predict %>% 
group_by(CO_NAME,CTU_NAME, CTU_ID_TXT, mc_classification) %>% 
  summarize(mean_sq_ft = mean(FIN_SQ_FT),
            total_sq_ft = sum(FIN_SQ_FT), 
            mean_emv = mean(EMV_BLDG),
            total_emv = sum(EMV_BLDG),
            mean_year = mean(YEAR_BUILT)) %>% 
  mutate(ctu_id = as.numeric(CTU_ID_TXT)) %>% 
  left_join(cprg_ctu, by = c("ctu_id" = "gnis",
                             "CO_NAME" = "county_name")) %>%
  st_as_sf()

ggplot(mn_parcel_map %>% 
         filter(mc_classification == "single_family_home")) +
  geom_sf(aes(fill = mean_sq_ft), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", name = "Mean square foot (SFH)") +
  theme_minimal()

ggplot(mn_parcel_map %>% 
         filter(mc_classification == "single_family_home")) +
  geom_sf(aes(fill = mean_emv), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", name = "Mean house value (SFH)") +
  theme_minimal()

ggplot(mn_parcel_map %>% 
        filter(mc_classification == "single_family_home")) +
  geom_sf(aes(fill = mean_year), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", name = "Mean year of construction") +
  theme_minimal()

ggplot(mn_parcel_map %>% 
         filter(mc_classification == "single_family_home",
                CO_NAME != "Hennepin"),
       aes(x = mean_sq_ft, y = mean_emv)) +
  geom_point() + theme_bw() +
  ggtitle("CTU: Mean square footage vs mean housing value")


# what does 2005 data look like?    

mn_parcel_2005 <- import_from_gpkg_all_layers(
  "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metrogis/plan_regonal_parcels_2005/gpkg_plan_regonal_parcels_2005.zip") %>% 
  st_drop_geometry()

mn_parcel_2005 <- mn_parcel_2005 %>%
  mutate(
    mc_classification = case_when(
      ## RESIDENTIAL
      # Apartments
      grepl("apartment|condo|apt|nursing|astd|eldry|fraternity|sorority",DWELL_TYPE, ignore.case = TRUE) ~ "apartment",
      grepl("Apartment|APARMENT|Apt|Elderly Liv Fac|Housing - Low Income > 3 Units|HRA|Nursing|Sr Citizens|RES 4 OR MORE UNITS", USE1_DESC, ignore.case = TRUE) ~ "apartment",
      grepl("APT|condo", HOME_STYLE, ignore.case = TRUE) ~ "apartment",
      grepl("Apartment|APARMENT|Apt|Elderly Liv Fac|Housing - Low Income > 3 Units|HRA|Nursing|Sr Citizens", USE2_DESC, ignore.case = TRUE) ~ "apartment",
      
      
      # Mobile Homes
      grepl("mobile|manufactured",DWELL_TYPE, ignore.case = TRUE) ~ "manufactured_home",
      grepl("manufactured|MH", USE1_DESC, ignore.case = TRUE) ~ "manufactured_home",
      grepl("manufactured|MH", HOME_STYLE, ignore.case = TRUE) ~ "manufactured_home",
      grepl("manufactured|MH", USE2_DESC, ignore.case = TRUE) ~ "manufactured_home",
      
      # multifamily
      grepl("townh|duplex|triplex|two-family|two family|three family|two residences|twin|multi res",DWELL_TYPE, ignore.case = TRUE) ~ "multifamily_home",
      grepl("Res 2-3|RES 1-3 UNITS|Double Bungalow|Duplex|Apartment|Low Income < 4 Units|Townh|Triplex", USE1_DESC, ignore.case = TRUE) ~ "multifamily_home",
      grepl("Quad|Townh|duplex", HOME_STYLE, ignore.case = TRUE) ~ "multifamily_home",
      grepl("Res 2-3|Double Bungalow|Duplex|Apartment|Low Income < 4 Units|Townh|Triplex", USE2_DESC, ignore.case = TRUE) ~ "multifamily_home",
      
      
      # Single-Family Homes
      grepl("Frame|Cabin|BUNGALOW|SPLIT|Rambler|Log", HOME_STYLE, ignore.case = TRUE) ~ "single_family_home",
      grepl("single|s.fam",DWELL_TYPE, ignore.case = TRUE) ~ "single_family_home",
      grepl("Res 1 unit|CABIN|Residential|Zero Lot Line", USE1_DESC, ignore.case = TRUE) ~ "single_family_home",
      grepl("Res 1 unit|CABIN|Residential|Zero Lot Line", USE2_DESC, ignore.case = TRUE) ~ "single_family_home",
      TRUE ~ NA)) %>% 
  
  ### COMM/IND/PUBLIC/AG
  mutate(
    mc_classification = case_when(
      # Commercial
      grepl("store",DWELL_TYPE, ignore.case = TRUE) ~ "commercial",
      
      DWELL_TYPE %in% c(
        "RETAIL STR", "RESTAURANT", "REST FSTFD", "BAR/TAVERN", "MARKET",
        "DISCNT STR",  "AUTO SHWRM", "AUTO CENTR", "POST OFFIC",
        "DAYCARECTR", "CLASSROOM", "THEATER", "BANK", "CARWASH", "BWLNGALLEY",
        "LAUNDROMAT", "MORTUARY", "COUNTRYCLB", "HAIRSALON", "CLUBHOUSE",
        "HEALTH CLB", "STABLE", "BED & BREAKFAST", 
        "PRKNG STRC", "OFC,MD/DTL", "OFFICE", "SHPCTR, COM", "SHPCTR,NBH",
        "SERVC GAR", "VET HSPTL", "CHURCH", "CONV STORE", "SERVC STN",
        "HOTEL", "MOTEL", "OFC,CORPTE","HOSPITALS","CREAMERY", "DEPT STORE"
      ) ~ "commercial",
      
      grepl("Commercial|COMM LAND & BLDGS|NON-PROFIT COMM|Com Ma & Pa|College|Skyways|Comm Services|SERVICE STATION|Golf Course|Condo|Cooperative|Restaurant|Marina|Arena|church", USE1_DESC, ignore.case = TRUE) ~ "commercial",
      grepl("Commercial|Arena|Charit", USE2_DESC, ignore.case = TRUE) ~ "commercial",
      grepl("church|Inst|Colleges|Private|Apprenticeship Training Facilities|hospitals", XUSE1_DESC, ignore.case = TRUE) ~ "commercial",
      
      # Industrial
      DWELL_TYPE %in% c(
        "INDL,MANFG", "MFG/PROCES", "INDUSTRIAL IMPROVED", "SHED,UTIL",
        "INDUSTRIAL VACANT", "GREENHOUSE", "UTILITIES", "UTIL,TELCM",
        "GARG/STRG","HANGAR/MTC", "SHED,EQUIP"
      ) ~ "industrial",
      grepl("Industrial|INDUSTIAL|Machinery|Utilit|Railroad|El Gen Mach|Utilities|INDS LAND & BLDGS|Pub Util", USE1_DESC, ignore.case = TRUE) ~ "industrial",
      grepl("Industrial", USE2_DESC, ignore.case = TRUE) ~ "industrial",
      grepl("railroad|airport", XUSE1_DESC, ignore.case = TRUE) ~ "industrial",
      
      ##public buildings
      grepl("School|Public|Municipal|County|State Property|FEDERAL|state", USE1_DESC, ignore.case = TRUE) ~ "public_building",
      grepl("Cities|state|hwy dept|county|waste control|township|federal property|public schools|Commission", XUSE1_DESC, ignore.case = TRUE) ~ "public_building",
      
      
      #agricultural
      grepl("Agricultural|Farm|AG|Green Acres|Preserve|Managed Forrest", USE1_DESC, ignore.case = TRUE) ~ "agriculture",
      grepl("Ag", USE2_DESC, ignore.case = TRUE) ~ "agriculture",
      grepl("farm", XUSE1_DESC, ignore.case = TRUE) ~ "agriculture",
      
      TRUE ~ mc_classification)) %>% 
  
  ### EMPTY LAND
  mutate(
    mc_classification = case_when(
      # Vacant/Exempt
      grepl("vacant|forfeit",DWELL_TYPE, ignore.case = TRUE) ~ "vacant",
      
      #open space
      grepl("Vacant|Res V Land|VAC LAND|Unimproved|Wetlands|Forest|HUNTING|Open Space", USE1_DESC, ignore.case = TRUE) ~ "no_building",
      grepl("Vacant|Wetlands|Common", USE2_DESC, ignore.case = TRUE) ~ "no_building",
      grepl("street|park|dnr|cemetary", XUSE1_DESC, ignore.case = TRUE) ~ "no_building",
      
      TRUE ~ mc_classification)
  )

mn_parcel_2005 %>% filter(is.na(mc_classification)) %>% count(USE1_DESC) %>% arrange(desc(n)) %>% print(n = 50)

mn_parcel_assigned_2005 <- mn_parcel_2005 %>%
  #remaining properties are EXEMPT - implying non-profit status, putting in commercial
  mutate(mc_classification = ifelse(is.na(mc_classification) & FIN_SQ_FT > 0,
                                    "commercial",
                                    mc_classification)) %>% 
  filter(!is.na(mc_classification))

mn_parcel_predict_2005 <- mn_parcel_assigned_2005 %>%
  # Calculate mc_classification averages by CTU_NAME and CO_NAME
  group_by(mc_classification, CITY) %>%
  mutate(
    mean_ctu_sqft = if_else(all(FIN_SQ_FT == 0, na.rm = TRUE), NA_real_, mean(FIN_SQ_FT[FIN_SQ_FT > 0], na.rm = TRUE)),
    mean_ctu_emv = if_else(all(EMV_BLDG == 0, na.rm = TRUE), NA_real_, mean(EMV_BLDG[EMV_BLDG > 0], na.rm = TRUE)),
    mean_ctu_year = if_else(all(YEAR_BUILT == 0, na.rm = TRUE), NA_real_, mean(YEAR_BUILT[YEAR_BUILT > 0], na.rm = TRUE))
  ) %>%
  ungroup() %>%
  group_by(mc_classification, COUNTY_ID) %>%
  mutate(
    mean_co_sqft = if_else(all(FIN_SQ_FT == 0, na.rm = TRUE), NA_real_, mean(FIN_SQ_FT[FIN_SQ_FT > 0], na.rm = TRUE)),
    mean_co_emv = if_else(all(EMV_BLDG == 0, na.rm = TRUE), NA_real_, mean(EMV_BLDG[EMV_BLDG > 0], na.rm = TRUE)),
    mean_co_year = if_else(all(YEAR_BUILT == 0, na.rm = TRUE), NA_real_, mean(YEAR_BUILT[YEAR_BUILT > 0], na.rm = TRUE))
  ) %>%
  ungroup() %>%
  # In-fill zeros using the calculated means
  mutate(
    FIN_SQ_FT = if_else(FIN_SQ_FT == 0, coalesce(mean_ctu_sqft, mean_co_sqft, FIN_SQ_FT), FIN_SQ_FT),
    EMV_BLDG = if_else(EMV_BLDG == 0, coalesce(mean_ctu_emv, mean_co_emv, EMV_BLDG), EMV_BLDG),
    YEAR_BUILT = if_else(YEAR_BUILT == 0, coalesce(mean_ctu_year, mean_co_year, YEAR_BUILT), YEAR_BUILT)
  ) %>%
  # select columns
  select(COUNTY_ID, CITY, FIN_SQ_FT, EMV_BLDG, YEAR_BUILT, mc_classification)

mn_parcel_map_2005 <- mn_parcel_predict_2005 %>% 
  group_by(COUNTY_ID,CITY, mc_classification) %>% 
  summarize(mean_sq_ft = mean(FIN_SQ_FT),
            total_sq_ft = sum(FIN_SQ_FT), 
            mean_emv = mean(EMV_BLDG),
            total_emv = sum(EMV_BLDG),
            mean_year = mean(YEAR_BUILT)) %>% 
  mutate(ctu_name = str_to_title(CITY),
         ctu_name = str_replace_all(ctu_name,"Twp", "Township"),
         ctu_name = str_replace_all(ctu_name,"St ", "Saint "),
         ctu_name = str_replace_all(ctu_name,"St.", "Saint"),
         ctu_name = str_replace_all(ctu_name," City", "")) %>% 
  left_join(cprg_ctu %>% 
              mutate(ctu_name = if_else(ctu_class == "TOWNSHIP",
                                        paste(ctu_name, "Township"),
                                        ctu_name)), 
            by = c("ctu_name" = "ctu_name")) %>%
  st_as_sf()

ggplot(mn_parcel_map_2005 %>% 
         filter(mc_classification == "single_family_home")) +
  geom_sf(aes(fill = mean_sq_ft), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", name = "Mean square foot (SFH)") +
  theme_minimal()

ggplot(mn_parcel_map_2005 %>% 
         filter(mc_classification == "single_family_home")) +
  geom_sf(aes(fill = mean_emv), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", name = "Mean house value (SFH)") +
  theme_minimal()

ggplot(mn_parcel_map_2005 %>% 
         filter(mc_classification == "single_family_home")) +
  geom_sf(aes(fill = mean_year), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", name = "Mean year of construction") +
  theme_minimal()

ggplot(mn_parcel_map_2005 %>% 
         filter(mc_classification == "single_family_home",
                COUNTY_ID  != "053"),
       aes(x = mean_sq_ft, y = mean_emv)) +
  geom_point() + theme_bw() +
  ggtitle("CTU: Mean square footage vs mean housing value")
