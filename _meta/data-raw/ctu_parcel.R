# Script to import ancillary housing data from MN geospatial commons
source("R/_load_pkgs.R")

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

mn_parcel <- mn_parcel %>% select(-mc_classification)

mn_parcel %>% distinct(DWELL_TYPE) %>% arrange() %>% 
  print(n=200) # approximately 1/2 assigned

mn_parcel <- mn_parcel %>%
  mutate(
    mc_classification = case_when(
      # Single-Family Homes
      DWELL_TYPE %in% c(
        "Single-Family / Owner Occupied", "S.FAM.RES", "SINGLE FAMILY DWELLING, PLATT", 
        "RESIDENTIAL, SINGLE FAMILY", "SINGLE FAMILY W/ACCESSORY UNI","RESIDENTIAL, OTHER"
      ) ~ "single_family_home",
      
      # multifamily
      grepl("townh",DWELL_TYPE, ignore.case = TRUE) ~ "multifamily_home",
      
      DWELL_TYPE %in% c(
       "Two-Family Duplex", 
        "DUPLEX", "Two-Family Conversion", "TWIN HOME",
        "TWO FAMILY DWELLING - SIDE/SI", "TWO FAMILY DWELLING - UP/DWN",
        "Three-Family Conversion", "TRIPLEX", "THREE FAMILY DWELLING, PLATTE",
        "MULTI RES", "TWO RESIDENCES ON ONE PARCEL"
      ) ~ "multifamily_home",

      # Apartments
      grepl("apartment|condo|apt",DWELL_TYPE, ignore.case = TRUE) ~ "apartment",
      
      DWELL_TYPE %in% c(
        "ASTD LIVNG", "NURSING HM", "NURSING HOMES", 
        "HMS ELDRY", "NURSING HOME & PRIVATE HOSPIT"
      ) ~ "apartment",
      
      # Mobile Homes
      DWELL_TYPE %in% c(
        "Mobile Home Housing", "MOBILE HOMES", "MOBILE HOME PARKS"
      ) ~ "manufactured_home",
      

      # Commercial
      grepl("store",DWELL_TYPE, ignore.case = TRUE) ~ "commercial",
      
      DWELL_TYPE %in% c(
        "RETAIL STR", "RESTAURANT", "REST FSTFD", "BAR/TAVERN", "MARKET", 
        "DISCNT STR",  "AUTO SHWRM", "AUTO CENTR", "POST OFFIC",
        "DAYCARECTR", "CLASSROOM", "THEATER", "BANK", "CARWASH", "BWLNGALLEY",
        "LAUNDROMAT", "MORTUARY", "COUNTRYCLB", "HAIRSALON", "CLUBHOUSE", 
        "HEALTH CLB", "STABLE", "BED & BREAKFAST", "FRATERNITY/SORORITY HOUSE",
        "PRKNG STRC", "OFC,MD/DTL", "OFFICE", "SHPCTR, COM", "SHPCTR,NBH",
        "SERVC GAR", "VET HSPTL", "CHURCH", "CONV STORE", "SERVC STN",
        "HOTEL", "MOTEL", "OFC,CORPTE","HOSPITALS","CREAMERY", "DEPT STORE"
      ) ~ "Commercial",
      
      # Industrial
      DWELL_TYPE %in% c(
        "INDL,MANFG", "MFG/PROCES", "INDUSTRIAL IMPROVED", "SHED,UTIL",
        "INDUSTRIAL VACANT", "GREENHOUSE", "UTILITIES", "UTIL,TELCM",
        "GARG/STRG","HANGAR/MTC", "SHED,EQUIP"
      ) ~ "Industrial",
      
      # Vacant/Exempt
      DWELL_TYPE %in% c(
        "RESIDENTIAL, VACANT LAND, LOT", "VACANT PROPERTY", 
        "CONDO VACANT LAND", "EXEMPT PROPERTY", "FORFEIT PROPERTY"
      ) ~ "vacant",
      
      # Other
      TRUE ~ NA
    )
  )

mn_parcel %>% filter(is.na(mc_classification)) %>% distinct(USECLASS1) %>% arrange() %>% 
  print(n=200) # approximately 1/2 assigned

mn_parcel <- mn_parcel %>%
  mutate(mc_classification = if_else(
    is.na(mc_classification), 
    case_when(
      grepl("Res 1 unit|CABIN|Residential|Zero Lot Line", USECLASS1, ignore.case = TRUE) ~ "single_family_home",
      grepl("Res 2-3|Double Bungalow|Duplex|Apartment|Low Income < 4 Units|Townh|Triplex", USECLASS1, ignore.case = TRUE) ~ "multifamily_home",
      grepl("Apartment|APARMENT|Apt|Elderly Liv Fac|Housing - Low Income > 3 Units|HRA|Nursing|Sr Citizens", USECLASS1, ignore.case = TRUE) ~ "apartment",
      grepl("manufactured|MH", USECLASS1, ignore.case = TRUE) ~ "manufactured",
      grepl("Commercial|NON-PROFIT COMM|Com Ma & Pa|College|Skyways|Comm Services|SERVICE STATION|Golf Course|Condo|Cooperative|Restaurant|Marina|Arena|church", USECLASS1, ignore.case = TRUE) ~ "commercial",
      grepl("Industrial|INDUSTIAL|Machinery|Utilit|Railroad|El Gen Mach|Utilities|Pub Util", USECLASS1, ignore.case = TRUE) ~ "industrial",
      grepl("School|Hospital|Public|Municipal|County|State Property|Charit Inst|Cemetery|FEDERAL|state", USECLASS1, ignore.case = TRUE) ~ "public_building",
      grepl("Vacant|Res V Land|VAC LAND|Unimproved|Vacant Land|Wetlands|Forest|InLieuTx|HUNTING|Rural Vacant Land|Open Space", USECLASS1, ignore.case = TRUE) ~ "no_building",
      grepl("Agricultural|Farm|AG|Green Acres|Preserve|Managed Forrest", USECLASS1, ignore.case = TRUE) ~ "agriculture",
      TRUE ~ mc_classification
    ),
    mc_classification # If mc_classification is not NA, retain the existing value
  ))

mn_parcel %>% filter(is.na(mc_classification)) %>% distinct(USECLASS2) %>% print (n = 100)

mn_parcel <- mn_parcel %>%
  mutate(mc_classification = if_else(
    is.na(mc_classification), 
    case_when(
      grepl("Res 1 unit|CABIN|Residential|Zero Lot Line", USECLASS2, ignore.case = TRUE) ~ "single_family_home",
      grepl("Res 2-3|Double Bungalow|Duplex|Apartment|Low Income < 4 Units|Townh|Triplex", USECLASS2, ignore.case = TRUE) ~ "multifamily_home",
      grepl("Apartment|APARMENT|Apt|Elderly Liv Fac|Housing - Low Income > 3 Units|HRA|Nursing|Sr Citizens", USECLASS2, ignore.case = TRUE) ~ "apartment",
      grepl("manufactured|MH", USECLASS2, ignore.case = TRUE) ~ "manufactured",
      grepl("Commercial|NON-PROFIT COMM|Com Ma & Pa|College|Skyways|Comm Services|SERVICE STATION|Golf Course|Condo|Cooperative|Restaurant|Marina|Arena|church", USECLASS2, ignore.case = TRUE) ~ "commercial",
      grepl("Industrial|INDUSTIAL|Machinery|Utilit|Railroad|El Gen Mach|Utilities|Pub Util", USECLASS2, ignore.case = TRUE) ~ "industrial",
      grepl("School|Hospital|Public|Municipal|County|State Property|Charit Inst|Cemetery|FEDERAL|state", USECLASS2, ignore.case = TRUE) ~ "public_building",
      grepl("Vacant|Res V Land|VAC LAND|Unimproved|Vacant Land|Wetlands|Forest|InLieuTx|HUNTING|Rural Vacant Land|Open Space", USECLASS2, ignore.case = TRUE) ~ "no_building",
      grepl("Agricultural|Farm|AG|Green Acres|Preserve|Managed Forrest", USECLASS2, ignore.case = TRUE) ~ "agriculture",
      TRUE ~ mc_classification
    ),
    mc_classification # If mc_classification is not NA, retain the existing value
  ))

mn_parcel %>% filter(is.na(mc_classification)) %>% distinct(HOME_STYLE) %>% print (n = 100)

mn_parcel <- mn_parcel %>%
  mutate(mc_classification = if_else(
    is.na(mc_classification), 
    case_when(
      grepl("Story|Frame|Cabin|BUNGALOW|SPLIT|Rambler|Log", HOME_STYLE, ignore.case = TRUE) ~ "single_family_home",
      grepl("Quad|Townhome", HOME_STYLE, ignore.case = TRUE) ~ "multifamily_home",
      grepl("APT", HOME_STYLE, ignore.case = TRUE) ~ "apartment",
      grepl("manufactured|MH", HOME_STYLE, ignore.case = TRUE) ~ "manufactured",
      TRUE ~ mc_classification
    ),
    mc_classification # If mc_classification is not NA, retain the existing value
  ))

mn_parcel %>% filter(is.na(mc_classification)) %>% count(XUSECLASS1) %>%
  arrange(desc(n)) %>% print(n= 100)

exempt <- mn_parcel %>% filter(USECLASS1 == "EXEMPT")

mn_parcel %>% filter(is.na(USECLASS1)) ### 314,577 / 3,002,804 ~ 10%
mn_parcel %>% filter(is.na(mc_classification)) ### 2,986,778 / 3,002,804 ~ 99%
mn_parcel %>% filter(is.na(XUSE1_DESC)) ### 2,923,951 / 3,002,804 ~ 99%
mn_parcel %>% filter(is.na(DWELL_TYPE)) ### 1,898,223 / 3,002,804 ~ 60%
mn_parcel %>% filter(is.na(HOME_STYLE)) ### 1,644,809 / 3,002,804 ~ 50%
mn_parcel %>% filter(FIN_SQ_FT == 0) ### 1,725,695 / 3,002,804 ~ 55%
mn_parcel %>% filter(NUM_UNITS  == 0) ### 517,604 / 3,002,804 ~ 15%

no_use <- mn_parcel %>% filter(is.na(USECLASS1))
no_sqft <- mn_parcel %>% filter(FIN_SQ_FT == 0)

%>%
  mutate(
    STATEFP = "27",
    STATE = "Minnesota",
    STATE_ABB = "MN"
  ) %>%
  select(
    CTU_NAME = FEATURE_NA,
    CTU_CLASS,
    COUNTY_NAM,
    STATEFP,
    STATE,
    STATE_ABB,
    GNIS_FEATU,
    geometry = geom
  ) %>%
  arrange(CTU_NAME) %>%
  clean_names()
