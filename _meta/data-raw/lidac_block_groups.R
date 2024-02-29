## Some header info
# Clear old vars
rm(list = ls())


## Start here:
source(file.path(here::here(), "R/_load_pkgs.R"))
source(file.path(here::here(), "R/_quarto_helpers.R"))
source(file.path(here::here(), "R/plot_county_emissions.R"))



# LOAD DATA ---------------------------------------------------------------
# Load in list of LIDACs
LIDACs <- readr::read_csv("_meta/data-raw/IRA_LIDAC_block_group.csv",
  name_repair = "universal"
)


# Load the EJ screening dataset (EPA Environmental Justice Screening and Mapping Tool)
# This is a csv of the national EJScreen dataset at the block group level
EJSCREEN <- readr::read_csv("_meta/data-raw/EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.csv",
  name_repair = "universal"
)
data.table::setDT(EJSCREEN)


# Read in CEJST data (Climate and Economic Justice Screening Tool)
# Tool highlights disadvantaged census tracts across the United States
CEJST <- readr::read_csv("_meta/data-raw/1.0-communities.csv",
                         name_repair = "universal"
)
data.table::setDT(CEJST)


# Get county data for mapping, subset to PCAP region 
county_data<- tigris::counties(c("Minnesota", "Wisconsin"))


# FILTER DATA -------------------------------------------------------------
data.table::setDT(CEJST)
# Subset to PCAP region
CEJST <- CEJST[State.Territory == "Minnesota" & County.Name == "Anoka County" |
                   State.Territory == "Minnesota" & County.Name == "Carver County" |
                   State.Territory == "Minnesota" & County.Name == "Chisago County" |
                   State.Territory == "Minnesota" & County.Name == "Dakota County" |
                   State.Territory == "Minnesota" & County.Name == "Hennepin County"|
                   State.Territory == "Minnesota" & County.Name == "Ramsey County" |
                   State.Territory == "Minnesota" & County.Name == "Scott County" |
                   State.Territory == "Minnesota" & County.Name == "Sherburne County" |
                   State.Territory == "Minnesota" & County.Name == "Washington County" |
                   State.Territory == "Wisconsin" & County.Name == "Pierce County" |
                   State.Territory == "Wisconsin" & County.Name == "St. Croix County"]


# data.table::setDT(EJSCREEN)
# Subset to PCAP jurisdiction
EJSCREEN <- EJSCREEN[STATE_NAME == "Minnesota" & CNTY_NAME == "Anoka County" |
                         STATE_NAME == "Minnesota" & CNTY_NAME == "Carver County" |
                         STATE_NAME == "Minnesota" & CNTY_NAME == "Chisago County" |
                         STATE_NAME == "Minnesota" & CNTY_NAME == "Dakota County" |
                         STATE_NAME == "Minnesota" & CNTY_NAME == "Hennepin County"|
                         STATE_NAME == "Minnesota" & CNTY_NAME == "Ramsey County" |
                         STATE_NAME == "Minnesota" & CNTY_NAME == "Scott County" |
                         STATE_NAME == "Minnesota" & CNTY_NAME == "Sherburne County" |
                         STATE_NAME == "Minnesota" & CNTY_NAME == "Washington County" |
                         STATE_NAME == "Wisconsin" & CNTY_NAME == "Pierce County" |
                         STATE_NAME == "Wisconsin" & CNTY_NAME == "St. Croix County"]


# Subset county boundaries to PCAP region
county_data <- subset(county_data, NAME =="Anoka" |
                        NAME == "Carver" |
                        NAME == "Chisago"|
                        NAME == "Dakota"|
                        NAME == "Hennepin"|
                        NAME == "Ramsey"|
                        NAME == "Scott"|
                        NAME == "Sherburne"|
                        NAME == "Washington" & STATEFP == "27"|
                        NAME == "Pierce"|
                        NAME == "St. Croix")







# JOIN DATA ---------------------------------------------------------------
# Get tract geometry (CEJST data use 2010 census geographies)
MN_tracts <- tigris::tracts(state = "MN", year = "2010")
WI_tracts <- tigris::tracts(state = "WI", year = "2010")
tracts <- rbind(MN_tracts, WI_tracts)

# Make sure the GEOID column header is consistent across your datasets 
colnames(CEJST)[1] <- "GEOID"
CEJST$GEOID <- as.character(CEJST$GEOID)
colnames(tracts)[4] <- "GEOID"

# Join your CEJST data to the tracts for MN and WI
CEJST_map_data <- inner_join(tracts, CEJST, by="GEOID")

# Get block group geometry
MN_block_groups <- tigris::block_groups(state = "MN")
WI_block_groups <- tigris::block_groups(state = "WI")
block_groups <- rbind(MN_block_groups, WI_block_groups)

# rename ID in EJSCREEN data to "GEOID", convert to character for join
colnames(EJSCREEN)[2] <- "GEOID"
EJSCREEN$GEOID <- as.character(EJSCREEN$GEOID)
LIDACs$GEOID <- as.character(LIDACs$GEOID)

# join block group data to EJ Screen data
LIDAC_polygons <- inner_join(block_groups, EJSCREEN, by = "GEOID")

# join LIDACs to map data
LIDAC_polygons <- LIDAC_polygons %>% left_join(LIDACs)

#replace NAs with No, make LIDAC indicator
LIDAC_polygons$GEOID_tract[is.na(LIDAC_polygons$GEOID_tract)] <- 0
LIDAC_polygons$LIDAC <- ifelse(LIDAC_polygons$GEOID_tract == "0", "No", "Yes")

# if you wanted to dissolve LIDAC geometries: 
LIDAC_polygons_dissolved <- LIDAC_polygons %>%
  dplyr::group_by(LIDAC) %>%
  summarize(do_union = TRUE)





CEJST_map_data
LIDAC_polygons


# saveRDS(LIDAC_polygons, "_meta/data/lidac_block_groups.RDS")
# saveRDS(CEJST_map_data, "_meta/data/ejscreen_block_groups.RDS")




