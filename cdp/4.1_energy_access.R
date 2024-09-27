
source("R/_load_pkgs.R")
minnesota_elecUtils_ActivityAndEmissions <- readRDS(file.path(here::here(),
                                                              "_energy/data/minnesota_elecUtils_ActivityAndEmissions.RDS"))

met_counties <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")

countyActivity <- minnesota_elecUtils_ActivityAndEmissions %>%
  filter(county %in% met_counties) %>% 
  group_by(county) %>%
  summarize(
    mWh_delivered_county = sum(mWh_delivered, na.rm = TRUE)
  ) %>%
  ungroup()%>% 
  summarize(mWh_delivered_region = sum(mWh_delivered_county))

mwh_total <- countyActivity$mWh_delivered_region

# Renewable energy can be defined as energy derived from sunlight, wind, 
# geothermal processes, biomass, and water (including hydropower, oceanic/tidal energy)
renewable_gridshare <- (0.9 + 34.6 + 0.0 + 0.8 + 4.4) /100
mwh_renewable <- mwh_total * renewable_gridshare
