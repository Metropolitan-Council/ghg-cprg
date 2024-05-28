source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

seq_rates <- read_csv("./_nature/data-raw/land_cover_seq_rates.csv")
stock <- read_csv("./_nature/data-raw/land_cover_stock.csv")

# sequestration rates require some recalculation given varying units.
# Further, urban grasslands (usually turf grass) have a C sequestration rate but also are expected emitters of N20
seq_rates_sum <- mutate(seq_rates, mtco2e = if_else(units == "metric ton C", activity * 3.67, # scale units carbon to CO2e (atomic weight of a carbon atom compared to CO2 molecule)
  if_else(units == "US ton C", activity * 3.67 * 0.907185, # us ton to metric ton CO2e
    if_else(units == "kg N", activity * 0.01 * gwp$n2o * 0.001, # IPCC 2019 assumes 1% of nitrogen fertilizer volatilizes to N20
      activity
    )
  )
)) %>%
  ##### convert all seq rates to Mt per 1 sq km
  mutate(sqkm = if_else(area_unit == "ha", area / 100,
    if_else(area_unit == "acre", area / 247.1, NA)
  )) %>%
  mutate(mtco2e_sqkm = mtco2e / sqkm) %>%
  filter(!activity == 0.25) %>% # remove hidden carbon cost calculation as it seems to underestimate emissions
  group_by(land_cover_type) %>%
  summarize(seq_mtco2e_sqkm = sum(mtco2e_sqkm))

# find the mean stock estimates with standardized units
stock_mean <- mutate(stock, mtco2e = if_else(units == "metric tons C", activity * 3.67, # scale units carbon to CO2e (atomic weight of a carbon atom compared to CO2 molecule)
  if_else(units == "kg C", activity * 3.67 * 0.001, # kg carbon to metric ton CO2e
    activity
  )
)) %>%
  ##### convert all seq rates to Mt per 1 sq km
  mutate(sqkm = if_else(area_unit == "ha", area / 100,
    if_else(area_unit == "m", area * 1e-6, NA)
  )) %>%
  mutate(mtco2e_sqkm = mtco2e / sqkm) %>%
  group_by(land_cover_type) %>%
  summarize(stock_mtco2e_sqkm = -mean(mtco2e_sqkm)) # take the mean values for cover types with multiple, call it negative for messaging


land_cover_c <- left_join(seq_rates_sum, stock_mean)

land_cover_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "land_cover_type", class(land_cover_c$land_cover_type), "Land cover type from World Cover. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "Sequestration rate", class(land_cover_c$seq_mtco2e_sqkm), "Carbon sequestration potential of land cover type in metric tons CO2 per square kilometer per year",
    "Stock potential", class(land_cover_c$stock_mtco2e_sqkm), "Total carbon stock potential of land cover type in metric tons CO2 equivalency per square kilometer"
  )

saveRDS(land_cover_c, "./_nature/data/land_cover_carbon.rds")
saveRDS(land_cover_c_meta, "./_nature/data/land_cover_carbon_meta.rds")
