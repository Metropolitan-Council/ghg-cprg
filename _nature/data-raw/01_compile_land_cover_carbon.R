rm(list = ls())
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

overwrite_RDS <- TRUE


seq_rates <- read_csv("./_nature/data-raw/land_cover_seq_rates.csv")
stock <- read_csv("./_nature/data-raw/land_cover_stock.csv")

# sequestration rates require some recalculation given varying units.
# Further, urban grasslands (usually turf grass) have a C sequestration rate but also are expected emitters of N20
seq_rates_sum <- seq_rates %>%
  mutate(
    mtco2e =
      case_when(
        # scale units carbon to CO2e (atomic weight of a carbon atom compared to CO2 molecule)
        units == "metric ton C" ~ activity * 3.67,
        # us ton to metric ton CO2e
        units == "US ton C" ~ activity * 3.67 * 0.907185,
        # IPCC 2019 assumes 1% of nitrogen fertilizer volatilizes to N20
        units == "kg N" ~ activity * 0.01 * gwp$n2o * 0.001,
        TRUE ~ activity
      )
  ) %>%
  ##### convert all seq rates to Mt per 1 sq km
  mutate(sqkm = case_when(
    area_unit == "ha" ~ area / 100,
    area_unit == "acre" ~ area / 247.1,
    TRUE ~ NA
  )) %>%
  mutate(mtco2e_sqkm = mtco2e / sqkm) %>%
  filter(
    !activity == 0.25, #  remove hidden carbon cost calculation as it seems to underestimate emissions
    !grepl("Polasky", Citation),
    !activity == -0.935 # remove the 30-yr estimate of urban grassland sequestration
  ) %>% # removing high wetland seq estimate
  group_by(land_cover_type) %>%
  summarize(seq_mtco2e_sqkm = sum(mtco2e_sqkm))

# find the mean stock estimates with standardized units
stock_mean <- stock %>%
  mutate(
    mtco2e = case_when(
      # scale units carbon to CO2e (atomic weight of a carbon atom compared to CO2 molecule)
      units == "metric tons C" ~ activity * 3.67,
      # kg carbon to metric ton CO2e
      units == "kg C" ~ activity * 3.67 * 0.001,
      TRUE ~ activity
    )
  ) %>%
  ##### convert all seq rates to Mt per 1 sq km
  mutate(sqkm = case_when(
    area_unit == "ha" ~ area / 100,
    area_unit == "m" ~ area * 1e-6,
    TRUE ~ NA
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

# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  saveRDS(land_cover_c, "./_nature/data/land_cover_carbon.rds")
  saveRDS(land_cover_c_meta, "./_nature/data/land_cover_carbon_meta.rds")
}

message("Done!")
