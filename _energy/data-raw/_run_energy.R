# run all energy processing
# in specific order

# kerosene and propane
source("_energy/data-raw/propane-estimate-2021.R")
source("_energy/data-raw/kerosene-estimate-2021.R")
source("_energy/data-raw/fuel-estimate-2021.R")

# create spatial data with utility areas
source("_energy/data-raw/minnesota_natGasUtilities.R")
source("_energy/data-raw/minnesota_electricUtilities.R")

source("_energy/data-raw/wisconsin_natGasUtilities.R")
source("_energy/data-raw/wisconsin_electricUtilities.R")

# next, run processing for MN and WI activity data

source("_energy/data-raw/minnesota_natGas_ActivityAndEmissions.R")
source("_energy/data-raw/wisconsin_natGas_ActivityAndEmissions.R")

# finalize processing
source("_energy/data-raw/processed_mn_electricUtil_activityData.R")
source("_energy/data-raw/wisconsin_2005_2021_utilityProportions.R")

source("_energy/data-raw/nrel_slope_energy.R")

# Done!

nrel_slope_proportions <- readRDS("_energy/data-raw/nrel_slope/nrel_slope_proportions.RDS")


electric_raw <- readRDS(file.path(here::here(), "_energy/data/minnesota_county_ElecEmissions.RDS")) %>%
  bind_rows(readRDS(file.path(here::here(), "_energy/data/wisconsin_county_ElecEmissions.RDS")) %>%
    rename(county = county_name))

natgas_raw <- readRDS(file.path(here::here(), "_energy/data/minnesota_county_GasEmissions.RDS")) %>%
  bind_rows(readRDS(file.path(here::here(), "_energy/data/wisconsin_county_GasEmissions.RDS")) %>%
    rename(county = county_name))

electric_natgas_nrel_proportioned <- electric_raw %>%
  bind_rows(natgas_raw) %>%
  select(county, source = sector, year, emissions_metric_tons_co2e) %>%
  mutate(source = str_to_sentence(source)) %>%
  left_join(nrel_slope_proportions,
    by = join_by(county, source, year)
  ) %>%
  mutate(
    commercial = commercial * emissions_metric_tons_co2e,
    industrial = industrial * emissions_metric_tons_co2e,
    residential = residential * emissions_metric_tons_co2e
  ) %>%
  rename(total = emissions_metric_tons_co2e) %>%
  pivot_longer(4:7,
    names_to = "category",
    values_to = "emissions_metric_tons_co2e"
  ) %>%
  mutate(category = str_to_sentence(category))

waldo::compare(electric_natgas_nrel_proportioned, readRDS("_energy/data/electric_natgas_nrel_proportioned.RDS"))
saveRDS(electric_natgas_nrel_proportioned, "_energy/data/electric_natgas_nrel_proportioned.RDS")
