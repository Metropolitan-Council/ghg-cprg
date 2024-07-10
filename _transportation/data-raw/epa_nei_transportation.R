# get data from the 2020 national emissions inventory
source("R/_load_pkgs.R")
source("R/global_warming_potential.R")
library(httr2)

cprg_county <- readRDS("_meta/data/cprg_county.RDS")
epa_moves <- readRDS("_transportation/data/epa_moves.RDS")

source("_meta/data-raw/epa_nei.R")

# mobile sectors only
mobile_sectors <- sectors %>%
  filter(sector_one == "Mobile")

nei_state_emissions <- nei_state_multi_year %>%
  filter(
    sector_code %in% mobile_sectors$sector_code,
    pollutant_type == "GHG",
    sector_two == "On-Road"
  ) %>%
  rowwise() %>%
  mutate(
    vehicle_weight_label = case_when(
      ei_sector %in% c(
        "Mobile - On-Road Diesel Light Duty Vehicles",
        "Mobile - On-Road non-Diesel Light Duty Vehicles"
      ) ~ "Light-duty",
      ei_sector %in% c(
        "Mobile - On-Road non-Diesel Heavy Duty Vehicles",
        "Mobile - On-Road Diesel Heavy Duty Vehicles"
      ) ~ "Heavy-duty"
    ) %>%
      factor(
        levels = c(
          "Light-duty",
          "Medium-duty",
          "Heavy-duty"
        ),
        ordered = TRUE
      )
  ) %>%
  mutate(emissions_grams = emissions %>%
    units::as_units("ton") %>% # short tons/US tons
    units::set_units("gram") %>% # convert to grams
    as.numeric()) %>%
  unique() %>%
  select(
    vehicle_weight_label,
    state_name,
    nei_inventory_year = inventory_year,
    pollutant_code, emissions_grams
  ) %>%
  unique() %>%
  group_by(state_name, nei_inventory_year, vehicle_weight_label, pollutant_code) %>%
  summarize(emissions_grams = sum(emissions_grams)) %>%
  pivot_wider(
    names_from = pollutant_code,
    values_from = emissions_grams
  ) %>%
  clean_names() %>%
  ungroup() %>%
  rowwise() %>%
  # n2o and ch4 to co2 equivalency
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o)),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  )




# combine MN and WI
# filter to only needed datasets
nei_county <- nei_county_multi_year %>%
  left_join(cprg_county, by = c("county_fips" = "COUNTYFP")) %>%
  filter(
    sector_code %in% mobile_sectors$sector_code,
    pollutant_type == "GHG"
  ) %>%
  filter(sector_two == "On-Road") %>%
  rowwise() %>%
  mutate(
    county_name = NAME,
    vehicle_weight_label = case_when(
      ei_sector %in% c(
        "Mobile - On-Road Diesel Light Duty Vehicles",
        "Mobile - On-Road non-Diesel Light Duty Vehicles"
      ) ~ "Light-duty",
      ei_sector %in% c(
        "Mobile - On-Road non-Diesel Heavy Duty Vehicles",
        "Mobile - On-Road Diesel Heavy Duty Vehicles"
      ) ~ "Heavy-duty"
    ) %>%
      factor(
        levels = c(
          "Light-duty",
          "Medium-duty",
          "Heavy-duty"
        ),
        ordered = TRUE
      )
  )

# check unit of measurement
# https://www.epa.gov/air-emissions-inventories/what-are-units-nei-emissions-data
nei_county_emissisons <- nei_county %>%
  mutate(emissions_grams = emissions %>%
    units::as_units("ton") %>% # short tons/US tons
    units::set_units("gram") %>% # convert to grams
    as.numeric()) %>%
  select(
    ei_sector, vehicle_weight_label,
    county_name, county_fips,
    nei_inventory_year = inventory_year,
    pollutant_code, emissions_grams
  ) %>%
  pivot_wider(
    names_from = pollutant_code,
    values_from = emissions_grams
  ) %>%
  clean_names() %>%
  rowwise() %>%
  # n2o and ch4 to co2 equivalency
  mutate(
    co2_co2_equivalent =
      sum(co2, (ch4 * gwp$ch4), (n2o * gwp$n2o)),
    emissions_metric_tons_co2e = co2_co2_equivalent / 1000000
  )

# aggregate by vehicle weight and county
epa_nei <- nei_county_emissisons %>%
  group_by(vehicle_weight_label, county_name, county_fips, nei_inventory_year) %>%
  summarize(
    total_co2 = sum(co2),
    total_ch4 = sum(ch4),
    total_n2o = sum(n2o),
    total_co2_w_equiv = sum(co2_co2_equivalent),
    emissions_metric_tons_co2e = sum(emissions_metric_tons_co2e),
    .groups = "keep"
  )


epa_nei_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "vehicle_weight_label", class(epa_nei$vehicle_weight_label), "\"Light-duty\", \"Medium-duty\", or \"Heavy-duty\"",
  "county_name", class(epa_nei$county_name), "County name",
  "county_fips", class(epa_nei$county_fips), "County FIPS",
  "nei_inventory_year", class(epa_nei$nei_inventory_year), "NEI inventory year",
  "total_co2", class(epa_nei$total_co2), "Annual total grams of CO~2~  attributed to the given county",
  "total_ch4", class(epa_nei$total_ch4), "Annual total grams of CH~4~  attributed to the given county",
  "total_n2o", class(epa_nei$total_n2o), "Annual total grams of N~2~O  attributed to the given county",
  "total_co2_w_equiv", class(epa_nei$total_co2_w_equiv), "Annual total grams of CO~2~ and CO~2~ equivalent attributed to the given county",
  "emissions_metric_tons_co2e", class(epa_nei$emissions_metric_tons_co2e), "Annual total metric tons CO~2~ and CO~2~ equivalent attributed to the given county"
)


waldo::compare(epa_nei, readRDS("_transportation/data/epa_nei.RDS"))
waldo::compare(epa_nei_meta, readRDS("_transportation/data/epa_nei_meta.RDS"))


saveRDS(epa_nei, "_transportation/data/epa_nei.RDS")
saveRDS(epa_nei_meta, "_transportation/data/epa_nei_meta.RDS")
