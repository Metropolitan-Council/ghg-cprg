rm(list = ls())
source("R/_load_pkgs.R")

overwrite_RDS <- TRUE

nlcd_county <- readRDS("./_nature/data/nlcd_county_landcover_allyrs.rds")
nlcd_ctu <- readRDS("./_nature/data/nlcd_ctu_landcover_allyrs.rds")
land_cover_c <- readRDS("./_nature/data/land_cover_carbon.rds")


# Compute C sequestration and stock potential for natural systems sectors by county
nlcd_county_c <- nlcd_county %>%
  as_tibble() %>%
  dplyr::select(-total_area) %>%
  left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
  mutate(
    data_source = if_else(source == "extrapolated",
      "Extrapolated from NLCD",
      "National Land Cover Database"
    ),
    source = land_cover_type
  ) %>%
  filter(seq_mtco2e_sqkm < 0) %>%
  mutate(
    sequestration_potential = area * seq_mtco2e_sqkm,
    stock_potential = area * stock_mtco2e_sqkm
  ) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm)) %>%
  mutate(
    sector = "Natural Systems",
    category = "Sequestration"
  ) %>%
  dplyr::select(
    county_id, county_name, state_name,
    inventory_year, sector, category, source, data_source,
    area, sequestration_potential, stock_potential
  ) %>%
  arrange(inventory_year, county_name, source)


# Compute C sequestration and stock potential for natural systems sectors by CTU
nlcd_ctu_c <- nlcd_ctu %>%
  as_tibble() %>%
  dplyr::select(-total_area) %>%
  left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
  mutate(
    data_source = if_else(source == "extrapolated",
      "Extrapolated from NLCD",
      "National Land Cover Database"
    ),
    source = land_cover_type
  ) %>%
  filter(seq_mtco2e_sqkm < 0) %>%
  mutate(
    sequestration_potential = area * seq_mtco2e_sqkm,
    stock_potential = area * stock_mtco2e_sqkm
  ) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm)) %>%
  mutate(
    sector = "Natural Systems",
    category = "Sequestration"
  ) %>%
  dplyr::select(
    county_id, ctu_id, ctu_name, ctu_class, county_name, state_name,
    inventory_year, sector, category, source, data_source,
    area, sequestration_potential, stock_potential
  ) %>%
  arrange(inventory_year, ctu_name, source)





nlcd_county_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county_id", class(nlcd_county_c$county_id), "County ID (5 digit)",
    "county_name", class(nlcd_county_c$county_name), "County name",
    "state_name", class(nlcd_county_c$state_name), "State name",
    "inventory_year", class(nlcd_county_c$inventory_year), "Year of survey",
    "sector", class(nlcd_county_c$sector), "Emissions sector. One of Transportation, Energy, Waste, Nature, Agriculture",
    "category", class(nlcd_county_c$category), "Category of emissions within given sector",
    "source", class(nlcd_county_c$source), "Source of sequestration from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "data_source", class(nlcd_county_c$data_source), "Land cover data source",
    "area", class(nlcd_county_c$area), "Area of land cover in square kilometers",
    "sequestration_potential", class(nlcd_county_c$sequestration_potential), "Carbon sequestration potential of county land cover type in metric tons of CO2e per year",
    "stock_potential", class(nlcd_county_c$stock_potential), "Carbon stock potential of county land cover type in metric tons of CO2e"
  )

nlcd_ctu_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county_id", class(nlcd_ctu_c$county_id), "County ID (5 digit)",
    "ctu_id", class(nlcd_ctu_c$ctu_id), "CTU ID",
    "ctu_name", class(nlcd_ctu_c$ctu_name), "CTU name",
    "ctu_class", class(nlcd_ctu_c$county_name), "CTU class",
    "county_name", class(nlcd_ctu_c$county_name), "County name",
    "state_name", class(nlcd_ctu_c$state_name), "State name",
    "inventory_year", class(nlcd_ctu_c$inventory_year), "Year of survey",
    "sector", class(nlcd_ctu_c$sector), "Emissions sector. One of Transportation, Energy, Waste, Nature, Agriculture",
    "category", class(nlcd_ctu_c$category), "Category of emissions within given sector",
    "source", class(nlcd_ctu_c$source), "Source of sequestration from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "data_source", class(nlcd_ctu_c$data_source), "Land cover data source",
    "area", class(nlcd_ctu_c$area), "Area of land cover in square kilometers",
    "sequestration_potential", class(nlcd_ctu_c$sequestration_potential), "Carbon sequestration potential of county land cover type in metric tons of CO2e per year",
    "stock_potential", class(nlcd_ctu_c$stock_potential), "Carbon stock potential of county land cover type in metric tons of CO2e"
  )




# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  saveRDS(nlcd_county_c, paste0("./_nature/data/nlcd_county_landcover_sequestration_allyrs.rds"))
  saveRDS(nlcd_county_c_meta, paste0("./_nature/data/nlcd_county_landcover_sequestration_allyrs_meta.rds"))

  saveRDS(nlcd_ctu_c, paste0("./_nature/data/nlcd_ctu_landcover_sequestration_allyrs.rds"))
  saveRDS(nlcd_ctu_c_meta, paste0("./_nature/data/nlcd_ctu_landcover_sequestration_allyrs_meta.rds"))
}
