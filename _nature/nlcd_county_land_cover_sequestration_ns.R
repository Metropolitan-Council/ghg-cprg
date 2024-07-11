# rm(list=ls())
source("R/_load_pkgs.R")

nlcd_county <- readRDS("./_nature/data/nlcd_county_landcover_2001_2021.rds")
land_cover_c <- readRDS("./_nature/data/land_cover_carbon.rds")

# Compute C sequestration and stock potential for natural systems sectors
nlcd_county_c <- nlcd_county %>%
  as_tibble() %>%
  left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
  filter(seq_mtco2e_sqkm < 0) %>%
  mutate(
    sequestration_potential = area * seq_mtco2e_sqkm,
    stock_potential = area * stock_mtco2e_sqkm
  ) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm)) %>%
  left_join( # this creates an empty grid of all desired year,livestock combinations
    x = expand.grid(
      year = seq(2001, 2021, by = 1),
      county = unique(nlcd_county$county) %>% sort(),
      land_cover_type = unique(land_cover_c$land_cover_type)
    ),
    y = ., by = join_by(year, county, land_cover_type)
  ) %>%
  mutate(source = ifelse(is.na(area), "est", "nlcd")) %>%
  group_by(county, land_cover_type) %>%
  arrange(year) %>%
  mutate(
    sequestration_potential2 = zoo::na.approx(sequestration_potential, na.rm = FALSE),
    stock_potential2 = zoo::na.approx(stock_potential, na.rm = FALSE),
    sequestration_potential3 = zoo::na.approx(sequestration_potential, na.rm = FALSE, maxgap = Inf, rule = 2),
    stock_potential3 = zoo::na.approx(stock_potential, na.rm = FALSE, maxgap = Inf, rule = 2),
    source = case_when(
      source == "est" & !is.na(sequestration_potential2) ~ "interpolated",
      source == "est" & !is.na(sequestration_potential3) ~ "extrapolated",
      source == "nlcd" ~ "nlcd"
    ),
    sequestration_potential = sequestration_potential3,
    stock_potential = stock_potential3
  ) %>%
  dplyr::select(year, county, land_cover_type, area, sequestration_potential, stock_potential, source) %>%

  arrange(year, county, land_cover_type) %>%
  # Now that we've extrapolated sequestration/stock values despite having no area estimates,
  # we need to calculate area based on the extrapolated sequestration.
  left_join(., land_cover_c, by = join_by(land_cover_type)) %>% # grab your emissions factors
  # when area estimates are unavailable, but sequestration totals are available, you can estimate area
  mutate(area = ifelse(source != "nlcd", sequestration_potential / seq_mtco2e_sqkm, area)) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm))



nlcd_county_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(nlcd_county_c$year), "Year",
    "county ", class(nlcd_county_c$county), "County name",
    "land_cover_type", class(nlcd_county_c$land_cover_type), "Land cover type from World Cover. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_county_c$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled down by NLCD percent impervious",
    "sequestration_potential", class(nlcd_county_c$sequestration_potential), "Carbon sequestration potential of county land cover type in metric tons of CO2e per year",
    "stock_potential", class(nlcd_county_c$stock_potential), "Carbon stock potential of county land cover type in metric tons of CO2e",
    "source", class(nlcd_county_c$source), "Data source indicates whether area estimates came directly from NLCD, or whether they were interpolated/extrapolated"
  )

saveRDS(nlcd_county_c, paste0("./_nature/data/nlcd_county_landcover_sequestration_", head(sort(unique(nlcd_county_c$year)), 1), "_", tail(sort(unique(nlcd_county_c$year)), 1), ".rds"))
saveRDS(nlcd_county_c_meta, paste0("./_nature/data/nlcd_county_landcover_sequestration_", head(sort(unique(nlcd_county_c$year)), 1), "_", tail(sort(unique(nlcd_county_c$year)), 1), "_meta.rds"))
