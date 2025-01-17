rm(list=ls())
source("R/_load_pkgs.R")

overwrite_RDS <- TRUE

nlcd_county <- readRDS("./_nature/data/nlcd_county_landcover_2001_2021.rds")
nlcd_ctu <- readRDS("./_nature/data/nlcd_ctu_landcover_2001_2021.rds")
land_cover_c <- readRDS("./_nature/data/land_cover_carbon.rds")



# load the county and ctu boundaries layer
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")

# turn your county and ctu layers into dataframes
cprg_county_df <- cprg_county %>% as.data.frame() %>%
  select(geoid, county_name, state_name) %>% st_drop_geometry()

cprg_ctu_df <- cprg_ctu %>% as.data.frame() %>%
  select(gnis, geoid_wis, ctu_name, ctu_class, county_name, state_name) %>% 
  st_drop_geometry() %>%
  mutate(
    geoid_wis = as.numeric(geoid_wis),
    gnis = as.numeric(gnis))





# Compute C sequestration and stock potential for natural systems sectors by county
nlcd_county_c <- nlcd_county %>%
  as_tibble() %>%
  left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
  filter(seq_mtco2e_sqkm < 0) %>%
  mutate(
    sequestration_potential = area * seq_mtco2e_sqkm,
    stock_potential = area * stock_mtco2e_sqkm
  ) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm)) %>%
  left_join( # this creates an empty grid of all desired year, land cover combinations
    x = cprg_county_df %>%
      crossing(year=seq(2001, 2021, by = 1),
               land_cover_type = unique(land_cover_c$land_cover_type)),
    y = ., by = join_by(county_name, state_name, year, land_cover_type)
  ) %>%
  mutate(source = ifelse(is.na(area), "est", "nlcd")) %>%
  group_by(geoid, county_name, state_name, land_cover_type) %>%
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
  dplyr::select(geoid, county_name, state_name, year, land_cover_type, area, sequestration_potential, stock_potential, source) %>%
  arrange(year, county_name, land_cover_type) %>%
  # Now that we've extrapolated sequestration/stock values despite having no area estimates,
  # we need to calculate area based on the extrapolated sequestration.
  left_join(., land_cover_c, by = join_by(land_cover_type)) %>% # grab your emissions factors
  # when area estimates are unavailable, but sequestration totals are available, you can estimate area
  mutate(area = ifelse(source != "nlcd", sequestration_potential / seq_mtco2e_sqkm, area)) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm))




# Compute C sequestration and stock potential for natural systems sectors by CTU
nlcd_ctu_c <- nlcd_ctu %>%
  as_tibble() %>%
  left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
  filter(seq_mtco2e_sqkm < 0) %>%
  mutate(
    sequestration_potential = area * seq_mtco2e_sqkm,
    stock_potential = area * stock_mtco2e_sqkm
  ) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm)) %>%
  left_join( # this creates an empty grid of all desired year, land cover combinations
    x = cprg_ctu_df %>%
      crossing(year=seq(2001, 2021, by = 1),
               land_cover_type = unique(land_cover_c$land_cover_type)),
    y = ., by = join_by(ctu_name, ctu_class, county_name, state_name, year, land_cover_type)
  ) %>%
  mutate(source = ifelse(is.na(area), "est", "nlcd")) %>%
  group_by(ctu_name, ctu_class, county_name, state_name, land_cover_type) %>%
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
  dplyr::select(gnis, geoid_wis, ctu_name, ctu_class, county_name, state_name,
                year, land_cover_type, area, sequestration_potential, stock_potential, source) %>%
  arrange(year, ctu_name, land_cover_type) %>%
  # Now that we've extrapolated sequestration/stock values despite having no area estimates,
  # we need to calculate area based on the extrapolated sequestration.
  left_join(., land_cover_c, by = join_by(land_cover_type)) %>% # grab your emissions factors
  # when area estimates are unavailable, but sequestration totals are available, you can estimate area
  mutate(area = ifelse(source != "nlcd", sequestration_potential / seq_mtco2e_sqkm, area)) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm))




nlcd_county_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "geoid", class(nlcd_county_c$geoid), "County GEOID",
    "county_name", class(nlcd_county_c$county_name), "County name",
    "state_name", class(nlcd_county_c$state_name), "State name",
    "year", class(nlcd_county_c$year), "Year",
    "land_cover_type", class(nlcd_county_c$land_cover_type), "Land cover type from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_county_c$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled down by NLCD percent impervious",
    "sequestration_potential", class(nlcd_county_c$sequestration_potential), "Carbon sequestration potential of county land cover type in metric tons of CO2e per year",
    "stock_potential", class(nlcd_county_c$stock_potential), "Carbon stock potential of county land cover type in metric tons of CO2e",
    "source", class(nlcd_county_c$source), "Data source indicates whether area estimates came directly from NLCD, or whether they were interpolated/extrapolated"
  )

# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  saveRDS(nlcd_county_c, paste0("./_nature/data/nlcd_county_landcover_sequestration_", head(sort(unique(nlcd_county_c$year)), 1), "_", tail(sort(unique(nlcd_county_c$year)), 1), ".rds"))
  saveRDS(nlcd_county_c_meta, paste0("./_nature/data/nlcd_county_landcover_sequestration_", head(sort(unique(nlcd_county_c$year)), 1), "_", tail(sort(unique(nlcd_county_c$year)), 1), "_meta.rds"))
}



nlcd_ctu_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "gnis", class(nlcd_ctu_c$gnis), "Minnesota geographic identifier",
    "geoid_wis", class(nlcd_ctu_c$geoid_wis), "Wisconsin geographic identifier",
    "ctu_name", class(nlcd_ctu_c$ctu_name), "City, township, unorganized territory, or village name",
    "ctu_class", class(nlcd_ctu_c$ctu_class), "City class (City, township, unorganized territory, or village)",
    "county_name", class(nlcd_ctu_c$county_name), "County name",
    "state_name", class(nlcd_ctu_c$state_name), "State name",
    "year", class(nlcd_ctu_c$year), "Year",
    "land_cover_type", class(nlcd_ctu_c$land_cover_type), "Land cover type from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_ctu_c$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled down by NLCD percent impervious",
    "sequestration_potential", class(nlcd_ctu_c$sequestration_potential), "Carbon sequestration potential of county land cover type in metric tons of CO2e per year",
    "stock_potential", class(nlcd_ctu_c$stock_potential), "Carbon stock potential of county land cover type in metric tons of CO2e",
    "source", class(nlcd_ctu_c$source), "Data source indicates whether area estimates came directly from NLCD, or whether they were interpolated/extrapolated"
  )

# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  saveRDS(nlcd_ctu_c, paste0("./_nature/data/nlcd_ctu_landcover_sequestration_", head(sort(unique(nlcd_ctu_c$year)), 1), "_", tail(sort(unique(nlcd_ctu_c$year)), 1), ".rds"))
  saveRDS(nlcd_ctu_c_meta, paste0("./_nature/data/nlcd_ctu_landcover_sequestration_", head(sort(unique(nlcd_ctu_c$year)), 1), "_", tail(sort(unique(nlcd_ctu_c$year)), 1), "_meta.rds"))
}




# nlcd_ctu_c %>%
#   filter(ctu_name %in% c("Minneapolis", "Edina", "Saint Paul")) %>%
#   ggplot() + theme_minimal() +
#   geom_point(data=. %>% filter(source!= "nlcd"), 
#              mapping=aes(x=year,y=-sequestration_potential, fill=source, color=land_cover_type), shape=21) +
#   scale_fill_manual(breaks=c("interpolated", "extrapolated"), values=c("white","gray70")) +
#   ggnewscale::new_scale_fill() +
#   geom_point(data=. %>% filter(source == "nlcd"), 
#              mapping=aes(x=year,y=-sequestration_potential, fill=land_cover_type, color=land_cover_type), shape=21) +
#   facet_wrap(~ctu_name)

