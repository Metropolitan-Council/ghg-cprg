# rm(list=ls())
source("R/_load_pkgs.R")

overwrite_RDS <- TRUE



nlcd_county <- readRDS("./_nature/data/nlcd_county_landcover_2001_2021_v2.rds")
nlcd_ctu <- readRDS("./_nature/data/nlcd_ctu_landcover_2001_2021_v2.rds")

land_cover_c <- readRDS("./_nature/data/land_cover_carbon.rds")

cprg_county <- readRDS("_meta/data/cprg_county.RDS") %>%
  as.data.frame() %>%
  as_tibble()
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS") %>%
  as.data.frame() %>%
  as_tibble()


# nlcd_county countains the area of each land cover type in each county for each year
# For some counties, there may not be a particular land cover type in a given year.
# Let's modify nlcd_county at this stage to include every combination of year, county, and land cover type,
# but assign NA to area where the land cover type is not present in the county for that year.
nlcd_county_rc <- nlcd_county %>%
  as_tibble() %>%
  dplyr::select(-state_name) %>%
  left_join( # this creates an empty grid of all desired year, land_cover_type combinations
    x = expand.grid(
      year = seq(2001, 2021, by = 1),
      county_name = unique(nlcd_county$county_name) %>% sort(),
      land_cover_type = unique(nlcd_county$land_cover_type)
    ),
    y = ., by = join_by(year, county_name, land_cover_type)
  ) %>%
  mutate(source = ifelse(is.na(area), "est", "nlcd")) %>% # filter(land_cover_type == "Urban_Tree")
  group_by(county_name, land_cover_type) %>%
  arrange(county_name, land_cover_type, year) %>%
  mutate(
    area2 = zoo::na.approx(area, na.rm = FALSE),
    area3 = zoo::na.approx(area, na.rm = FALSE, maxgap = Inf, rule = 2),
    source = case_when(
      source == "est" & !is.na(area2) ~ "interpolated",
      source == "est" & !is.na(area3) ~ "extrapolated",
      source == "nlcd" ~ "nlcd"
    ),
    area = case_when(
      source == "nlcd" ~ area,
      source == "interpolated" ~ area2,
      source == "extrapolated" ~ area3
    )
  ) %>%
  dplyr::select(year, county_name, land_cover_type, area, source) %>%
  arrange(county_name, land_cover_type, year) %>%
  left_join(cprg_county, by = join_by(county_name))



# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  saveRDS(nlcd_county_rc, paste0("./_nature/data/nlcd_county_landcover_", head(sort(unique(nlcd_county_rc$year)), 1), "_", tail(sort(unique(nlcd_county_rc$year)), 1), "_v2.rds"))
}



# Compute C sequestration and stock potential for natural systems sectors
nlcd_county_c <- nlcd_county_rc %>%
  left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
  filter(seq_mtco2e_sqkm < 0) %>%
  mutate(
    sequestration_potential = area * seq_mtco2e_sqkm,
    stock_potential = area * stock_mtco2e_sqkm
  ) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm)) %>%
  dplyr::select(year, county_name, state_name, land_cover_type, area, sequestration_potential, stock_potential, source) %>%
  arrange(year, county_name, land_cover_type)


nlcd_county_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(nlcd_county_c$year), "Year",
    "county_name ", class(nlcd_county_c$county_name), "County name",
    "state_name ", class(nlcd_county_c$state_name), "State name",
    "land_cover_type", class(nlcd_county_c$land_cover_type), "Land cover type from World Cover. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_county_c$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled down by NLCD percent impervious",
    "sequestration_potential", class(nlcd_county_c$sequestration_potential), "Carbon sequestration potential of county land cover type in metric tons of CO2e per year",
    "stock_potential", class(nlcd_county_c$stock_potential), "Carbon stock potential of county land cover type in metric tons of CO2e",
    "source", class(nlcd_county_c$source), "Data source indicates whether area estimates came directly from NLCD, or whether they were interpolated/extrapolated"
  )

saveRDS(nlcd_county_c, paste0("./_nature/data/nlcd_county_landcover_sequestration_", head(sort(unique(nlcd_county_c$year)), 1), "_", tail(sort(unique(nlcd_county_c$year)), 1), "_v2.rds"))
saveRDS(nlcd_county_c_meta, paste0("./_nature/data/nlcd_county_landcover_sequestration_", head(sort(unique(nlcd_county_c$year)), 1), "_", tail(sort(unique(nlcd_county_c$year)), 1), "_meta_v2.rds"))








nlcd_ctu_rc <- nlcd_ctu %>%
  as_tibble() %>%
  mutate(tmp1 = paste(ctu_name, ctu_class, county_name, state_name, sep = "_")) %>%
  dplyr::select(year, land_cover_type, area, tmp1) %>%
  left_join( # this creates an empty grid of all desired year, land_cover_type combinations
    x = expand.grid(
      year = seq(2001, 2021, by = 1),
      tmp1 = cprg_ctu %>%
        dplyr::select(ctu_name, ctu_class, county_name, state_name) %>%
        # create a new column that concatenates the ctu_name, ctu_class, county_name, and state_name
        mutate(tmp1 = paste(ctu_name, ctu_class, county_name, state_name, sep = "_")) %>%
        pull(tmp1),
      land_cover_type = unique(nlcd_ctu$land_cover_type)
    ),
    y = ., by = join_by(year, land_cover_type, tmp1)
  ) %>%
  mutate(source = ifelse(is.na(area), "est", "nlcd")) %>% # filter(land_cover_type == "Urban_Tree")
  group_by(tmp1, land_cover_type) %>%
  arrange(tmp1, land_cover_type, year) %>%
  mutate(
    area2 = zoo::na.approx(area, na.rm = FALSE),
    area3 = zoo::na.approx(area, na.rm = FALSE, maxgap = Inf, rule = 2),
    source = case_when(
      source == "est" & !is.na(area2) ~ "interpolated",
      source == "est" & !is.na(area3) ~ "extrapolated",
      source == "nlcd" ~ "nlcd"
    ),
    area = case_when(
      source == "nlcd" ~ area,
      source == "interpolated" ~ area2,
      source == "extrapolated" ~ area3
    )
  ) %>%
  dplyr::select(year, tmp1, land_cover_type, area, source) %>%
  # Let's take the tmp1 column and split it into its constituent parts
  separate(tmp1, c("ctu_name", "ctu_class", "county_name", "state_name"), sep = "_") %>%
  arrange(ctu_name, land_cover_type, year) %>%
  left_join(cprg_ctu, by = join_by(ctu_name, ctu_class, county_name, state_name))






# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  saveRDS(nlcd_ctu_rc, paste0("./_nature/data/nlcd_ctu_landcover_", head(sort(unique(nlcd_ctu_rc$year)), 1), "_", tail(sort(unique(nlcd_ctu_rc$year)), 1), "_v2.rds"))
}



# Compute C sequestration and stock potential for natural systems sectors
nlcd_ctu_c <- nlcd_ctu_rc %>%
  left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
  filter(seq_mtco2e_sqkm < 0) %>%
  mutate(
    sequestration_potential = area * seq_mtco2e_sqkm,
    stock_potential = area * stock_mtco2e_sqkm
  ) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm)) %>%
  dplyr::select(
    year, ctu_name, ctu_class, county_name, state_name, land_cover_type,
    area, sequestration_potential, stock_potential, source
  ) %>%
  arrange(year, ctu_name, land_cover_type)


nlcd_ctu_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(nlcd_county_c$year), "Year",
    "ctu_name", class(nlcd_ctu$ctu_name), "CTU name",
    "ctu_class", class(nlcd_ctu$county_name), "CTU class",
    "county_name ", class(nlcd_county_c$county_name), "County name",
    "state_name ", class(nlcd_county_c$state_name), "State name",
    "land_cover_type", class(nlcd_county_c$land_cover_type), "Land cover type from World Cover. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_county_c$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled down by NLCD percent impervious",
    "sequestration_potential", class(nlcd_county_c$sequestration_potential), "Carbon sequestration potential of county land cover type in metric tons of CO2e per year",
    "stock_potential", class(nlcd_county_c$stock_potential), "Carbon stock potential of county land cover type in metric tons of CO2e",
    "source", class(nlcd_county_c$source), "Data source indicates whether area estimates came directly from NLCD, or whether they were interpolated/extrapolated"
  )



saveRDS(nlcd_ctu_c, paste0("./_nature/data/nlcd_ctu_landcover_sequestration_", head(sort(unique(nlcd_ctu_c$year)), 1), "_", tail(sort(unique(nlcd_ctu_c$year)), 1), "_v2.rds"))
saveRDS(nlcd_ctu_c_meta, paste0("./_nature/data/nlcd_ctu_landcover_sequestration_", head(sort(unique(nlcd_ctu_c$year)), 1), "_", tail(sort(unique(nlcd_ctu_c$year)), 1), "_meta_v2.rds"))
