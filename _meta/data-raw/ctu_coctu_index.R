# create county-ctu-coctu index, agnostic of year
source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS") %>% st_drop_geometry()

ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  left_join(
    cprg_county %>% sf::st_drop_geometry(),
    join_by(geoid)
  )

cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS") %>%
  left_join(
    cprg_county %>% sf::st_drop_geometry(),
    join_by(county_name, state_name, statefp, state_abb, cprg_area)
  ) %>%
  mutate(ctu_name_full = paste0(ctu_name, ", ", ctu_class)) %>%
  left_join(
    ctu_population %>%
      select(geoid, gnis, coctu_id_fips, coctu_id_gnis) %>%
      unique(),
    by = c("gnis", "geoid")
  ) %>%
  filter(state_abb == "MN")

# create index with all contextual information for each coctu_id_gnis
# independent of year
ctu_coctu_index <- ctu_population %>%
  mutate(
    ctu_name_full = paste0(ctu_name, ", ", ctu_class),
    ctu_name_full_county = paste0(ctu_name_full, ", ", county_name)
  ) %>%
  # remove credit river township!
  filter(
    coctu_id_gnis != "13900663886",
    coctu_id_gnis != "03700664099"
  ) %>%
  select(geoid, gnis, coctu_id_gnis, ctu_name, ctu_name_full, ctu_name_full_county, county_name) %>%
  unique() %>%
  left_join(
    cprg_ctu %>%
      select(ctu_name, gnis, imagine_designation) %>% sf::st_drop_geometry() %>% unique(),
    by = join_by(gnis, ctu_name)
  ) %>%
  mutate(imagine_designation = case_when(
    coctu_id_gnis == "03700664099" ~ "Agricultural", # Empire Township
    coctu_id_gnis == "13900663886" ~ "Rural Residential", # Credit River township
    TRUE ~ imagine_designation
  ))
