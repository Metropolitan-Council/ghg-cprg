source("R/_load_pkgs.R")

cprg_ctu    <- readRDS("_meta/data/cprg_ctu.RDS")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

mn_counties <- cprg_county %>% filter(state_abb == "MN") %>% pull(county_name)
wi_counties <- cprg_county %>% filter(state_abb == "WI") %>% pull(county_name)

ctu_mn <- cprg_ctu %>% filter(state_abb == "MN")
ctu_wi <- cprg_ctu %>% filter(state_abb == "WI")

# Areal interpolation: split block-group household counts proportionally
# across CTU polygons based on area overlap.
# Assumes households are uniformly distributed within each block group.
# MOE is area-weighted in the same proportion as the estimate (simplification).


interpolate_bg_to_ctu <- function(bg_sf, ctu_sf) {
  crs_proj <- 26915L  # UTM Zone 15N — equal-area CRS for MN/WI
  bg_proj  <- sf::st_transform(bg_sf,  crs_proj)
  ctu_proj <- sf::st_transform(ctu_sf, crs_proj)
  
  bg_proj <- bg_proj %>%
    mutate(bg_area_m2 = as.numeric(sf::st_area(geometry)))
  
  frags <- sf::st_intersection(bg_proj, ctu_proj) %>%
    mutate(
      frag_area_m2 = as.numeric(sf::st_area(geometry)),
      area_wt      = frag_area_m2 / bg_area_m2
    )
  
  numeric_cols <- c("propane_hhE", "propane_hhM", "kerosene_hhE", "kerosene_hhM")
  
  frags %>%
    sf::st_drop_geometry() %>%
    mutate(across(all_of(numeric_cols), ~ . * area_wt)) %>%
    group_by(ctu_name, ctu_class, county_name, state_abb) %>%
    summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)),
              .groups = "drop")
}


# Fetch ACS block-group heating fuel data for one year × state


get_heating_bg <- function(state, counties, yr) {
  tidycensus::get_acs(
    geography = "block group",
    variables = c(propane_hh  = "B25040_003",
                  kerosene_hh = "B25040_005"),
    state     = state,
    county    = counties,
    year      = yr,
    output    = "wide",
    geometry  = TRUE
  ) %>%
    sf::st_transform(4326)
}

# Year loop — 5-year ACS 2009–2024

acs_years <- c(2009:2024)

ctu_fuel_list <- purrr::map(acs_years, function(yr) {
  message("\n── Year ", yr, " ──────────────────────────────────")
  
  tryCatch({
    mn_bg <- get_heating_bg("MN", mn_counties, yr)
    wi_bg <- get_heating_bg("WI", wi_counties, yr)
    
    dplyr::bind_rows(
      interpolate_bg_to_ctu(mn_bg, ctu_mn),
      interpolate_bg_to_ctu(wi_bg, ctu_wi)
    ) %>%
      mutate(acs_year = yr)
    
  }, error = function(e) {
    message("  Skipping year ", yr, ": ", conditionMessage(e))
    NULL
  })
})

ctu_fuel_hh <- dplyr::bind_rows(purrr::compact(ctu_fuel_list))

## extrapolate back to 2005

ctu_fuel_hh_extrap <- ctu_fuel_hh %>%
  tidyr::complete(
    tidyr::nesting(ctu_name, ctu_class, county_name, state_abb),
    acs_year = 2005:max(acs_year)
  ) %>%
  mutate(observed = !is.na(propane_hhE)) %>%
  arrange(ctu_name, county_name, state_abb, acs_year) %>%
  group_by(ctu_name, ctu_class, county_name, state_abb) %>%
  mutate(across(c(propane_hhE, propane_hhM, kerosene_hhE, kerosene_hhM),
                imputeTS::na_kalman)) %>%
  ungroup()

numeric_cols <- c("propane_hhE", "propane_hhM", "kerosene_hhE", "kerosene_hhM")

## set urban to suburban counties to zero as this is likely an ACS sampling artifact
ctu_fuel_hh_out <- left_join(ctu_fuel_hh_extrap,
                             cprg_ctu %>% 
                               st_drop_geometry() %>% 
                               distinct(ctu_name, ctu_class, thrive_designation),
                             by = c("ctu_name", "ctu_class")) %>% 
  mutate(across(
    all_of(numeric_cols),
    ~ if_else(thrive_designation %in% c("Urban", "Suburban", "Urban Center"), 0, .x)
  )) %>% 
  select(-thrive_designation)

saveRDS(ctu_fuel_hh_out, "_energy/data-raw/propane_kerosene_hh_ctu.RDS")

### repeat for counties

get_heating_county <- function(state, counties, yr) {
  tidycensus::get_acs(
    geography = "county",
    variables = c(propane_hh  = "B25040_003",
                  kerosene_hh = "B25040_005"),
    state     = state,
    county    = counties,
    year      = yr,
    output    = "wide"
  ) %>%
    mutate(
      county_name = stringr::str_remove(NAME, " County,.*$"),
      state_abb   = state
    ) %>%
    select(county_name, state_abb, propane_hhE, propane_hhM, kerosene_hhE, kerosene_hhM)
}

county_fuel_list <- vector("list", length(acs_years))

for (i in seq_along(acs_years)) {
  yr <- acs_years[[i]]
  message("\n── Year ", yr, " ──────────────────────────────────")
  
  tryCatch({

    
    county_fuel_list[[i]] <- dplyr::bind_rows(
      get_heating_county("MN", mn_counties, yr),
      get_heating_county("WI", wi_counties, yr)
    ) %>%
      mutate(acs_year = yr)
    
  }, error = function(e) {
    message("  Skipping year ", yr, ": ", conditionMessage(e))
  })
}

county_fuel_hh <- dplyr::bind_rows(purrr::compact(county_fuel_list))

county_fuel_hh_extrap <- county_fuel_hh %>%
  tidyr::complete(
    tidyr::nesting(county_name, state_abb),
    acs_year = 2005:max(acs_year)
  ) %>%
  arrange(county_name, state_abb, acs_year) %>%
  group_by(county_name, state_abb) %>%
  mutate(across(all_of(numeric_cols), imputeTS::na_kalman)) %>%
  ungroup()

saveRDS(county_fuel_hh_extrap, "_energy/data-raw/propane_kerosene_hh_county.RDS")
