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

saveRDS(ctu_fuel_hh, "_energy/data-raw/propane_kerosene_hh_ctu.RDS")
