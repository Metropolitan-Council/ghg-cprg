source("R/_load_pkgs.R")


# SEDS residential consumption
# Source: https://www.eia.gov/state/seds/sep_use/total/csv/use_all_btu.csv
# PARCB = residential HGL/propane (billion BTU)
# KERCB = residential kerosene (billion BTU)


seds <- read.csv("https://www.eia.gov/state/seds/sep_use/total/csv/use_all_btu.csv") %>%
  filter(
    State %in% c("MN", "WI"),
    MSN %in% c("HLRCB", "DFRCB", "KSRCB")
  ) %>%
  pivot_longer(
    cols = matches("^X?[0-9]{4}$"),
    names_to = "year",
    values_to = "billion_btu"
  ) %>%
  mutate(
    year  = as.integer(gsub("X", "", year)),
    mmBtu = billion_btu * 1e3,
    # combine fueloil and kerosene to match ACS pull
    msn   = if_else(MSN %in% c("DFRCB", "KSRCB"), "fueloil_other", "propane")
  ) %>%
  filter(year >= 2005) %>%
  group_by(state_abb = State, msn, year) %>%
  summarise(mmBtu = sum(mmBtu, na.rm = TRUE), .groups = "drop")

# ACS state-level household counts — denominator for per-household rate

acs_years <- c(2009:2024)

state_hh <- purrr::map(acs_years, function(yr) {
  purrr::map(c("MN", "WI"), function(st) {
    tryCatch(
      tidycensus::get_acs(
        geography = "state",
        variables = c(propane_hh = "B25040_003", kerosene_hh = "B25040_005"),
        state     = st,
        year      = yr,
        output    = "wide"
      ) %>%
        mutate(state_abb = st, acs_year = yr) %>%
        select(state_abb, acs_year, propane_hhE, kerosene_hhE),
      error = function(e) {
        message("Skipping ", st, " ", yr, ": ", conditionMessage(e))
        NULL
      }
    )
  })
}) %>%
  purrr::flatten() %>%
  dplyr::bind_rows() %>%
  tidyr::complete(state_abb, acs_year = 2005:max(acs_year)) %>%
  arrange(state_abb, acs_year) %>%
  group_by(state_abb) %>%
  mutate(across(c(propane_hhE, kerosene_hhE), imputeTS::na_kalman)) %>%
  ungroup()

# Per-household mmBtu rates: SEDS state total / ACS state households

rates <- state_hh %>%
  left_join(
    seds %>% filter(msn == "propane") %>% select(state_abb, year, propane_mmBtu_state = mmBtu),
    by = c("state_abb", "acs_year" = "year")
  ) %>%
  left_join(
    seds %>% filter(msn == "fueloil_other") %>% select(state_abb, year, fueloil_other_mmBtu_state = mmBtu),
    by = c("state_abb", "acs_year" = "year")
  ) %>%
  mutate(
    propane_mmBtu_per_hh = propane_mmBtu_state / propane_hhE,
    fueloil_other_mmBtu_per_hh = fueloil_other_mmBtu_state / kerosene_hhE
  )

# Apply per-household rates to CTU household counts

ctu_fuel_hh <- readRDS("_energy/data-raw/propane_kerosene_hh_ctu.RDS")

ctu_fuel_estimates <- ctu_fuel_hh %>%
  left_join(
    rates %>% select(state_abb, acs_year, propane_mmBtu_per_hh, fueloil_other_mmBtu_per_hh),
    by = c("state_abb", "acs_year")
  ) %>%
  mutate(
    propane_mmBtu = propane_hhE * propane_mmBtu_per_hh,
    fueloil_other_mmBtu = kerosene_hhE * fueloil_other_mmBtu_per_hh
  ) %>%
  select(ctu_name, ctu_class, county_name, state_abb, acs_year, propane_mmBtu, fueloil_other_mmBtu)

saveRDS(ctu_fuel_estimates, "_energy/data-raw/ctu_propane_fueloil_use.RDS")

# ── County estimates ───────────────────────────────────────────────────────────
county_fuel_hh <- readRDS("_energy/data-raw/propane_kerosene_hh_county.RDS")

county_fuel_estimates <- county_fuel_hh %>%
  left_join(
    rates %>% select(state_abb, acs_year, propane_mmBtu_per_hh, fueloil_other_mmBtu_per_hh),
    by = c("state_abb", "acs_year")
  ) %>%
  mutate(
    propane_mmBtu       = propane_hhE * propane_mmBtu_per_hh,
    fueloil_other_mmBtu = kerosene_hhE * fueloil_other_mmBtu_per_hh
  ) %>%
  select(county_name, state_abb, acs_year, propane_mmBtu, fueloil_other_mmBtu)

saveRDS(county_fuel_estimates, "_energy/data-raw/county_propane_fueloil_use.RDS")
