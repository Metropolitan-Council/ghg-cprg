# Import data from EPA MOVES
#
# for now, use values from the last MOVES run
source("R/_load_pkgs.R")

# from QAPP and https://www.epa.gov/ghgemissions/understanding-global-warming-potentials
gwp <- tibble::tribble(
  ~ghg, ~co2_equiv,
  "co2", 1,
  "ch4", 28,
  "n2o", 273
) %>% 
  pivot_wider(names_from = ghg,
              values_from = co2_equiv)


epa_moves <- tibble::tribble(
  ~vehicle_weight, ~CO2, ~CH4, ~N2O,
  "Passenger", 353.43, 0.01, 0.01,
  "Medium", 473.24, 0.01, 0.01,
  "Heavy", 1212.36, 0.0206, 0.0402
) %>%
  janitor::clean_names() %>%
  rowwise() %>% 
  mutate(co2_co2_equivalent = 
           sum(co2, (ch4 * 28), (n2o * 273))) %>% 
  mutate(
    moves_year = case_when(
      vehicle_weight == "Passenger" ~ 2019,
      TRUE ~ 2018
    ),
    vehicle_weight = factor(vehicle_weight,
      levels = c(
        "Passenger",
        "Medium",
        "Heavy"
      ),
      ordered = TRUE
    )
  ) %>%
  select(moves_year, vehicle_weight, co2, co2_co2_equivalent, everything())


epa_moves_meta <- tribble(
  ~Column, ~Class, ~Description,
  "moves_year", class(epa_moves$moves_year), "EPA MOVES model run year",
  "vehicle_weight", class(epa_moves$vehicle_weight), "Vehicle weight classification",
  "co2", class(epa_moves$co2), "Grams of carbon dioxide emitted per vehicle mile traveled",
  "co2_co2_equivalent", class(epa_moves$co2_co2_equivalent), "Grams of carbon dioxide and carbon dioxide equivalent according to global warming potential (GWP)",
  "ch4", class(epa_moves$ch4), "Grams of methane emitted per vehicle mile traveled",
  "n2o", class(epa_moves$n2o), "Grams of nitrous oxide per vehicle mile traveled"
)


saveRDS(epa_moves, "_transportation/data/epa_moves.RDS")
saveRDS(epa_moves_meta, "_transportation/data/epa_moves_meta.RDS")
