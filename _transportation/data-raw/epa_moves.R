# Import data from EPA MOVES
#
# for now, use values from the last MOVES run
source("R/_load_pkgs.R")

epa_moves <- tibble::tribble(
  ~vehicle_weight, ~CO2, ~`CO2.+.CO2.equivalent`, ~CH4, ~N2O,
  "Passenger", 353.43, 355.69, 0.01, 0.01,
  "Medium", 473.24, 475.15, 0.01, 0.01,
  "Heavy", 1212.36, 1213.90, 0.0206, 0.0402
) %>%
  janitor::clean_names() %>%
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
  select(moves_year, everything())


epa_moves_meta <- tribble(
  ~Column, ~Class, ~Description,
  "moves_year", class(epa_moves$moves_year), "EPA MOVES model run year",
  "vehicle_weight", class(epa_moves$vehicle_weight), "Vehicle weight classification",
  "co2", class(epa_moves$co2), "Grams of carbon dioxide emitted per vehicle mile traveled",
  "co2_co2_equivalent", class(epa_moves$co2_co2_equivalent), "Grams of carbon dioxide and carbon dioxide equivalent",
  "ch4", class(epa_moves$ch4), "Grams of methane emitted per vehicle mile traveled",
  "n2o", class(epa_moves$n2o), "Grams of nitrous oxide per vehicle mile traveled"
)


saveRDS(epa_moves, "_transportation/data/epa_moves.RDS")
saveRDS(epa_moves_meta, "_transportation/data/epa_moves_meta.RDS")
