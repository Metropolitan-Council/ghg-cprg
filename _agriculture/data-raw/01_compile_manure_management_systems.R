source("R/_load_pkgs.R")

if (!file.exists("_agriculture/data-raw/ag-module.xlsx")) {
  cli::cli_abort("Download agriculture data from MS Team")
}

dairy_mm <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "manure%",
  range = "B4:H1804"
) %>%
  mutate(
    year = as.numeric(substr(`Year & State`, 1, 4)),
    state = substr(`Year & State`, 5, 6)
  ) %>%
  filter(state %in% c("MN", "WI"), year >= 2005) %>%
  select(-1) %>%
  pivot_longer(cols = 1:6, names_to = "mgmt_system", values_to = "percentage") %>%
  mutate(
    managed = if_else(mgmt_system %in% c("Daily Spread", "Pasture"), "No", "Yes"),
    livestock_type = "Dairy Cows"
  )

heifers_mm <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "manure%",
  range = "J4:M1804"
) %>%
  mutate(
    year = as.numeric(substr(`Year & State`, 1, 4)),
    state = substr(`Year & State`, 5, 6)
  ) %>%
  filter(state %in% c("MN", "WI"), year >= 2005) %>%
  select(-1) %>%
  pivot_longer(cols = 1:3, names_to = "mgmt_system", values_to = "percentage") %>%
  mutate(
    managed = if_else(mgmt_system %in% c("Daily Spread", "PRP"), "No", "Yes"),
    livestock_type = "Heifers"
  ) # not sure we have data on percent that are heifers

feedlot_cattle_mm <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "manure%",
  range = "O4:P54"
) %>%
  mutate(state = STATE) %>%
  filter(state %in% c("MN", "WI")) %>%
  select(-1) %>%
  pivot_longer(cols = 1, names_to = "mgmt_system", values_to = "percentage") %>%
  mutate(
    managed = "Yes",
    livestock_type = "Feedlot Cattle"
  ) %>%
  crossing(year = 2005:2022)

beef_cattle_mm <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "manure%",
  range = "R4:S54"
) %>%
  mutate(state = STATE) %>%
  filter(state %in% c("MN", "WI")) %>%
  select(-1) %>%
  pivot_longer(cols = 1, names_to = "mgmt_system", values_to = "percentage") %>%
  mutate(
    managed = "No",
    livestock_type = "Beef Cows"
  ) %>%
  crossing(year = 2005:2022)

swine_mm <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "manure%",
  range = "U4:Z1804"
) %>%
  mutate(
    year = as.numeric(substr(`Year & State`, 1, 4)),
    state = substr(`Year & State`, 5, 6)
  ) %>%
  filter(state %in% c("MN", "WI"), year >= 2005) %>%
  select(-1) %>%
  pivot_longer(cols = 1:5, names_to = "mgmt_system", values_to = "percentage") %>%
  mutate(
    managed = if_else(mgmt_system %in% c("Daily Spread", "Pasture"), "No", "Yes"),
    livestock_type = "Swine"
  )

layers_mm <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "manure%",
  range = "AB4:AF1804"
) %>%
  mutate(
    year = as.numeric(substr(`Year & State`, 1, 4)),
    state = substr(`Year & State`, 5, 6)
  ) %>%
  filter(state %in% c("MN", "WI"), year >= 2005) %>%
  select(-1) %>%
  pivot_longer(cols = 1:4, names_to = "mgmt_system", values_to = "percentage") %>%
  mutate(
    managed = "Yes",
    livestock_type = "Layers"
  )

turkeys_mm <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "manure%",
  range = "AK4:AM54"
) %>%
  mutate(state = State) %>%
  filter(state %in% c("MN", "WI")) %>%
  select(-1) %>%
  pivot_longer(cols = 1:2, names_to = "mgmt_system", values_to = "percentage") %>%
  mutate(
    managed = if_else(mgmt_system %in% c("Range"), "No", "Yes"),
    livestock_type = "Turkeys"
  ) %>%
  crossing(year = 2005:2022)

sheep_mm <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "manure%",
  range = "AO4:AQ54"
) %>%
  mutate(state = `Year & State`) %>%
  filter(state %in% c("MN", "WI")) %>%
  select(-1) %>%
  pivot_longer(cols = 1:2, names_to = "mgmt_system", values_to = "percentage") %>%
  mutate(
    managed = "Unknown",
    livestock_type = "Sheep"
  ) %>%
  crossing(year = 2005:2022)

goats_mm <- readxl::read_xlsx("_agriculture/data-raw/ag-module.xlsx",
  sheet = "manure%",
  range = "AS4:AT54"
) %>%
  mutate(state = STATE) %>%
  filter(state %in% c("MN", "WI")) %>%
  select(-1) %>%
  pivot_longer(cols = 1, names_to = "mgmt_system", values_to = "percentage") %>%
  mutate(
    managed = "No",
    livestock_type = "Goats"
  ) %>%
  crossing(year = 2005:2022)

manure_mgmt_formatted <- bind_rows(
  dairy_mm,
  heifers_mm,
  feedlot_cattle_mm,
  beef_cattle_mm,
  swine_mm,
  layers_mm,
  turkeys_mm,
  sheep_mm,
  goats_mm
)

manure_mgmt_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(manure_mgmt_formatted$year), "Year",
    "state", class(manure_mgmt_formatted$state), "State",
    "mgmt_system", class(manure_mgmt_formatted$mgmt_system), "Number of individual (heads) of livestock type",
    "percentage", class(manure_mgmt_formatted$percentage), "Percentage of animals on management system",
    "managed", class(manure_mgmt_formatted$managed), "Is this a managed manure system or are animals free range?",
    "livestock_type", class(manure_mgmt_formatted$livestock_type), "Livestock classification"
  )

saveRDS(manure_mgmt_formatted, "./_agriculture/data/manure_management_systems.rds")
saveRDS(manure_mgmt_meta, "./_agriculture/data/manure_management_systems_meta.rds")
