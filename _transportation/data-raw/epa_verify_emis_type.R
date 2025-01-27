# Review differences in emissions process type availability ----
# RPD = Rate-per-distance
# RPV = Rate-per-vehicle
# RPP = Rate-per-profile
# RPS = Rate-per-start
# RPHO = Rate-per-hour for off network idling
# RPH = Rate-per-hour for hoteling
# be sure to run epa_onroad_emissions_compile.R before trying this script

epa_nei_onroad %>%
  left_join(scc_equates,
    by = join_by(scc, scc6, scc6_desc, fuel_type)
  ) %>%
  select(calc_year, emis_type, scc_level_six) %>%
  unique() %>%
  View()

epa_nei_onroad %>%
  select(calc_year, emis_type) %>%
  unique() %>%
  group_by(calc_year) %>%
  arrange(emis_type) %>%
  summarize(types = paste0(emis_type, collapse = ", ")) %>%
  mutate(
    data_source = "National Emissions Inventory",
    emissions_year = as.numeric(calc_year),
  )

epa_emismod %>%
  select(calc_year, emis_type, poll) %>%
  unique() %>%
  group_by(calc_year) %>%
  arrange(emis_type) %>%
  summarize(
    types = paste0(unique(emis_type), collapse = ", "),
    polls = paste0(unique(poll), collapse = ", ")
  ) %>%
  mutate(
    data_source = "Air Emissions Modeling",
    emissions_year = as.numeric(calc_year),
  )

epa_equates %>%
  select(calc_year, emis_type, poll) %>%
  unique() %>%
  group_by(calc_year) %>%
  arrange(emis_type) %>%
  summarize(
    types = paste0(unique(emis_type), collapse = ", "),
    # polls = paste0(unique(poll), collapse = ", ")
  ) %>%
  mutate(
    data_source = "EQUATES",
    emissions_year = as.numeric(calc_year),
  )

epa_equates %>%
  group_by(emis_type) %>%
  summarize(poll = paste0(unique(poll), collapse = ", "))

epa_emismod %>%
  group_by(emis_type) %>%
  summarize(poll = paste0(unique(poll), collapse = ", "))


epa_nei_onroad %>%
  group_by(calc_year, emis_type) %>%
  summarize(poll = paste0(unique(poll), collapse = ", "))
