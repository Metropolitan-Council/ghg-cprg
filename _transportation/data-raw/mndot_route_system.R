source("R/_load_pkgs.R")
source("R/download_read_table.R")
source("_meta/data-raw/ctu_saint_names.R")


# from vmt_city_raw -----
route_system_reference <- vmt_city_raw %>%
  select(route_system, year) %>%
  filter(year == 2023) %>%
  select(route_system) %>%
  unique() %>%
  tidyr::separate_wider_delim(route_system,
                              delim = "-",
                              names = c(
                                "route_system_id",
                                "route_system_desc"
                              ),
                              too_many = "merge"
  ) %>%
  mutate(route_system_id = str_pad(
    route_system_id %>%
      str_trim(),
    side = "left",
    pad = "0",
    width = 2
  ),
  route_system_desc = stringr::str_trim(route_system_desc)) %>%
  arrange(route_system_id) %>% 
  bind_rows(
    tibble::tribble(
      ~route_system_id, ~route_system_desc,
      "TW",       "Township Systems",
      "MI",       "Minor Systems",
      "08, 09", "Township Systems",
      "11 TO 23", "Minor Systems"
    )
  )

route_system_year_all <- vmt_city_raw %>%
  select(route_system, year) %>%
  unique() %>%
  tidyr::separate_wider_delim(
    route_system,
    delim = "-",
    names = c(
      "route_system_id",
      "route_system"
    ),
    too_many = "merge",
    too_few = "align_start",
    cols_remove = FALSE
  ) %>%
  mutate(route_system_id = str_pad(
    route_system_id %>%
      str_trim(),
    side = "left",
    pad = "0",
    width = 2
  )) %>%
  left_join(route_system_reference,
            by = c("route_system_id")
  )

saveRDS(route_system_year_all, "_transportation/data-raw/mndot/route_system_ctu.RDS")

# from vmt_county_raw

route_system_reference <- vmt_county_raw %>%
  select(route_system, year) %>%
  filter(year == 2023) %>%
  select(route_system) %>%
  unique() %>%
  tidyr::separate_wider_delim(route_system,
                              delim = "-",
                              names = c(
                                "route_system_id",
                                "route_system_desc"
                              ),
                              too_many = "merge"
  ) %>%
  mutate(route_system_id = str_pad(
    route_system_id %>%
      str_trim(),
    side = "left",
    pad = "0",
    width = 2
  )) %>%
  arrange(route_system_id)

route_system_year_all <- vmt_county_raw %>%
  select(route_system, year) %>%
  unique() %>%
  tidyr::separate_wider_delim(route_system,
                              delim = "-",
                              names = c(
                                "route_system_id",
                                "route_system"
                              ),
                              too_many = "merge",
                              cols_remove = FALSE
  ) %>%
  mutate(route_system_id = str_pad(
    route_system_id %>%
      str_trim(),
    side = "left",
    pad = "0",
    width = 2
  )) %>%
  left_join(route_system_reference,
            by = c("route_system_id")
  ) %>% 
  ungroup()

saveRDS(route_system_year_all, "_transportation/data-raw/mndot/route_system_county.RDS")

# combine all -----
route_system_ctu <- readRDS("_transportation/data-raw/mndot/route_system_ctu.RDS")
route_system_county <- readRDS("_transportation/data-raw/mndot/route_system_county.RDS")


mndot_route_system <- bind_rows(route_system_county, 
          route_system_ctu) %>% 
  mutate(route_system_desc = stringr::str_trim(route_system_desc)) %>% 
  select(-year) %>% 
  unique() %>% 
  mutate(route_system_level = case_when(
    route_system_id %in% c("01", "02", "03", "04", "07",
                           "52", "32", "53") ~ "Trunk highway and county systems",
    TRUE ~ "Local systems"
  ))


saveRDS(mndot_route_system,  "_transportation/data-raw/mndot/mndot_route_system.RDS")
