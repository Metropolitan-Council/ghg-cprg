source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_county_meta <- readRDS("_meta/data/cprg_county_meta.RDS")
# load VMT from both MnDOT and WisDOT
mndot_vmt <- readRDS(file.path(here::here(), "_transportation/data-raw/mndot/mndot_vmt_county.RDS")) %>%
  mutate(data_source = "MnDOT")
wisdot_vmt <- readRDS(file.path(here::here(), "_transportation/data-raw/wisdot/wisdot_vmt_county.RDS")) %>%
  mutate(data_source = "WisDOT")

dot_vmt <- bind_rows(
  mndot_vmt,
  wisdot_vmt
) %>%
  ungroup() %>%
  rename(county_name = county) %>%
  left_join(cprg_county, by = join_by(county_name, cprg_area)) %>%
  select(
    vmt_year = year,
    geoid,
    county_name,
    cprg_area,
    daily_vmt,
    annual_vmt,
    centerline_miles,
    data_source
  )

names(dot_vmt)

dot_vmt_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "vmt_year", class(dot_vmt$vmt_year), "VMT estimation year",
    "daily_vmt", class(dot_vmt$daily_vmt), "County vehicle miles traveled (VMT) on an average day",
    "annual_vmt", class(dot_vmt$annual_vmt), "County annual vehicle miles traveled",
    "centerline miles", class(dot_vmt$centerline_miles), "Centerline miles included in county VMT estimate. Minnesota only",
    "data_source", class(dot_vmt$data_source), "State DOT. Either \"MnDOT\" or \"WisDOT\""
  ) %>%
  bind_rows(cprg_county_meta) %>%
  filter(Column %in% names(dot_vmt)) %>% 
  arrange(match(Column, names(dot_vmt)))


saveRDS(dot_vmt, "_transportation/data/dot_vmt.RDS")
saveRDS(dot_vmt_meta, "_transportation/data/dot_vmt_meta.RDS")
