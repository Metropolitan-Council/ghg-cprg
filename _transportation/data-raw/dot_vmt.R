source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
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
  left_join(cprg_county,
            by = c("county" = "NAME")) %>% 
  select(
    year, 
    GEOID, 
    county, 
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
    "year", class(dot_vmt$year), "VMT estimation year",
    "GEOID", class(dot_vmt$GEOID), "5-digit county ID",
    "county", class(dot_vmt$county), "County name",
    "cprg_area", class(dot_vmt$cprg_area), "Whether county is included in the CPRG area",
    "daily_vmt", class(dot_vmt$daily_vmt), "Vehicle miles traveled on an average day",
    "annual_vmt", class(dot_vmt$annual_vmt), "Annual vehicle miles traveled",
    "centerline miles", class(dot_vmt$centerline_miles), "Centerline miles included in VMT estimate. Minnesota only",
    "data_source", class(dot_vmt$data_source), "State DOT. Either \"MnDOT\" or \"WisDOT\""
  )

saveRDS(dot_vmt, "_transportation/data/dot_vmt.RDS")
saveRDS(dot_vmt_meta, "_transportation/data/dot_vmt_meta.RDS")
