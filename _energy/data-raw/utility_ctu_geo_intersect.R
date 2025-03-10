# Process Xcel Community Reports data
source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

# Xcel utility region
MN_elecUtils <- readRDS(here("_energy", "data", "MN_elecUtils.RDS")) 

# simplified CPRG CTU geometry for quick comparison -- dissolve counties and just retain cities/township boundaries
cprg_ctu_dissolve <- readRDS(here("_meta", "data", "cprg_ctu.RDS")) %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  mutate(geometry = st_simplify(geometry, dTolerance = 0.1)) %>%
  group_by(ctu_name, ctu_class) %>%
  summarise(geometry = st_union(geometry), .groups = "keep") %>%
  ungroup() %>%
  st_make_valid() %>%
  left_join(cprg_ctu %>% st_drop_geometry() %>% select(ctu_name, ctu_class, state_name),
            by = join_by(ctu_name, ctu_class),
            multiple = "first")

cprg_region <- readRDS(here("_meta", "data", "cprg_county.RDS")) %>%
  st_union() %>%
  st_as_sf()

MN_elecUtils_dissolve_cprg <- MN_elecUtils %>%
  st_intersection(cprg_region)
  group_by(mmua_name) %>%
  summarise(
    geometry = st_union(geometry)
  ) %>%
  ungroup()


ggplot() +
  geom_sf(data = MN_elecUtils_dissolve, fill = "red", color = "red") +
  geom_sf_label(data = MN_elecUtils, aes(label = mmua_name)) +
  geom_sf(data = cprg_ctu, fill = NA, color = "blue") +
  theme_minimal()

# Ensure CRS matches and calc city area
cprg_ctu_proj <- st_transform(cprg_ctu_dissolve, st_crs(xcelArea_single)) %>%
  mutate(area_city = as.numeric(st_area(geometry)))

# Compute the intersection: Get city area where Xcel operates
ctu_xcel_intersect <- st_intersection(cprg_ctu_proj, xcelArea_single) %>%
  mutate(area_xcel = as.numeric(st_area(geometry)))

# Anti-join with activity data gathered in first pass to identify list of geographies to request
ctu_xcel_not_reported_yet <- ctu_xcel_intersect %>%
  anti_join(Xcel_activityData_2015_2023 %>% st_drop_geometry(),
            by = join_by(ctu_name, ctu_class)
            ) %>%
  filter(state_name == "Minnesota") %>%
  st_drop_geometry()

write_csv(ctu_xcel_not_reported_yet, here("_energy", "data-raw", "ctu_xcel_not_reported_yet.csv"))
