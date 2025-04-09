# Identify utility-CTU overlaps
source("R/_load_pkgs.R")

# ELECTRICITY
# MN electric utility region
MN_elecUtils <- readRDS(here("_energy", "data", "MN_elecUtils.RDS")) %>%
  st_make_valid()

# simplified CPRG CTU geometry for quick comparison -- dissolve counties and just retain cities/township boundaries
cprg_mn_ctu_dissolve <- readRDS(here("_meta", "data", "cprg_ctu.RDS")) %>%
  mutate(geometry = st_make_valid(geometry)) %>%
  mutate(geometry = st_simplify(geometry, dTolerance = 0.1)) %>%
  st_transform(st_crs(MN_elecUtils)) %>%
  group_by(ctu_name, ctu_class) %>%
  summarise(geometry = st_union(geometry), .groups = "keep") %>%
  ungroup() %>%
  st_make_valid() %>%
  left_join(readRDS(here("_meta", "data", "cprg_ctu.RDS")) %>% st_drop_geometry() %>% select(ctu_name, ctu_class, state_name),
    by = join_by(ctu_name, ctu_class),
    multiple = "first"
  ) %>%
  filter(state_name == "Minnesota")

# Perform spatial join to match each CTU (city/township) with electric utilities
ctu_elec_utility_mapping <- st_join(
  cprg_mn_ctu_dissolve,
  MN_elecUtils,
  join = st_intersects, # Searches for utilities that overlap each CTU
  left = FALSE
) %>%
  select(ctu_name, ctu_class, utility_name = full_name) %>%
  distinct() %>%
  st_drop_geometry()

# Count of utilities per CTU
ctu_elec_utility_count <- ctu_elec_utility_mapping %>%
  group_by(ctu_name, ctu_class) %>%
  summarize(
    numUtilities = n_distinct(utility_name),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  st_drop_geometry()



# NAT GAS
# nationwide natural gas utility service areas 
natGasUtils <- readRDS(here("_energy", "data", "MN_fed_natGasUtils.RDS")) %>%
  st_transform(st_crs(cprg_mn_ctu_dissolve)) %>% # CRS transform to UTM 15N
  st_make_valid()

# Perform spatial join to match each CTU (city/township) with electric utilities
ctu_ng_utility_mapping <- st_join(
  cprg_mn_ctu_dissolve,
  natGasUtils,
  join = st_intersects, # Searches for utilities that overlap each CTU
  left = FALSE
) %>%
  select(ctu_name, ctu_class, utility_name = NAME) %>%
  distinct() %>%
  st_drop_geometry()

# Count of utilities per CTU
ctu_ng_utility_count <- ctu_ng_utility_mapping %>%
  group_by(ctu_name, ctu_class) %>%
  summarize(
    numUtilities = n_distinct(utility_name),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  st_drop_geometry()


write_rds(ctu_elec_utility_mapping, here("_energy", "data", "ctu_elec_utility_intersect.RDS"))
write_rds(ctu_ng_utility_mapping, here("_energy", "data", "ctu_ng_utility_intersect.RDS"))
