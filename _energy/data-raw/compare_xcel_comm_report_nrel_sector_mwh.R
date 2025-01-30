#### Script to compare Xcel community report sector breakouts to
#### NREL sector breakouts. Loads in data created in Minnesota_xcelCommunity_reports_electricity.R
#### and nrel_slope_energy.R


Xcel_activityData_2015_2023 <- readRDS("_energy/data/Xcel_activityData_2015_2023.RDS")

# preprocess the NREL proportions dataset -- make duplicates of 2017 records for 2015 and 2016 to enable join to Xcel data
nrel_slope_city_emission_proportions_adjusted <- readRDS("_energy/data-raw/nrel_slope/nrel_slope_city_emission_proportions.RDS") %>%
  filter(source == "Electricity") %>%
  mutate(nrel_year = year) %>%
  # Duplicate rows for 2015 and 2016, mapping them to 2017
  bind_rows(
    filter(., year == 2017) %>% mutate(year = 2015),
    filter(., year == 2017) %>% mutate(year = 2016)
  )

xcel_activityData_NREL_QA_2015_2023 <- Xcel_activityData_2015_2023 %>%
  left_join(
    nrel_slope_city_emission_proportions_adjusted,
    # implicitly applies same proportions to COCTU splits
    by = join_by(
      ctu_name == city_name,
      ctu_class == ctu_class,
      year == year
    )
  ) %>%
  select(
    -source.x,
    -source.y,
    -kwh_delivered,
    -city_name
  )

xcel_activityData_NREL_2015_2023 <- bind_rows(
  # Keep original rows that are not 'Business'
  xcel_activityData_NREL_QA_2015_2023 %>%
    filter(sector_mapped != "Business"),
  
  # Expand `Business` rows into 'commercial*'
  xcel_activityData_NREL_QA_2015_2023 %>%
    filter(sector_mapped == "Business") %>%
    mutate(sector_mapped = "commercial*"),
  
  # Expand `Business` rows into 'industrial*'
  xcel_activityData_NREL_QA_2015_2023 %>%
    filter(sector_mapped == "Business") %>%
    mutate(sector_mapped = "industrial*")
) %>%
  # Mutate disaggregated or original values based on the value of `sector_mapped`
  mutate(
    util_co2e = coalesce(disagg_util_reported_co2e, util_reported_co2e),
    util_mWh = coalesce(disagg_mWh_delivered, mWh_delivered),
    
    # Adjust values for 'commercial*' and 'industrial*' rows
    # breaks out the Business records into two pieces based oon the proportional breakdown of NREL-modeled commercial/industrial
    util_co2e = case_when(
      sector_mapped == "commercial*" ~ util_co2e * coalesce(
        (commercial_city / (commercial_city + industrial_city)),
        (commercial_downscale / (commercial_downscale + industrial_downscale))
      ),
      sector_mapped == "industrial*" ~ util_co2e * coalesce(
        (industrial_city / (commercial_city + industrial_city)),
        (industrial_downscale / (commercial_downscale + industrial_downscale))
      ),
      TRUE ~ util_co2e
    ),
    util_mWh = case_when(
      sector_mapped == "commercial*" ~ util_mWh * coalesce(
        (commercial_city / (commercial_city + industrial_city)),
        (commercial_downscale / (commercial_downscale + industrial_downscale))
      ),
      sector_mapped == "industrial*" ~ util_mWh * coalesce(
        (industrial_city / (commercial_city + industrial_city)),
        (industrial_downscale / (commercial_downscale + industrial_downscale))
      ),
      TRUE ~ util_mWh
    ),
    
    # Create `nrel_breakout_source` column to identify which NREL data set was used to disaggregate BUSINESS records (if such disagg was done)
    # NA in this field + no asterisk on commercial/industrial means the breakout was directly provided by utility
    nrel_breakout_source = case_when(
      sector_mapped == "commercial*" & !is.na(commercial_city) ~ "CITY",
      sector_mapped == "industrial*" & !is.na(industrial_city) ~ "CITY",
      sector_mapped == "commercial*" & !is.na(commercial_downscale) ~ "COUNTY",
      sector_mapped == "industrial*" & !is.na(industrial_downscale) ~ "COUNTY",
      TRUE ~ NA_character_
    )
  )

# final cleanup of Xcel Activity data
xcel_activityData_NREL_2015_2023_process <- xcel_activityData_NREL_2015_2023 %>%
  select(
    -util_reported_co2e, # source numbers, not disaggregated by sector or COCTU
    -mWh_delivered, # source numbers, not disaggregated by sector or COCTU
    -c(10:22)
  ) # intermediate calculations

write_rds(xcel_activityData_NREL_2015_2023_process, "_energy/data/xcel_activityData_NREL_2015_2023_process.RDS")

ctu_commDesg <- read.csv("_meta/data-raw/ctus_summary_2018.csv") %>%
  select(
    ctu_name = Ctu.Name,
    community_designation = Community.Designation
  )

# Validate and repair geometries before dissolving
cprg_ctu <- cprg_ctu %>%
  filter(ctu_class == "CITY") %>%
  mutate(geometry = st_make_valid(geometry)) # Ensure all geometries are valid

# Dissolve polygons by ctu_name
dissolved_ctu <- cprg_ctu %>%
  select(ctu_name, geometry) %>% # Keep relevant columns
  group_by(ctu_name) %>% # Group by ctu_name
  summarize(geometry = st_union(geometry)) %>% # Merge geometries
  ungroup()

# plot(dissolved_ctu["geometry"]) # Quick visualization

# enable comparison of NREL breakdowns to actual breakdowns from Xcel
complete_city_years <- xcel_activityData_NREL_2015_2023 %>%
  filter(
    !is.na(util_mWh),
    !is.na(commercial_city),
    !is.na(industrial_city),
    !is.na(residential_city)
  ) %>%
  group_by(ctu_name, year) %>%
  summarize(
    has_residential = any(sector_mapped == "residential"),
    has_commercial = any(sector_mapped == "commercial"),
    has_industrial = any(sector_mapped == "industrial"),
    .groups = "drop"
  ) %>%
  filter(
    has_residential & has_commercial & has_industrial
  )

# Collapse rows to city-year grain
complete_city_NREL_comparison <- xcel_activityData_NREL_2015_2023 %>%
  semi_join(complete_city_years, by = c("ctu_name", "year")) %>%
  group_by(ctu_name, year) %>%
  summarize(
    total_util_mWh = sum(util_mWh, na.rm = TRUE),
    total_commercial_mWh = sum(util_mWh[sector_mapped == "commercial"], na.rm = TRUE),
    total_industrial_mWh = sum(util_mWh[sector_mapped == "industrial"], na.rm = TRUE),
    total_residential_mWh = sum(util_mWh[sector_mapped == "residential"], na.rm = TRUE),
    commercial_city = first(commercial_city), # Values are repeated, so take the first
    industrial_city = first(industrial_city),
    residential_city = first(residential_city),
    .groups = "drop"
  ) %>%
  mutate(
    actual_commercial_prop = total_commercial_mWh / total_util_mWh,
    actual_industrial_prop = total_industrial_mWh / total_util_mWh,
    actual_residential_prop = total_residential_mWh / total_util_mWh
  ) %>%
  left_join(ctu_commDesg,
            by = join_by(ctu_name)
  ) %>%
  left_join(dissolved_ctu,
            by = join_by(ctu_name)
  ) %>%
  st_as_sf()




# Assessment and analysis of Xcel data v. NREL data

# Data preparation: Transform data to long format, enrich with sector/metric source, calculate diffs for visualization
data_city_year_sector_props <- complete_city_NREL_comparison %>%
  st_transform(crs = 32615) %>% # Projected CRS (UTM Zone 15N for the Twin Cities area)
  # Pivot to long format to separate sectors and actual/modeled
  pivot_longer(
    cols = c(
      residential_city, commercial_city, industrial_city,
      actual_residential_prop, actual_commercial_prop, actual_industrial_prop
    ),
    names_to = "key",
    values_to = "value"
  ) %>%
  mutate(
    sector = case_when(
      str_detect(key, "residential") ~ "Residential",
      str_detect(key, "commercial") ~ "Commercial",
      str_detect(key, "industrial") ~ "Industrial"
    ),
    data_type = case_when(
      str_detect(key, "actual") ~ "Actual",
      TRUE ~ "Modeled"
    )
  )


data_city_year_sector_propDiffs <- data_city_year_sector_props %>%
  # Pivot wider to create columns for Modeled and Actual values
  pivot_wider(names_from = data_type, values_from = value) %>%
  group_by(ctu_name, year, sector, community_designation, geometry) %>%
  summarize(
    Modeled = mean(Modeled, na.rm = TRUE),
    Actual = mean(Actual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    difference = Modeled - Actual
  )

# Remove year and take an overall look at trends in modeled v. actuals by community designation (averages all years -- biases cities with more data)
commDesg_diff_agg <- data_city_year_sector_propDiffs %>%
  st_drop_geometry() %>%
  group_by(community_designation, sector) %>%
  summarize(
    avgPropDiff = mean(ifelse(is.na(difference), 0, difference), na.rm = TRUE),
    .groups = "drop"
  )



# #PLOT ONE -- Bar graphs showing Average Proportion Differences by Community Designation and Sector
# ggplot(commDesg_diff_agg, aes(x = community_designation, y = avgPropDiff, fill = sector)) +
#   geom_bar(stat = "identity", position = "dodge") +  # Side-by-side bars
#   scale_y_continuous() +  # Allow positive and negative values
#   labs(
#     title = "Average Proportion Differences by Community Designation and Sector",
#     x = "Community Designation",
#     y = "Average Proportion Difference",
#     fill = "Sector"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
#   )
#
#
# # PLOT TWO -- Map showing Spatial Distribution of Proportion Differences (Modeled - Actual)
# ggplot(data_city_year_sector_propDiffs) +
#   geom_sf(aes(geometry = geometry, fill = difference), color = "black") +
#   geom_sf_text(aes(geometry = geometry, label =  ctu_name), size = 3) +
#   scale_fill_distiller(palette = "RdBu", oob = scales::squish) +
#   facet_wrap(~sector) +
#   labs(
#     title = "Spatial Distribution of Proportion Differences (Modeled - Actual)",
#     fill = "Difference"
#   ) +
#   theme_minimal()
#
# # PLOT THREE - Scatter plots comparing Modeled vs Actual Proportions by Sector
# ggplot(data_city_year_sector_propDiffs, aes(x = Actual, y = Modeled, color = community_designation)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   facet_wrap(~sector, scales = "free") +
#   labs(
#     title = "Modeled vs Actual Proportions by Sector",
#     x = "Actual Proportion",
#     y = "Modeled Proportion",
#     color = "Community Designation"
#   ) +
#   theme_minimal()
#
