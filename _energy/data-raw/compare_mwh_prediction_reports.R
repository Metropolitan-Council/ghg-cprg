### compare mwh prediction scripts with observed county data

source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")

county_busi_predict <- read_rds("_energy/data-raw/predicted_county_business_mwh.rds")
county_res_predict <- read_rds("_energy/data-raw/predicted_county_residential_mwh.rds")

county_predict <- left_join(
  county_res_predict,
  county_busi_predict
) %>%
  mutate(
    total_mwh_predicted = residential_mwh_predicted + business_mwh_predicted,
    source = "CTU prediction"
  )

### look at total mwh

mwh_county_util <- read_rds("_energy/data/minnesota_elecUtils_ActivityAndEmissions.RDS")

connexus <- mwh_county_util %>% filter(utility == "Connexus Energy")
anoka_util <- mwh_county_util %>% filter(county == "Anoka", year == 2021)

ctu_utility_year <- read_rds("_energy/data/ctu_utility_mwh.RDS")
ctu_connexus <- filter(ctu_utility_year, utility == "Connexus Energy", inventory_year == 2021)

mwh_county <- read_rds("_energy/data/minnesota_elecUtils_ActivityAndEmissions.RDS") %>%
  group_by(year, county) %>%
  summarize(mwh_delivered = sum(mWh_delivered)) %>%
  mutate(source = "Utility reports") %>%
  rename(
    inventory_year = year,
    county_name = county
  )


ggplot(
  rbind(
    mwh_county,
    county_predict %>%
      select(inventory_year,
        county_name,
        source,
        mwh_delivered = total_mwh_predicted
      )
  ) %>%
    filter(inventory_year == 2021),
  aes(x = county_name, y = mwh_delivered, fill = source)
) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7)

mn_mwh <- read_rds("_energy/data/minnesota_elecUtils_ActivityAndEmissions.RDS") %>%
  group_by(county) %>%
  summarize(mwh = sum(mWh_delivered, na.rm = TRUE)) %>%
  mutate(
    sector = "Total",
    source = "Utility Report"
  ) %>%
  filter(!county %in% c("Chisago", "Sherburne", "St. Croix", "Pierce"))


### bring in NREL splits
nrel <- read_rds("_energy/data-raw/nrel_slope/nrel_slope_proportions.RDS")

mn_mwh_sector <- rbind(
  left_join(mn_mwh, nrel %>%
    filter(
      year == 2021,
      source == "Electricity"
    ),
  by = "county"
  ) %>%
    mutate(
      mwh = mwh * residential,
      sector = "Residential"
    ) %>%
    select(county, mwh, sector, source = source.x),
  left_join(mn_mwh, nrel %>%
    filter(
      year == 2021,
      source == "Electricity"
    ),
  by = "county"
  ) %>%
    mutate(
      mwh = mwh * (commercial + industrial),
      sector = "Commercial/Industrial"
    ) %>%
    select(county, mwh, sector, source = source.x)
)

ctu_mwh_predict <- rbind(
  ctu_res_predict %>% st_drop_geometry() %>%
    select(
      ctu_name = ctu_name.x,
      mwh_predicted,
      county_name
    ) %>%
    mutate(sector = "Residential"),
  ctu_nonres_predict %>% st_drop_geometry() %>%
    select(
      ctu_name = ctu_name.x,
      mwh_predicted,
      county_name
    ) %>%
    mutate(sector = "Commercial/Industrial")
) %>%
  mutate(source = "MC Model - RF")


mwh <- rbind(mn_mwh_sector, ctu_mwh_predict %>%
  group_by(county_name, sector, source) %>%
  summarize(mwh = sum(mwh_predicted)) %>%
  rename(county = county_name))

ggplot(mwh, aes(x = source, y = mwh, fill = sector)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7) +
  facet_wrap(~county, scales = "free_y") + # Side-by-side sources
  # scale_fill_manual(values = c("Total" = "blue", "Commercial/Industrial" = "orange")) + # Customize colors
  labs(
    title = "Electricity Usage by County",
    x = "County",
    y = "MWh",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )



varImpPlot(rf_nonres_model)

nonres_simple <- lm(mWh_delivered ~ total_job_spaces, data = electricity_nonres)
summary(nonres_simple) # R2 = 0.9704

plot(mWh_delivered ~ total_job_spaces, data = electricity_nonres)
abline(nonres_simple)

ctu_predict_linear <- rbind(
  cprg_ctu %>%
    left_join(urbansim_res, by = c("gnis" = "ctu_id")) %>%
    left_join(mn_parcel_res, by = c("gnis" = "ctu_id", "ctu_name")) %>%
    mutate(mwh_predicted = predict(res_simple, .)) %>%
    mutate(sector = "Residential") %>%
    select(ctu_name, county_name, mwh_predicted, sector),
  cprg_ctu %>%
    left_join(urbansim_nonres, by = c("gnis" = "ctu_id")) %>%
    # left_join(mn_parcel_res, by = c("gnis" = "ctu_id")) %>%
    mutate(mwh_predicted = predict(nonres_simple, .)) %>%
    mutate(sector = "Commercial/Industrial") %>%
    select(ctu_name, county_name, mwh_predicted, sector)
)


county_predict_linear <- ctu_predict_linear %>%
  filter(!is.na(mwh_predicted)) %>%
  st_drop_geometry() %>%
  group_by(county_name, sector) %>%
  summarize(mwh_predicted = sum(mwh_predicted)) %>%
  mutate(
    # apply emission factor and convert to metric tons
    co2 = (mwh_predicted * eGRID_MROW_emissionsFactor_CO2_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    ch4 = (mwh_predicted * eGRID_MROW_emissionsFactor_CH4_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    n2o = (mwh_predicted * eGRID_MROW_emissionsFactor_N2O_2021) %>%
      units::as_units("lb") %>%
      units::set_units("ton") %>%
      as.numeric(),
    co2e =
      co2 +
        (ch4 * gwp$n2o) +
        (n2o * gwp$n2o)
  )

prediction_comparison_linear <- rbind(
  county_predict_linear %>%
    select(county_name, mwh = mwh_predicted, sector) %>%
    mutate(source = "MC model linear"),
  mn_mwh_sector %>%
    select(county_name = county, mwh, sector) %>%
    filter(county_name %in% county_res_predict$county_name) %>%
    mutate(source = "NREL")
)

mwh_add <- rbind(mwh, county_predict_linear %>%
  select(county = county_name, mwh = mwh_predicted, sector) %>%
  mutate(source = "MC model linear"))

mwh_predict_total <- ggplot(mwh_add, aes(x = source, y = mwh, fill = sector)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7) +
  facet_wrap(~county, scales = "free_y") + # Side-by-side sources
  scale_fill_manual(
    values = c("Residential" = "cornflowerblue", "Commercial/Industrial" = "cornflowerblue"),
    guide = "none"
  ) + # Customize colors
  labs(
    title = "Electricity Usage by County - Total",
    x = "County",
    y = "MWh",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

mwh_predict_total

mwh_predict_res <- ggplot(
  mwh_add %>% filter(sector == "Residential"),
  aes(x = source, y = mwh)
) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7, fill = "firebrick4", col = "black") +
  facet_wrap(~county, scales = "free_y") + # Side-by-side sources
  labs(
    title = "Electricity Usage by County - Residential",
    x = "County",
    y = "MWh",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

mwh_predict_res

mwh_predict_nonres <- ggplot(
  mwh_add %>% filter(sector != "Residential"),
  aes(x = source, y = mwh)
) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.7, fill = "#FFDB58", col = "black") +
  facet_wrap(~county, scales = "free_y") + # Side-by-side sources
  labs(
    title = "Electricity Usage by County - Non-Residential",
    x = "County",
    y = "MWh",
    fill = "Sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

mwh_predict_nonres
