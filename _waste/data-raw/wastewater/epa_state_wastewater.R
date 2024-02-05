source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

### MPCA provides waste water emission estimates
mn_mpca <- readxl::read_xlsx("_waste/data-raw/wastewater/mpca-mn-wastewater.xlsx")

mpca_state_ghg <- mn_mpca %>%
  pivot_longer(7:37,
    names_to = "year"
  ) %>%
  filter(year == 2020) %>%
  select(GHGs, year, value)

mn_state_est <- as.numeric(mn_mpca[mn_mpca$Sector == "Grand Total", "2020"])

# mn epa -----
# summary page from the
# State Inventory and Projection Tool
# saved as a CSV
mn_epa <- readr::read_csv("_waste/data-raw/wastewater/epa/epa-mn-wastewater.csv")
mn_epa_est <- as.numeric(mn_epa[12, 33]) * 10^6
mn_epa_ch4_est <- as.numeric(mn_epa[5, 33]) * 10^6
mn_epa_n2o_est <- as.numeric(mn_epa[6, 33]) * 10^6

# wi epa -----
# summary page from
wi_epa <- readr::read_csv("_waste/data-raw/wastewater/epa/epa-wi-wastewater.csv")
wi_epa_est <- as.numeric(wi_epa[12, 33]) * 10^6
wi_epa_ch4_est <- as.numeric(wi_epa[5, 33]) * 10e6
wi_epa_n2o_est <- as.numeric(wi_epa[6, 33]) * 10e6
wi_state_est <- 0.6 * 10^6
# taken from WI state inventory document (https://widnr.widen.net/view/pdf/o9xmpot5x7/AM610.pdf?t.download=true)
# wisconsindnrWisconsinGreenhouseGas2021


cprg_pop <- readRDS(file.path(here::here(), "_meta/data/cprg_population.RDS"))
cprg_county_proportions <- readRDS("_meta/data/cprg_county_proportions.RDS")

wi_2020 <- cprg_county_proportions %>%
  filter(STATE == "Wisconsin",
         year == "2020") %>% 
  mutate(
    epa_co2e = county_proportion_of_state_pop * wi_epa_est,
    state_co2e = county_proportion_of_state_pop * wi_state_est
  )

mn_2020 <- cprg_county_proportions %>%
  filter(STATE == "Minnesota",
         year == "2020") %>% 
  mutate(
    epa_co2e = county_proportion_of_state_pop * mn_epa_est,
    state_co2e = county_proportion_of_state_pop * mn_state_est
  )

ww_epa <- rows_append(
  wi_2020 %>% dplyr::select(GEOID, NAME, epa_co2e, state_co2e),
  mn_2020 %>% dplyr::select(GEOID, NAME, epa_co2e, state_co2e)
)

saveRDS(ww_epa, "_waste/data-raw/wastewater/epa_wastewater.RDS")

# compare MN numbers to met council estimates

metc_wastewater <- readRDS("_waste/data-raw/wastewater/metc_wastewater.RDS")


cty_wastewater <- metc_wastewater %>%
  group_by(year, COUNTY_NAM, class) %>%
  summarize(co2e = sum(emissions_metric_tons_co2e))

county_ww_bio_2020 <- metc_wastewater %>%
  filter(year == 2020 & class == "biogenic") %>%
  group_by(COUNTY_NAM) %>%
  summarize(co2e = sum(emissions_metric_tons_co2e))

comp_2020 <- rows_append(
  cty_wastewater %>%
    filter(year == 2020) %>%
    ungroup() %>%
    select(-year),
  mn_2020 %>%
    mutate(COUNTY_NAM = gsub(" County, Minnesota", "", NAME)) %>%
    select(COUNTY_NAM, epa_co2e, state_co2e) %>%
    pivot_longer(!COUNTY_NAM, names_to = "class", values_to = "co2e")
)

ggplot(
  comp_2020 %>% filter(!COUNTY_NAM %in% c("Chisago", "Sherburne")),
  aes(x = COUNTY_NAM, y = co2e, fill = class)
) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Paired")
