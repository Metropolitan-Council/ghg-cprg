source("R/_load_pkgs.R")
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")

ww_raw <- readxl::read_xlsx("_waste/data-raw/wastewater/metc/GHGEmissions_2019-2021_CorrectedWSCA.xlsx") %>%
  clean_names()

metc_ww <- ww_raw %>%
  select(everything(),
    CTU_NAME = city_or_township,
    scope = emissions_scope,
    biogenic = biogenic_emissions_tonnes_co2e,
    anthropogenic = anthropogenic_emissions_tonnes_co2e
  ) %>%
  pivot_longer(c(biogenic, anthropogenic),
    names_to = "class",
    values_to = "emissions_metric_tons_co2e"
  ) %>%
  mutate(
    CTU_NAME = stringr::str_replace(CTU_NAME, "St. ", "Saint "),
    CTU_NAME = case_when(
      CTU_NAME == "White Bear Twp." ~ "White Bear",
      CTU_NAME == "Laketown Twp." ~ "Laketown",
      TRUE ~ CTU_NAME
    )
  ) %>%
  left_join(
    cprg_ctu %>%
      sf::st_drop_geometry() %>%
      select(
        CTU_NAME, CTU_CLASS,
        COUNTY_NAM, STATE,
        STATEFP
      ),
    relationship = "many-to-many",
    by = c("CTU_NAME"),
    multiple = "first"
  )

saveRDS(metc_ww, "_waste/data-raw/wastewater/metc_wastewater.RDS")
