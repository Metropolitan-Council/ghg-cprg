source("R/_load_pkgs.R")
# from the SIT tool

# check that needed data are available locally
if (!file.exists("_waste/data-raw/wastewater/epa/wastewater-module.xlsx")) {
  cli::cli_abort("Download wastewater data from MS Teams")
}

### going to slice and dice this below due to difficult formatting
ww_control <- suppressMessages(
  readxl::read_xlsx("_waste/data-raw/wastewater/epa/wastewater-module.xlsx",
    sheet = "Control"
  )
)

ww_base_constants <- ww_control[c(96, 98:106), c(2, 4)] %>%
  row_to_names(row_number = 1) %>%
  mutate(value = as.numeric(Default)) %>%
  dplyr::select(-Default) %>%
  rename(description = 1) %>%
  mutate(
    short_text = case_when(
      description == "N2O/N2 (molecular weight ratio)" ~ "N2O_N_MWR",
      description == "C/CO2 (molecular weight ratio)" ~ "CO2_C_MWR",
      description == "Methane GWP*" ~ "CH4_GWP",
      description == "Nitrous Oxide GWP*" ~ "N2O_GWP",
      description == "Number of Days per Year" ~ "n_days_per_year",
      description == "Million Metric Tons / Metric Tons (MMT/MT)" ~ "MMT_per_MT",
      description == "Metric Tons / Kg" ~ "MT_per_kg",
      description == "Liters / Cubic Meter (l/m3)" ~ "L_per_m3",
      description == "Grams / Teragram (g/Tg)" ~ "g_per_Tg"
    )
  ) %>%
  select(value, description, short_text)

ww_base_constants <- rbind(
  ww_base_constants,
  data.frame(value = 1E-6, description = "Grams / Metric Ton", short_text = "g_per_MT")
)



ww_default_constants <- ww_control[c(
  16, 18:20, 22, 24:26, 29, 31:32,
  34, 36:39, 41, 43:46, 48, 49:52, 54, 56:59
), c(1, 2, 4)] %>%
  rename(source = 1, description = 2, value = 3) %>%
  mutate(value = as.numeric(value)) %>%
  fill(source) %>%
  # Remove rows where `description` is NA (these are the headers)
  filter(!is.na(description)) %>%
  mutate(
    source = case_when(
      source == "Municipal Wastewater CH4 Emissions" ~ "municipal_ch4",
      source == "Municipal Wastewater Direct N2O Emissions" ~ "municipal_n2o",
      source == "Municipal Wastewater N2O Emissions from Biosolids" ~ "biosolids_n2o",
      source == "Industrial Wastewater CH4 Emissions - Fruits and Vegetables" ~ "fruit_veg_ch4",
      source == "Industrial Wastewater CH4 Emissions - Red Meat" ~ "red_meat_ch4",
      source == "Industrial Wastewater CH4 Emissions - Poultry" ~ "poultry_ch4",
      source == "Industrial Wastewater CH4 Emissions - Pulp and Paper" ~ "pulp_paper_ch4",
      TRUE ~ source # Keep as is for unmatched cases
    )
  ) %>%
  mutate(
    short_text = case_when(
      description == "Per capita 5-day Biochemical Oxygen Demand (BOD5) (kg/day)" ~ "Per_capita_BOD5",
      description == "Fraction of wastewater BOD5 anaerobically digested" ~ "Fraction_BOD5_anaerobically_digested",
      description == "Emission Factor (Gg CH4/Gg BOD5)" ~ "Emission_Factor_CH4_BOD5",
      description == "Factor non-consumption nitrogen" ~ "Factor_non_consumption_nitrogen",
      description == "Fraction of population not on septic" ~ "Fraction_population_not_on_septic",
      description == "Direct wastewater treatment plant emissions (g N2O/person/year)" ~ "Direct_wwtp_emissions",
      description == "Emission Factor (kg N2O-N/kg sewage N-produced)" ~ "Emission_Factor_N2O_N",
      description == "Fraction of nitrogen in protein (FracNPR)" ~ "Fraction_nitrogen_in_protein",
      str_detect(description, "Wastewater Outflow") ~ paste0("Wastewater_Outflow_", source),
      description == "WW Organic Content - Chemical Oxygen Demand (COD) (g/l)" ~ paste0("COD_", source),
      description == "WW Organic Content - Biochemical Oxygen Demand (BOD) (g/l)" ~ paste0("BOD_", source),
      str_detect(description, "Fraction of COD anaerobically degraded") ~ paste0("Fraction_COD_anaerobically_degraded_", source),
      str_detect(description, "Fraction of BOD anaerobically degraded") ~ paste0("Fraction_BOD_anaerobically_degraded_", source),
      description == "Emission factor (g CH4/g COD)" ~ paste0("Emission_factor_CH4_COD_", source),
      description == "Emission factor (g CH4/g BOD)" ~ paste0("Emission_factor_CH4_BOD_", source)
    )
  ) %>%
  select(value, description, short_text)



ww_constants_formatted <- rbind(ww_base_constants, ww_default_constants)

ww_constants_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "value", class(ww_constants_formatted$value), "Multiplier constant for wastewater emissions calculations",
    "description", class(ww_constants_formatted$description), "Description of use case for multiplier constant",
    "short_text", class(ww_constants_formatted$short_text), "Syntax friendly classifier for coding"
  )

saveRDS(ww_constants_formatted, "./_waste/data-raw/wastewater/epa/epa_wastewater_constants.rds")
saveRDS(ww_constants_meta, "./_waste/data-raw/wastewater/epa/epa_wastewater_constants_meta.rds")


rm(list = ls(pattern = "^ww_"))
