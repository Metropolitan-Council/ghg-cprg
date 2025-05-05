# Process Dakota Electrics data
source("R/_load_pkgs.R")

# Read city data
city_raw <- read_xlsx(here("_energy", "data-raw", "dakotaElectricDataRequest", "METC_DEA_Usage_Breakdown.xlsx")) %>%
  mutate(
    ctu_name = str_to_title(Municipality),
    sector = case_when(
      CUSTGROUP == 'C&I' ~ 'Commercial/Industrial',
      CUSTGROUP == 'COM' ~ 'Commercial',
      CUSTGROUP == 'DEA' ~ 'Dakota Electric Operations',
      CUSTGROUP == 'IRR' ~ 'Irrigation Services',
      CUSTGROUP == 'RES' ~ 'Residential',
    ),
    mwh_delivered = `Usage (kWh)`/ 1000,
    utility = 'Dakota Electric'
  ) %>%
  select(
    ctu_name,
    ctu_class = MunicipalityClass,
    year = ReportingYear,
    sector,
    customer_count = `Count Services`,
    mwh_delivered
  ) %>%
  # Dakota's 2018 data is faulty -- starts in March
  filter(!year == 2018)
