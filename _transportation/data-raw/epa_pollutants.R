# compile pollutants from transportation data sources
# results in epa_pollutants_key.RDS and associated metadata
source("R/_load_pkgs.R")
# read in regional summarized datasets
epa_nei_onroad_regional <- readRDS("_transportation/data-raw/epa/epa_nei_onroad_emissions.RDS")
epa_nei_nonroad_regional <- readRDS("_transportation/data-raw/epa/epa_nei_nonroad_emissions.RDS")

# EnviroFacts -----
# pull pollutants from EnviroFacts
# useful for double checking descriptions
req_base <- httr2::request("https://data.epa.gov/efservice")

pollutants <- req_base %>%
  httr2::req_url_path_append("POLLUTANT/CSV") %>%
  httr2::req_method("GET") %>%
  httr2::req_perform() %>%
  httr2::resp_body_string(encoding = "UTF-8") %>%
  readr::read_delim(
    delim = ",",
    col_types = "c",
    show_col_types = FALSE
  ) %>%
  clean_names()

# NEI data -----
# pull pollutants from NEI onroad and nonroad emissions
pollutant_key <-
  bind_rows(
    epa_nei_nonroad_regional %>%
      select(pollutant_code, pollutant_desc),
    epa_nei_onroad_regional %>%
      select(pollutant_code, pollutant_desc)
  ) %>%
  unique() %>%
  mutate(
    pollutant = make_clean_names(pollutant_code),
    unit_of_measurement = "grams"
  ) %>%
  bind_rows(
    # add emissions_metric_tons_co2e
    tibble(
      pollutant = "emissions_metric_tons_co2e",
      pollutant_code = "emissions_metric_tons_co2e",
      pollutant_desc = "Carbon dioxide equivalence, excluding nitrous oxide",
      unit_of_measurement = "metric tons"
    )
  ) %>%
  bind_rows(
    # add nitric oxide
    tibble(
      pollutant = "no",
      pollutant_code = "NO",
      pollutant_desc = "Nitric Oxide",
      unit_of_measurement = "grams"
    )
  ) %>%
  mutate(
    # fill out HTML formatting for future use
    pollutant_format = case_when(
      pollutant_code == "CH4" ~ "CH<sub>4</sub>",
      pollutant_code == "CO" ~ "CO",
      pollutant_code == "CO2" ~ "CO<sub>2</sub>",
      pollutant_code == "N2O" ~ "N<sub>2</sub>O",
      pollutant_code == "NH3" ~ "NH<sub>3</sub>",
      pollutant_code == "SO2" ~ "SO<sub>2</sub>",
      pollutant_code == "NOX" ~ "NOx",
      pollutant_code == "NO" ~ "NO",
      pollutant_code == "PM10-PRI" ~ "PM<sub>2.5</sub>",
      pollutant_code == "PM25-PRI" ~ "PM<sub>10</sub>",
      pollutant_code == "VOC" ~ "VOC",
      pollutant_code == "SF6" ~ "SF<sub>6</sub>",
      pollutant_code == "emissions_metric_tons_co2e" ~ "CO<sub>2</sub>e",
      TRUE ~ pollutant_code
    )
  )


# document and save -----
pollutant_key_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "pollutant_code", class(pollutant_key$pollutant_code), "Pollutant code",
  "pollutant_desc", class(pollutant_key$pollutant_desc), "Full pollutant description",
  "pollutant", class(pollutant_key$pollutant), "Column name friendly pollutant code",
  "unit_of_measurement", class(pollutant_key$unit_of_measurement), "Unit of measurement, grams or metric tons",
  "pollutant_format", class(pollutant_key$pollutant_format), "Pollutant with proper HTML formatting"
)

saveRDS(pollutant_key, "_transportation/data/pollutant_key.RDS")
saveRDS(pollutant_key_meta, "_transportation/data/pollutant_key_meta.RDS")
