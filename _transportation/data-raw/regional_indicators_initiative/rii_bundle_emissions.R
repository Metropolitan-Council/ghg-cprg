# Bundle onroad datasets to send to Becky and Ingrid at the Regional Indicators Initiative (RII)

source("R/_load_pkgs.R")
# load in datasets
epa_onroad_emissions_compile <- readRDS("_transportation/data/epa_onroad_emissions_compile.RDS")
epa_onroad_emissions_compile_meta <- readRDS("_transportation/data/epa_onroad_emissions_compile_meta.RDS")
epa_onroad_source_set <- readRDS("_transportation/data/epa_onroad_source_set.RDS") %>%
  mutate(emissions_year = as.numeric(emissions_year))

onroad_emissions <- readRDS("_transportation/data/onroad_emissions.RDS")
onroad_emissions_meta <- readRDS("_transportation/data/onroad_emissions_meta.RDS")

# make a new timestamped folder
file_path_today <- paste0(
  "_transportation/data-raw/regional_indicators_initiative/",
  Sys.Date(), "_transportation_emissions"
)

fs::dir_create(path = file_path_today)

# write out all our datasets
write.csv(epa_onroad_emissions_compile,
  file = paste0(file_path_today, "/epa_onroad_emissions_compile.CSV"),
  row.names = FALSE
)

write.csv(epa_onroad_emissions_compile_meta,
  file = paste0(file_path_today, "/epa_onroad_emissions_compile_meta.CSV"),
  row.names = FALSE
)

write.csv(epa_onroad_source_set,
  file = paste0(file_path_today, "/epa_onroad_source_set.CSV"),
  row.names = FALSE
)

write.csv(onroad_emissions,
  file = paste0(file_path_today, "/onroad_emmissions.CSV"),
  row.names = FALSE
)

write.csv(onroad_emissions_meta,
  file = paste0(file_path_today, "/onroad_emissions_meta.CSV"),
  row.names = FALSE
)

# copy over epa_downloads.xlsx
fs::file_copy("_transportation/data-raw/epa/epa_downloads.xlsx",
  paste0(file_path_today, "/"),
  overwrite = TRUE
)


fs::file_copy("_transportation/data-raw/regional_indicators_initiative/README",
  paste0(file_path_today, "/"),
  overwrite = TRUE
)

# zip our timestamped folder

zip(
  zipfile = paste0(file_path_today, ".zip"),
  files = file_path_today,
  # store files, not full parent directory structure
  flags = "-r9Xj"
)
