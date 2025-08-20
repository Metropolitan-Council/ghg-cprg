##### read in and reformat UrbanSim output

source("R/_load_pkgs.R")
cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS")
ctu_population_meta <- read_rds("_meta/data/ctu_population_meta.RDS")

us_meta <- readxl::read_xlsx(
  "_meta/data-raw/urbansim/datadictionary.xlsx"
) %>%
  janitor::clean_names()

### trunk path
us_path <- here::here("_meta", "data-raw", "urbansim")
### list year files
us_list <- list.files(us_path)[1:5]

# function to read and process files
us_format <- function(year_folder) {
  file_full_path <- file.path(us_path, year_folder) # Construct folder path
  files_in_folder <- list.files(file_full_path, full.names = TRUE) # List files in folder

  # Read all files in the folder and bind them
  urbansim_data <- read.csv(files_in_folder) %>%
    pivot_longer(
      cols = 2:112, # Adjust column selection as needed
      names_to = "variable"
    ) %>%
    left_join(us_meta, by = "variable") %>%
    mutate(
      coctu_id_gnis = stringr::str_pad(coctu_id, side = "left", width = 11, pad = "0"),
      ctu_id = stringr::str_sub(coctu_id, -8, -1),
      inventory_year = as.numeric(year_folder)
    )
}

# Read and combine all files, assigning inventory year
urbansim <- lapply(us_list, us_format) %>%
  bind_rows() %>%
  filter(!is.na(status)) %>%
  # only retain variables marked as ready for public display
  filter(status != "needs clarification")


urbansim_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "ctu_id", class(urbansim$ctu_id), "City-township-unorganized identifier",
  "variable", class(urbansim$variable), "Short variable name",
  "value", class(urbansim$value), "County-city value of variable",
  "definition", class(urbansim$definition), "Variable description",
  "broad_category", class(urbansim$broad_category), "Sector grouping of variable",
  "status", class(urbansim$status), "Internal assessment of variable readiness; interpret cautiously where advised",
  "notes", class(urbansim$notes), "Research department notes"
) %>%
  bind_rows(ctu_population_meta) %>%
  filter(Column %in% names(urbansim))


saveRDS(
  urbansim,
  "_meta/data/urbansim_data.RDS"
)
saveRDS(
  urbansim_meta,
  "_meta/data/urbansim_data_meta.RDS"
)
