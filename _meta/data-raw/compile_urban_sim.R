##### read in and reformat UrbanSim output

source("R/_load_pkgs.R")
cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS")
  
# read in urbansim metadata
us_meta <- read_xlsx(
  "N:/CommDev/Research/Research/Forecasts/2050 Forecasts/Draft Preliminary Local Forecasts/Outputs/run_212/datadictionary.xlsx") %>% 
  clean_names()


# read in urbansim data
urbansim_2010 <- read_csv(
  "N:/CommDev/Research/Research/Forecasts/2050 Forecasts/Draft Preliminary Local Forecasts/Outputs/run_212/evolvingCOCTU/inflationDeflation/2010/results_metcouncil_run_212_inflationPostProcess_COCTU_2010.csv") %>% 
  #group_by(coctu_id) %>% 
  #summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop") %>%  # Sum all columns, excluding the grouping variable
  pivot_longer(cols=2:112,
               names_to = "variable") %>%
  left_join(us_meta,
            by= "variable") %>% 
  mutate(ctu_id = as.numeric(substr(as.character(coctu_id), 
                                    nchar(as.character(coctu_id)) - 6, 
                                    nchar(as.character(coctu_id)))),
         inventory_year = 2010)

urbansim_2020 <- read_csv(
  "N:/CommDev/Research/Research/Forecasts/2050 Forecasts/Draft Preliminary Local Forecasts/Outputs/run_212/evolvingCOCTU/inflationDeflation/2020/results_metcouncil_run_212_inflationPostProcess_COCTU_2020.csv") %>% 
  #group_by(coctu_id) %>% 
  #summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop") %>%  # Sum all columns, excluding the grouping variable
  pivot_longer(cols=2:112,
               names_to = "variable") %>%
  left_join(us_meta,
            by= "variable") %>% 
  mutate(ctu_id = as.numeric(substr(as.character(coctu_id), 
                                    nchar(as.character(coctu_id)) - 6, 
                                    nchar(as.character(coctu_id)))),
         inventory_year = 2020)
  
  # left_join(cprg_ctu, by = c("ctu_id" = "gnis")) %>% 
  # filter(!is.na(ctu_name))%>% #Shakopee Mdewakanton Community - revisit if we have utility data
  # st_drop_geometry() %>% select(-geometry) %>% 
  # mutate(inventory_year = 2020)

urbansim <- rbind(urbansim_2010, urbansim_2020) %>% 
  filter(!is.na(status))

urbansim_meta <- tibble::tribble(
  ~"Column", ~"Class", ~"Description",
  "inventory_year", class(urbansim$inventory_year), "Inventory year",
  "coctu_id", class(urbansim$coctu_id), "Unique county-city identifier",
  "ctu_id", class(urbansim$ctu_id), "City-township-unorganized identifier",
  "variable", class(urbansim$variable), "Short variable name",
  "value", class(urbansim$value), "County-city value of variable",
  "definition", class(urbansim$definition), "Variable description",
  "broad_category", class(urbansim$broad_category), "Sector grouping of variable",
  "status", class(urbansim$status), "Internal assessment of variable readiness; interpret cautiously where advised",
  "notes", class(urbansim$notes), "Research department notes")


saveRDS(urbansim,
        "_meta/data/urbansim_data.RDS")
saveRDS(urbansim_meta,
        "_meta/data/urbansim_data_meta.RDS")
