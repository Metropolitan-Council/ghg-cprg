##### read in an reformat UrbanSim output

source("R/_load_pkgs.R")
cprg_ctu <- read_rds("_meta/data/cprg_ctu.RDS") %>% 
  

us_meta <- read_xlsx(
  "N:/CommDev/Research/Research/Forecasts/2050 Forecasts/Draft Preliminary Local Forecasts/Outputs/run_212/datadictionary.xlsx") %>% 
  mutate(use = case_when( # these are at various levels of vetting
    Status == "ready" ~ "Y",
    grepl("internal",Status) ~ "Y",
    Status == "do not use" ~ "N",
    Status == "needs clarification" ~ "N",
  ))
  
# bin possible variables of use for various sectors
residential <- c("total_households",
                 "total_residential_units",
                 "total_pop",
                 "max_detached",
                 "max_lrglot",
                 "max_attached",
                 "max_multifam",
                 "manufactured_homes",
                 "single_fam_det_sl_own",
                 "single_fam_det_ll_own",
                 "single_fam_det_rent",
                 "single_fam_attached_own",
                 "single_fam_attached_rent",
                 "multi_fam_own",
                 "multi_fam_rent")

commercial <- c("total_job_spaces",
                "max_office",
                "max_commercial",
                "max_institutional",
                "max_school",
                "js_type_1011",
                "js_type_13",
                "js_type_14",
                "jobs_sector_4",
                "jobs_sector_5",
                "jobs_sector_6",
                "jobs_sector_7",
                "jobs_sector_8",
                "jobs_sector_9",
                "jobs_sector_10")

industrial <- c("total_job_spaces",
                "max_industrial",
                "js_type_12",
                "jobs_sector_1",
                "jobs_sector_2",
                "jobs_sector_3")

#collapse to 
urbansim <- read_csv(
  "N:/CommDev/Research/Research/Forecasts/2050 Forecasts/Draft Preliminary Local Forecasts/Outputs/run_212/evolvingCOCTU/inflationDeflation/2020/results_metcouncil_run_212_inflationPostProcess_COCTU_2020.csv") %>% 
  mutate(ctu_id = as.numeric(substr(as.character(coctu_id), nchar(as.character(coctu_id)) - 6, nchar(as.character(coctu_id))))) %>% 
  select(-coctu_id) %>% 
  group_by(ctu_id) %>% # Group by the new variable
  summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop") %>%  # Sum all columns, excluding the grouping variable
  pivot_longer(cols=2:112,
               names_to = "Variable") %>%
  left_join(us_meta %>% select(Variable,use),
            by= "Variable") %>% 
  filter(use == "Y",
         Variable %in% c(residential,commercial,industrial))
  # left_join(cprg_ctu, by = c("ctu_id" = "gnis")) %>% 
  # filter(!is.na(ctu_name))%>% #Shakopee Mdewakanton Community - revisit if we have utility data
  # st_drop_geometry() %>% select(-geometry) %>% 
  # mutate(inventory_year = 2020)

saveRDS(urbansim,
        "_meta/data/urban_sim_2020.RDS")

