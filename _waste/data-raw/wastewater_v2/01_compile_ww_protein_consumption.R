source("R/_load_pkgs.R")


# check that needed data are available locally
if (!file.exists("_waste/data-raw/wastewater_v2/data-raw/EPA_2023_GHG_Emissions_and_Sinks_Table 7-34_Annual_protein_consumption.csv")) {
  cli::cli_abort("Download protein consumption data from MS Teams")
}


## Load Table 7-34 from the EPA US Greenhouse Gas Emissions and Sinks 2023 Report
protein_consumption <- suppressMessages(
  read_csv("_waste/data-raw/wastewater_v2/data-raw/EPA_2023_GHG_Emissions_and_Sinks_Table 7-34_Annual_protein_consumption.csv"
  ))


protein_consumption_formatted <- protein_consumption[c(3:34), c(1,4)] %>%
  rename(year=1, 
         Protein_kg_per_person_per_year=2) %>%
  as_tibble() %>%
  mutate(Protein_kg_per_person_per_year = as.numeric(Protein_kg_per_person_per_year),
         # Percentage of biosolids applied as fertilizer
         pct_of_biosolids_as_fertilizer = 0)
  
protein_consumption_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "year", class(protein_consumption_formatted$year), "Inventory year",
    "Protein_kg_per_person_per_year", class(protein_consumption_formatted$Protein_kg_per_person_per_year), "Annual per capita protein consumption rate",
    "pct_of_biosolids_as_fertilizer", class(protein_consumption_formatted$pct_of_biosolids_as_fertilizer), "Percent of biosolids used as fertilizer"
  )

saveRDS(protein_consumption_formatted, "./_waste/data-raw/wastewater_v2/data/epa_protein_consumption.rds")
saveRDS(protein_consumption_meta, "./_waste/data-raw/wastewater_v2/data/epa_protein_consumption_meta.rds")

rm(list=ls(pattern="^protein_"))


