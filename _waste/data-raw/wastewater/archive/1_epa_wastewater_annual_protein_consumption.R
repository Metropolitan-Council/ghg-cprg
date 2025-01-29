# rm(list=ls())
source("R/_load_pkgs.R")
overwrite_rds <- TRUE

## Note: These values come from the EPA State Inventory Tool Wastewater Module
## and reflect the annual rate of protein consumption in kg per person per year
## for the years 1990-2021. These values are used to estimate N2O emissions from
## municipal wastewater treatment plants.

## Note: As of 10-14-2024, these values are the same for MN and WI,
## and they appear to be hard-coded into the Tool. If these values change in the future,
## they should be updated here.

## Additional notes from the EPA State Inventory Tool Wastewater Module, Effluent N20 Emissions:

## "Municipal wastewater N₂O emissions from biosolids are calculated by multiplying the total
## annual protein consumption by the nitrogen content of protein and fraction of nitrogen not
## consumed, an N₂O emission factor per metric ton of nitrogen treated, subtracting direct
## emissions, converted to million metric tons carbon equivalent (MMTCE), and converted to
## million metric tons carbon dioxide equivalent (MMTCO₂E). Direct and biosolids N₂O emissions
## are then added to produce an estimate of total municipal wastewater treatment N₂O emissions.
## The methodology and factors used for these calculations are discussed in detail in the
## Wastewater Chapter of the User's Guide. Sewage sludge is often applied to agricultural fields
## as fertilizer. Emissions from this use should be accounted under Agricultural Soil Management.
## The Agriculture Module of the SIT is designed to calculate emissions from sewage sludge applied
## to land, but to be consistent, users should enter the percentage of sewage sludge applied to
## agricultural soils in Column S so that emissions are not double-counted. Currently, there are
## no default data for this percentage."



# Minnesota Municipal Wastewater N2O Emissions - annual protein consumption
# Source: EPA State Inventory Tool Wastewater Module
mn_protein_consumption <-
  as_tibble(
    data.frame(
      # State abbreviation for Minnesota
      State = "MN",
      # Sequence of years from 1990-2021
      Year = seq(1990, 2021),
      # Annual protein consumption in kg per person per year
      # See Inventory of U.S. Greenhouse Gas Emissions and Sinks (U.S. EPA 2023)
      # Table 7-28, for data on annual per capita protein consumption for the United States
      Protein_kg_per_person_per_year = c(
        43.1, 43.5, 43.8, 43.8, 44.6,
        44.2, 44.6, 44.2, 44.6, 45.3,
        45.3, 44.6, 44.9, 45.3, 45.7,
        44.9, 45.3, 44.9, 44.2, 43.8,
        43.8, 42.9, 43.3, 43.3, 43.9,
        44.3, 44.5, 44.7, 44.9, 44.4,
        44.6, 44.6
      ),
      # Percentage of biosolids applied as fertilizer
      pct_of_biosolids_as_fertilizer = rep(0, 32)
    )
  )




# Minnesota Municipal Wastewater N2O Emissions - annual protein consumption
# Source: EPA State Inventory Tool Wastewater Module
## NOTE: These are the same values, but should they need to be changed in the future, do so here
wi_protein_consumption <-
  as_tibble(
    data.frame(
      # State abbreviation for Wisconsin
      State = "WI",
      # Sequence of years from 1990-2021
      Year = seq(1990, 2021),
      # Annual protein consumption in kg per person per year
      # See Inventory of U.S. Greenhouse Gas Emissions and Sinks (U.S. EPA 2023)
      # Table 7-28, for data on annual per capita protein consumption for the United States
      Protein_kg_per_person_per_year = c(
        43.1, 43.5, 43.8, 43.8, 44.6,
        44.2, 44.6, 44.2, 44.6, 45.3,
        45.3, 44.6, 44.9, 45.3, 45.7,
        44.9, 45.3, 44.9, 44.2, 43.8,
        43.8, 42.9, 43.3, 43.3, 43.9,
        44.3, 44.5, 44.7, 44.9, 44.4,
        44.6, 44.6
      ),
      # Percentage of biosolids applied as fertilizer
      pct_of_biosolids_as_fertilizer = rep(0, 32)
    )
  )

# Combine the data frames for MN and WI
epa_protein_consumption <-
  rbind(mn_protein_consumption, wi_protein_consumption)


# Save the data frame as an RDS file
if (overwrite_rds) {
  # Export the updated data frame as an RDS file
  saveRDS(epa_protein_consumption, "_waste/data-raw/wastewater_v2/data-raw/epa_protein_consumption.RDS")
}
