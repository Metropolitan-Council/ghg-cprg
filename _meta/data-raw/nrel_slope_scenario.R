# data from NREL SLOPE emissions scenario planner
# Download from MS Teams location first
# https://maps.nrel.gov/slope/
# Zotero key @schleiferSLOPEScenarioPlanner2023

# This scenario evaluates the effects of business-as-usual projections for the 
# evolution of electricity supply and energy demand sectors, incorporating the 
# potential impacts of the Inflation Reduction Act. The electricity generation 
# mix evolves over time based on existing policies and default market and 
# technology assumptions. In this scenario, customer adoption of electric vehicles 
# and heat pumps expands significantly over time due to a combination of technology 
# cost and performance improvements, as well as incentives from the Inflation Reduction Act.
# The electrification of buildings modeling includes new construction and 
# beneficial retrofit applications, while the electrification of vehicles 
# modeling assumes that vehicles are replaced at the end of their useful lives.



nrel_reference_state <- 
  bind_rows(
    read.csv("_meta/data-raw/nrel_slope_emissions/scenario_planner/minnesota_reference/a53f3fe1e47c31850239f7b18130bb2b10d6fcbfa78789cc3f04d391-CO2-emissions-state.csv") %>% 
      clean_names() %>% 
      mutate(state = "MN"),
    read.csv("_meta/data-raw/nrel_slope_emissions/scenario_planner/wisconsin_reference/3613af67df1f93b92f59917564f5ac243295e4305ffc066fe88a492c-CO2-emissions-state.csv") %>% 
      clean_names() %>% 
      mutate(state = "WI")
  )

read_nrel_scenario <- function(file_path, state, county){
  
  read.csv(file_path) %>% 
    clean_names() %>% 
    mutate(
      state = state,
      county = county
    )
}

nrel_reference_county <- bind_rows(
  read_nrel_scenario("_meta/data-raw/nrel_slope_emissions/scenario_planner/anoka_reference/c03e2f7f40cf72b29fdb11fe595cc6851bf4ed23c592b02d0014c6db-CO2-emissions-county.csv",
                     state = "MN",
                     county = "Anoka"),
  read_nrel_scenario("_meta/data-raw/nrel_slope_emissions/scenario_planner/carver_reference/bec31af0df88e9aee2dc23c8c2824bcafc0853354ff2ff47d3d4c543-CO2-emissions-county.csv",
                     state = "MN",
                     county = "Carver"),
  read_nrel_scenario("_meta/data-raw/nrel_slope_emissions/scenario_planner/chisago_reference/39b909e572184b8a8531342c8a5315589d75204019182c61b0dcc745-CO2-emissions-county.csv",
                     state = "MN",
                     county = "Chisago"),
  read_nrel_scenario("_meta/data-raw/nrel_slope_emissions/scenario_planner/dakota_reference/8a5f4c944361f69342e894c13702c1b743f6e3fc9946dd0f36c9d2b4-CO2-emissions-county.csv",
                     state = "MN",
                     county = "Dakota"),
  read_nrel_scenario("_meta/data-raw/nrel_slope_emissions/scenario_planner/pierce_reference/a4794ae3e72bcab0cd614f877d587092b9f5944e137d9574704875b4-CO2-emissions-county.csv",
                     state = "WI",
                     county = "Pierce"),
  read_nrel_scenario("_meta/data-raw/nrel_slope_emissions/scenario_planner/scott_reference/14e98aa5e7ac6c0c9914193b44c825c680a042941e86ac4697c5b183-CO2-emissions-county.csv",
                     state = "MN",
                     county = "Scott"),
  read_nrel_scenario("_meta/data-raw/nrel_slope_emissions/scenario_planner/sherburne_reference/3fac4bb99b09007be8365f62fb7853db53487c6d8ada61287368e059-CO2-emissions-county.csv",
                     state = "MN",
                     county = "Sherburne"),
  read_nrel_scenario("_meta/data-raw/nrel_slope_emissions/scenario_planner/st_croix_reference/335ef3b33af8dbcd9d62044979ca32a959ebca1d7965689de8ac3edc-CO2-emissions-county.csv",
                     state = "WI",
                     county = "St. Croix"),
  read_nrel_scenario("_meta/data-raw/nrel_slope_emissions/scenario_planner/washington_reference/00ce19fbf4747a8696d1bc68b7590a9a702c270364883d01901ed16b-CO2-emissions-county.csv",
                     state = "MN",
                     county = "Washington"),
)
