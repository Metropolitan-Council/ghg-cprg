# data from NREL SLOPE emissions scenario planner
# Reference scenario downloaded only - no interventions
# CO2 emissions only.
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

# The scenario planning tool is interactive, so we can't download directly
# from a single link
# I downloaded each state and county manually and added each to our MS Team site
# 
source("R/_load_pkgs.R")
if (!file.exists("_meta/data-raw/nrel_slope/scenario_planner/anoka_reference/c03e2f7f40cf72b29fdb11fe595cc6851bf4ed23c592b02d0014c6db-CO2-emissions-county.csv")) {
  cli::cli_abort("You need to download the raw data from MS Teams. Contact a project lead if you need help")
}

# read in state level data -----
nrel_reference_state <-
  bind_rows(
    read.csv("_meta/data-raw/nrel_slope/scenario_planner/minnesota_reference/a53f3fe1e47c31850239f7b18130bb2b10d6fcbfa78789cc3f04d391-CO2-emissions-state.csv") %>%
      clean_names() %>%
      mutate(state = "MN"),
    read.csv("_meta/data-raw/nrel_slope/scenario_planner/wisconsin_reference/3613af67df1f93b92f59917564f5ac243295e4305ffc066fe88a492c-CO2-emissions-state.csv") %>%
      clean_names() %>%
      mutate(state = "WI")
  )

# function to read in and standard cleaning for county level data -----
# the tables within each county folder do not have a county name column
# so we need to add it manually
read_nrel_scenario <- function(file_path, state, county) {
  read.csv(file_path) %>%
    clean_names() %>%
    mutate(
      state = state,
      county = county
    )
}

# read in and bind together all county tables
nrel_reference_county <- bind_rows(
  read_nrel_scenario("_meta/data-raw/nrel_slope/scenario_planner/anoka_reference/c03e2f7f40cf72b29fdb11fe595cc6851bf4ed23c592b02d0014c6db-CO2-emissions-county.csv",
    state = "MN",
    county = "Anoka"
  ),
  read_nrel_scenario("_meta/data-raw/nrel_slope/scenario_planner/carver_reference/bec31af0df88e9aee2dc23c8c2824bcafc0853354ff2ff47d3d4c543-CO2-emissions-county.csv",
    state = "MN",
    county = "Carver"
  ),
  read_nrel_scenario("_meta/data-raw/nrel_slope/scenario_planner/chisago_reference/39b909e572184b8a8531342c8a5315589d75204019182c61b0dcc745-CO2-emissions-county.csv",
    state = "MN",
    county = "Chisago"
  ),
  read_nrel_scenario("_meta/data-raw/nrel_slope/scenario_planner/dakota_reference/8a5f4c944361f69342e894c13702c1b743f6e3fc9946dd0f36c9d2b4-CO2-emissions-county.csv",
    state = "MN",
    county = "Dakota"
  ),
  read_nrel_scenario("_meta/data-raw/nrel_slope/scenario_planner/hennepin_reference/d037c812b9dd6dc0d9b42c6bbb97213172391853a9ac4e83ec5c3628_county.csv",
                     state = "MN",
                     county = "Hennepin"
  ),
  read_nrel_scenario("_meta/data-raw/nrel_slope/scenario_planner/ramsey_reference/4e0b8987e8d3b66893a4e348f049eed5af27c7d000b4e7c1f28cb2eb-CO2-emissions-county.csv",
                     state = "MN",
                     county = "Ramsey"
  ),
  read_nrel_scenario("_meta/data-raw/nrel_slope/scenario_planner/pierce_reference/a4794ae3e72bcab0cd614f877d587092b9f5944e137d9574704875b4-CO2-emissions-county.csv",
    state = "WI",
    county = "Pierce"
  ),
  read_nrel_scenario("_meta/data-raw/nrel_slope/scenario_planner/scott_reference/14e98aa5e7ac6c0c9914193b44c825c680a042941e86ac4697c5b183-CO2-emissions-county.csv",
    state = "MN",
    county = "Scott"
  ),
  read_nrel_scenario("_meta/data-raw/nrel_slope/scenario_planner/sherburne_reference/3fac4bb99b09007be8365f62fb7853db53487c6d8ada61287368e059-CO2-emissions-county.csv",
    state = "MN",
    county = "Sherburne"
  ),
  read_nrel_scenario("_meta/data-raw/nrel_slope/scenario_planner/st_croix_reference/335ef3b33af8dbcd9d62044979ca32a959ebca1d7965689de8ac3edc-CO2-emissions-county.csv",
    state = "WI",
    county = "St. Croix"
  ),
  read_nrel_scenario("_meta/data-raw/nrel_slope/scenario_planner/washington_reference/00ce19fbf4747a8696d1bc68b7590a9a702c270364883d01901ed16b-CO2-emissions-county.csv",
    state = "MN",
    county = "Washington"
  ),
)


length(unique(nrel_reference_county$county))
