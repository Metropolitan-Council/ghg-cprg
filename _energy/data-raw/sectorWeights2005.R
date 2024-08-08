# MN
# 2005

# tabulapdf read in sector table



# STATE LEVEL residential, commercial, industrial (industrial + farm) proportions BY utility type

MN_elecUtils_2005 <- read_rds(here("_energy", 
                                   "data", 
                                   "distinct_electricity_util_type_MN.RDS"))


mn_elecUtils_2005_consumptionBySector <- MN_elecUtils_2005 %>%
  mutate(
    year = 2005,
    #reported as "non-farm residential"
    res_consumption_mwh = case_when(
      utility_name == "Connexus Energy" ~ 1145680,
      utility_name == "Dakota Electric Assn" ~ 930505,
      utility_name == "East Central Energy" ~ 490578,
      utility_name == "Goodhue County Coop Electric Assn" ~ 28359,
      utility_name == "McLeod Coop Power Assn" ~ 0, #check definition of farm vs industrial...
      utility_name == "Minnesota Valley Electric Coop" ~ 367542,
      utility_name == "Stearns Coop Electric Assn" ~ 189249,
      utility_name == "Wright-Hennepin Coop Electric Assn" ~ 381234,
      utility_name == "Xcel Energy" ~ 8841946,
      utility_name == "City of Anoka" ~ 79324,
      utility_name == "City of Chaska" ~ 73421,
      utility_name == "City of North St Paul" ~ 44195,
      utility_name == "Delano Municipal Utilities" ~ 15585,
      utility_name == "Elk River Municipal Utilities" ~ 71382,
      utility_name == "New Prague Utilities Commission" ~ 16392,
      utility_name == "North Branch Municipal Water & Light" ~ 11966,
      utility_name == "Princeton Public Utilities" ~ 16297,
      utility_name == "Shakopee Public Utilities" ~ 13377,
      TRUE ~ NA_real_
    ),
    #reported as "commercial"
    com_consumption_mwh = case_when(
      utility_name == "Connexus Energy" ~ 664655,
      utility_name == "Dakota Electric Assn" ~ 62921,
      utility_name == "East Central Energy" ~ 235498,
      utility_name == "Goodhue County Coop Electric Assn" ~ 3229,
      utility_name == "McLeod Coop Power Assn" ~ 12063,
      utility_name == "Minnesota Valley Electric Coop" ~ 235766,
      utility_name == "Stearns Coop Electric Assn" ~ 62547,
      utility_name == "Wright-Hennepin Coop Electric Assn" ~ 235607,
      utility_name == "Xcel Energy" ~ 14482254,
      utility_name == "City of Anoka" ~ 87893,
      utility_name == "City of Chaska" ~ 19355,
      utility_name == "City of North St Paul" ~ 33252,
      utility_name == "Delano Municipal Utilities" ~ 4603,
      utility_name == "Elk River Municipal Utilities" ~ 21916,
      utility_name == "New Prague Utilities Commission" ~ 20219, # Used Statewide breakdown to allocate remainder after actual reported figure
      utility_name == "North Branch Municipal Water & Light" ~ 12926,
      utility_name == "Princeton Public Utilities" ~ 14093,
      utility_name == "Shakopee Public Utilities" ~ 873,
      TRUE ~ NA_real_
    ),
    #Includes values reported as "farm" and "industrial" 
    #assumption is that residential energy use is massively outnumbered by farm "productive" uses
    ind_consumption = case_when(
      utility_name == "Connexus Energy" ~ (7988 + 121961),
      utility_name == "Dakota Electric Assn" ~ (9885 + 791563),
      utility_name == "East Central Energy" ~ (3343 + 114731),
      utility_name == "Goodhue County Coop Electric Assn" ~ (50532 + 1115),
      utility_name == "McLeod Coop Power Assn" ~ (104879 + 44438),
      utility_name == "Minnesota Valley Electric Coop" ~ (0 + 0),
      utility_name == "Stearns Coop Electric Assn" ~ (131462 + 20263),
      utility_name == "Wright-Hennepin Coop Electric Assn" ~ (48814 + 68717),
      utility_name == "Xcel Energy" ~ (0 + 8993804),
      utility_name == "City of Anoka" ~ (0 + 98114),
      utility_name == "City of Chaska" ~ (0 + 193934),
      utility_name == "City of North St Paul" ~ (0 + 0),
      utility_name == "Delano Municipal Utilities" ~ (0 + 26746),
      utility_name == "Elk River Municipal Utilities" ~ (203 + 89014),
      utility_name == "New Prague Utilities Commission" ~ (0 + 21642), # used statewide figures to allocate to unreported industrial New Prague figure (Farm was reported as 0)
      utility_name == "North Branch Municipal Water & Light" ~ (0 + 4827),
      utility_name == "Princeton Public Utilities" ~ (0 + 18055),
      utility_name == "Shakopee Public Utilities" ~ (29 + 419),
      TRUE ~ NA_real_
    )
  )


#disaggregate farm to farm residential and farm industrial based on popualtion?

# county x utility x countyPopulation x utilityPop x utilityProportionOfCountyPop (contribution to total)


# vector of res-commercial-industrial proportion in a utility --> assume that is THE mix for a given area...
# need to retain GRE subsidiary .shp


# based on population in utility service area... aggregate




# farm is residential or maybe 50-50?
