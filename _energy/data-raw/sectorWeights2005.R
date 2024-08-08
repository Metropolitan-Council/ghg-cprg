# MN
# 2005

# tabulapdf read in sector table



# STATE LEVEL residential, commercial, industrial (industrial + farm) proportions BY utility type

MN_elecUtils_2005 <- read_rds(here("_energy", 
                                   "data", 
                                   "distinct_electricity_util_type_MN.RDS"))

# county x utility x countyPopulation x utilityPop x utilityProportionOfCountyPop (contribution to total)


# vector of res-commercial-industrial proportion in a utility --> assume that is THE mix for a given area...
# need to retain GRE subsidiary .shp


# based on population in utility service area... aggregate




# farm is residential or maybe 50-50?
