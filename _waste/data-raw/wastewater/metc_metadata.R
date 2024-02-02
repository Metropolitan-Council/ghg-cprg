source("R/_load_pkgs.R")

# metro_emission_sources <- readxl::read_xlsx("_waste/data-raw/wastewater/Table of Emission Sources.xlsx",
#                   sheet = 3,
#                   skip = 1,
#                   col_types = c(rep("text", 6),
#                                 "numeric",
#                                 rep("text", 4)),
#                   )
# write.csv(metro_emission_sources, "_waste/data-raw/wastewater/Table of Emission Sources_long.csv",
#           row.names = FALSE)

metc_sources <- read.csv("_waste/data-raw/wastewater/Table of Emission Sources_long.csv") %>%
  clean_names()

metc_sources
