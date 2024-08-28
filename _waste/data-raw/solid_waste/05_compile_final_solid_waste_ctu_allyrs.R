# pull in final RDS
solid_waste <- readRDS(file.path(here::here(), "_waste/data/final_solid_waste_allyrs.RDS"))
# pull in population proportion timeseries

# allocate to ctu by proportion
