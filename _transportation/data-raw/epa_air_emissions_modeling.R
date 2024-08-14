# the EPA publishes data for years in between NEI releases
# these are modeled estimates based on a variety of sources
# 

if(file.exists("")){
  
  download.file("https://gaftp.epa.gov/Air/emismod/2022/v1/2022emissions/onroad_emissions_SMOKE-MOVES_FF10_2022hc_17jul2024.zip",
                "_transportation/data-raw/epa/air_emissions_modeling/2022v1/onroad_emissions_SMOKE-MOVES_FF10_2022hc_17jul2024.zip")

  unzip("_transportation/data-raw/epa/air_emissions_modeling/2022v1/onroad_emissions_SMOKE-MOVES_FF10_2022hc_17jul2024.zip", exdir = "_transportation/data-raw/epa/air_emissions_modeling/2022v1/")  
  
  
  download.file("https://gaftp.epa.gov/Air/emismod/2022/v1/2022emissions/CDBs_2022_20240214.zip",
                "_transportation/data-raw/epa/air_emissions_modeling/2022v1/CDBs_2022_20240214.zip")
}
