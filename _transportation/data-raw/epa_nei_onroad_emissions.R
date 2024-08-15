# fetch more detailed emissions data than what is available in EnviroFacts
source("R/_load_pkgs.R")

if(!file.exists("_transportation/data-raw/epa/nei/2008NEI/2008neiv3_onroad_byregions/2008NEIv3_onroad5.csv")){
  # 2020 
  download.file("https://gaftp.epa.gov/air/nei/2020/data_summaries/2020nei_onroad_byregion.zip",
                "_transportation/data-raw/epa/nei/2020NEI/2020nei_onroad_byregion.zip")
  unzip("_transportation/data-raw/epa/nei/2020NEI/2020nei_onroad_byregion.zip",
        "_transportation/data-raw/epa/nei/2020NEI/2020nei_onroad_byregion/")
  
  # 2017
  download.file("https://gaftp.epa.gov/air/nei/2017/data_summaries/2017v1/2017neiApr_onroad_byregions.zip",
                "_transportation/data-raw/epa/nei/2017NEI/2017neiApr_onroad_byregions.zip")
  unzip(zipfile = "_transportation/data-raw/epa/nei/2017NEI/2017neiApr_onroad_byregions.zip",
        exdir = "_transportation/data-raw/epa/nei/2017NEI/2017neiApr_onroad_byregions/")
  
  # 2014
  download.file("https://gaftp.epa.gov/air/nei/2014/data_summaries/2014v2/2014neiv2_onroad_byregions.zip",
                "_transportation/data-raw/epa/nei/2014NEI/2014neiv2_onroad_byregions.zip")
  unzip("_transportation/data-raw/epa/nei/2014NEI/2014neiv2_onroad_byregions.zip",
        exdir = "_transportation/data-raw/epa/nei/2014NEI/")
  
  # 2011 
  download.file("https://gaftp.epa.gov/air/nei/2011/data_summaries/2011v2/2011neiv2_onroad_byregions.zip",
                "_transportation/data-raw/epa/nei/2011NEI/2011neiv2_onroad_byregions.zip")
  unzip(zipfile = "_transportation/data-raw/epa/nei/2011NEI/2011neiv2_onroad_byregions.zip",
        exdir = "_transportation/data-raw/epa/nei/2011NEI/2011neiv2_onroad_byregions")
  
  # 2008 
  download.file("https://gaftp.epa.gov/air/nei/2008/data_summaries/2008neiv3_onroad_byregions.zip",
                destfile = "_transportation/data-raw/epa/nei/2008NEI/2008neiv3_onroad_byregions.zip")
  unzip("_transportation/data-raw/epa/nei/2008NEI/2008neiv3_onroad_byregions.zip",
        exdir = "_transportation/data-raw/epa/nei/2008NEI/")
}

v2008 <- read.csv("_transportation/data-raw/epa/nei/2008NEI/2008neiv3_onroad_byregions/2008NEIv3_onroad5.csv",
                  colClasses = "character") %>% 
  clean_names() %>% 
  mutate(geoid = state_and_county_fips_code,
         nei_year = "2008",
         pollutant_code = pollutant_cd,
         data_category = data_category_cd,
         emissions_uom = uom,
         pollutant_desc = description,
         data_set = data_set_short_name,
         emissions_operating_type = emissions_op_type_code) %>% 
  filter(geoid %in% county_geography$geoid,
         pollutant_code %in% c("CO2", "CH4", "N2O", "CO", "PM25-PRI", "PM10-PRI"))

v2011 <- read.csv("_transportation/data-raw/epa/nei/2011NEI/2011neiv2_onroad_byregions/onroad_5.csv",
                  colClasses = "character") %>% 
  clean_names() %>% 
  mutate(geoid = state_and_county_fips_code,
         nei_year = "2011",
         pollutant_code = pollutant_cd,
         emissions_uom = uom,
         data_category = data_category_cd,
         data_set = data_set_short_name,
         pollutant_desc = description,
         emissions_operating_type = emissions_op_type_code) %>% 
  filter(geoid %in% county_geography$geoid,
         pollutant_code %in% c("CO2", "CH4", "N2O", "CO", "PM25-PRI", "PM10-PRI"))

v2014 <- read.csv("_transportation/data-raw/epa/nei/2014NEI/2014neiv2_onroad_byregions/onroad_5.csv",
                  colClasses = "character") %>% 
  clean_names() %>% 
  mutate(geoid = state_and_county_fips_code,
         nei_year = "2014",
         pollutant_code = pollutant_cd,
         emissions_uom = uom,
         data_category = data_category) %>% 
  filter(geoid %in% county_geography$geoid,
         pollutant_code %in% c("CO2", "CH4", "N2O", "CO", "PM25-PRI", "PM10-PRI"))

v2017 <- read.csv("_transportation/data-raw/epa/nei/2017NEI/2017neiApr_onroad_byregions/onroad_5.csv",
                  colClasses = "character") %>% 
  clean_names() %>% 
  mutate(geoid = fips_code,
         nei_year = "2017",
         data_category = data_category) %>% 
  filter(geoid %in% county_geography$geoid,
         pollutant_code %in% c("CO2", "CH4", "N2O", "CO", "PM25-PRI", "PM10-PRI"))

v2020 <- read.csv("_transportation/data-raw/epa/nei/2020NEI/2020nei_onroad_byregion/onroad_5.csv",
                  colClasses = "character") %>% 
  clean_names() %>% 
  mutate(geoid = fips_code,
         nei_year = "2020") %>% 
  filter(geoid %in% county_geography$geoid,
         pollutant_code %in% c("CO2", "CH4", "N2O", "CO", "PM25-PRI", "PM10-PRI"))

# combine all 
nei_onroad_emissions <- bind_rows(
  v2008, v2011, v2014, v2017, v2020
) %>% 
  select(-aircraft_engine_type_cd, -tribal_name, -pollutant_cd, -county,
         -uom, -st_usps_cd, -state, -data_category_cd, -fips_state_code,
         -state_and_county_fips_code, -county_name, -description, -emissions_op_type_code,
         -data_set_short_name, -epa_region_code, -fips_code, -emission_operating_type, -sector,
         -emissions_type_code) %>% 
  select(geoid, scc, nei_year, everything()) %>% 
  mutate(total_emissions = as.numeric(total_emissions)) %>% 
  left_join(counties_light) %>% 
  left_join(scc_mobile %>% 
              select(scc, scc6, sector, scc_level_one, 
                     scc_level_two, scc_level_three, scc_level_four,
                     fuel_type_detect, scc_road_type) %>% 
              unique())


nei_onroad_summary <- nei_onroad_emissions %>% 
  group_by(geoid, pollutant_code, emissions_uom, nei_year, sector, fuel_type_detect) %>% 
  summarize(total_emissions = sum(total_emissions))
