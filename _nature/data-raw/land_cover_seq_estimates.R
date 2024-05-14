source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

wc_county <- readRDS('./_nature/data/county_landcover_2021.rds')
seq_rates <- read_csv('./_nature/data-raw/land_cover_seq_rates.csv')

#sequestration rates require some recalculation given varying units. 
# Further, urban grasslands (usually turf grass) have a C sequestration rate but also are expected emitters of N20
seq_rates_sum <- mutate(seq_rates, mtco2e = if_else(units == 'metric ton C', activity * 3.67,  #scale units carbon to CO2e (atomic weight of a carbon atom compared to CO2 molecule)
                                            if_else(units == 'US ton C', activity * 3.67 * 0.907185, # us ton to metric ton CO2e
                                                  if_else(units == 'kg N', activity * 0.01 * gwp$n2o * 0.001, # IPCC 2019 assumes 1% of nitrogen fertilizer volatilizes to N20
                                    activity)))) %>% 
  ##### convert all seq rates to Mt per 1 sq km
          mutate(sqkm = if_else(area_unit == 'ha', area / 100, 
                                       if_else(area_unit == 'acre', area / 247.1  , NA))) %>% 
  mutate(mtco2e_sqkm = mtco2e / sqkm) %>% 
  filter(!activity == 0.25) %>% # remove hidden carbon cost calculation as it seems to underestimate emissions
  group_by(land_cover_type) %>% 
  summarize(mtco2e_sqkm = sum(mtco2e_sqkm))

wc_county_seq <- left_join(wc_county,seq_rates_sum) %>% 
  filter(mtco2e_sqkm < 0) %>% 
  mutate(sequestration_potential = area  * mtco2e_sqkm)

wc_county_seq_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county ", class(wc_county_seq$county ), "County name",
    "land_cover_type", class(wc_county_seq$land_cover_type), "Land cover type from World Cover. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(wc_county_seq$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled down by NLCD percent impervious",
    "CO2e per square km", class(wc_county_seq$mtco2e_sqkm), "Average regional sequestration rate in CO2 equivalency per square kilometer",
    "land cover sequestration potential", class(wc_county_seq$sequestration_potential), "Annual sequestration potential of land cover type in the county"
  )
  

saveRDS(wc_county_seq, './_nature/data/county_landcover_sequestration_2021.rds')
saveRDS(wc_county_seq_meta, './_nature/data/county_landcover_sequestration_2021_meta.rds')

county_seq_total <- summarize(wc_county_seq, seq_total = sum(sequestration_potential))

county_seq_total

sum(county_seq_total$seq_total) #-2180841
