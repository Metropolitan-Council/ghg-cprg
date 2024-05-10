source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

wc_county <- readRDS('./_nature/data/county_landcover_2021.rds')
seq_rates <- read_csv('./_nature/data-raw/land_cover_seq_rates.csv')

#sequestration rates require some recalculation given varying units. 
# Further, urban grasslands (usually turf grass) have a C sequestration rate but also are expected emitters of N20
seq_rates<- mutate(seq_rates, mtco2e = if_else(units == 'metric ton C', activity * 3.67,  #scale units carbon to CO2e (atomic weight of a carbon atom compared to CO2 molecule)
                                            if_else(units == 'Tg C', activity * 3.67 * 1e+6, # teragrams carbon to metric ton CO2e
                                                  if_else(units == 'kg N', activity * 0.01 * gwp$n2o * 0.001, # IPCC 2019 assumes 1% of nitrogen fertilizer volatilizes to N20
                                    activity)))) %>% 
  ##### convert all seq rates to Mt per 1 sq km
          mutate(mtco2e_sqkm = if_else(area_unit == 'ha', (mtco2e / area) * 100, 
                                       if_else(area_unit == 'sqkm', mtco2e / area, NA)))

wc_county_seq <- left_join(wc_county,seq_rates) %>% 
  filter(activity < 0) %>% 
  mutate(area_ha = area * 100, sequestration_potential = area_ha * activity * 3.67) # 3.67 is conversion of unit carbon to unit co2e
  
county_seq_total <- summarize(wc_county_seq, seq_total = sum(sequestration_potential))

county_seq_total

sum(county_seq_total$seq_total) #601099.1
