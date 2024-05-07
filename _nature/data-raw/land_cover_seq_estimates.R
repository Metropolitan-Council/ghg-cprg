source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

wc_county <- readRDS('./_nature/data/county_landcover_2021.rds')
seq_rates <- read_csv('./_nature/data-raw/land_cover_seq_rates.csv')

wc_county_seq <- left_join(wc_county,seq_rates) %>% 
  filter(!is.na(sequestration_rate)) %>% 
  mutate(area_ha = area * 100, sequestration_potential = area_ha * sequestration_rate)
  
county_seq_total <- summarize(wc_county_seq, seq_total = sum(sequestration_potential))

county_seq_total

sum(county_seq_total$seq_total) #601099.1
