source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

wc_parks <- readRDS('./_nature/data/park_landcover_2021.rds')
land_cover_c <- readRDS( './_nature/data/land_cover_carbon.rds')


wc_park_c <- left_join(wc_parks,land_cover_c) %>% 
  filter(seq_mtco2e_sqkm < 0) %>% 
  mutate(sequestration_potential = area  * seq_mtco2e_sqkm,
         stock_potential = area * stock_mtco2e_sqkm) %>% 
  dplyr::select(-c(seq_mtco2e_sqkm,stock_mtco2e_sqkm ))


wc_park_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "park", class(wc_county_c$county ), "Park implementing agency",
    "land_cover_type", class(wc_county_c$land_cover_type), "Land cover type from World Cover. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(wc_county_c$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled down by NLCD percent impervious",
    "Carbon sequestration potential", class(wc_county_c$sequestration_potential), "Carbon sequestration potential of park land cover type in metric tons of CO2e per year",
    "Carbon stock potential", class(wc_county_c$stock_potential), "Carbon stock potential of park land cover type in metric tons of CO2e"
  )


saveRDS(wc_park_c, './_nature/data/park_landcover_sequestration_2021.rds')
saveRDS(wc_park_c_meta, './_nature/data/park_landcover_sequestration_2021_meta.rds')

county_seq_total <- summarize(wc_park_c, seq_total = sum(sequestration_potential))
county_stock_total <- summarize(wc_park_c, stock_total = sum(stock_potential))


sum(county_seq_total$seq_total) #-79320.31
sum(county_stock_total$stock_total) #-9550743
