source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

wc_county <- readRDS('./_nature/data/county_landcover_2021.rds')
land_cover_c <- readRDS( './_nature/data/land_cover_carbon.rds')

wc_county_c <- left_join(wc_county,land_cover_c) %>% 
  filter(seq_mtco2e_sqkm < 0) %>% 
  mutate(sequestration_potential = area  * seq_mtco2e_sqkm,
         stock_potential = area * stock_mtco2e_sqkm) %>% 
  dplyr::select(-c(seq_mtco2e_sqkm,stock_mtco2e_sqkm ))

wc_county_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county ", class(wc_county_c$county ), "County name",
    "land_cover_type", class(wc_county_c$land_cover_type), "Land cover type from World Cover. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(wc_county_c$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled down by NLCD percent impervious",
    "Carbon sequestration potential", class(wc_county_c$sequestration_potential), "Carbon sequestration potential of county land cover type in metric tons of CO2e per year",
    "Carbon stock potential", class(wc_county_c$stock_potential), "Carbon stock potential of county land cover type in metric tons of CO2e"
  )


saveRDS(wc_county_c, './_nature/data/county_landcover_sequestration_2021.rds')
saveRDS(wc_county_c_meta, './_nature/data/county_landcover_sequestration_2021_meta.rds')

county_seq_total <- summarize(wc_county_c, seq_total = sum(sequestration_potential))
county_stock_total <- summarize(wc_county_c, stock_total = sum(stock_potential))


sum(county_seq_total$seq_total) #-2180841
sum(county_stock_total$stock_total) #-257,462,245
