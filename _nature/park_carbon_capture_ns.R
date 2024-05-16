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
    "park", class(wc_park_c$park ), "Park implementing agency",
    "land_cover_type", class(wc_park_c$land_cover_type), "Land cover type from World Cover. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(wc_park_c$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled down by NLCD percent impervious",
    "Carbon sequestration potential", class(wc_park_c$sequestration_potential), "Carbon sequestration potential of park land cover type in metric tons of CO2e per year",
    "Carbon stock potential", class(wc_park_c$stock_potential), "Carbon stock potential of park land cover type in metric tons of CO2e"
  )


saveRDS(wc_park_c, './_nature/data/park_landcover_sequestration_2021.rds')
saveRDS(wc_park_c_meta, './_nature/data/park_landcover_sequestration_2021_meta.rds')

county_seq_total <- summarize(wc_park_c, seq_total = sum(sequestration_potential))
county_stock_total <- summarize(wc_park_c, stock_total = sum(stock_potential))


sum(county_seq_total$seq_total) #-79320.31
sum(county_stock_total$stock_total) #-9550743


#### holding spot for parks graphs. Refine and move to qmd later

parks <- vect('./_nature/data-raw/DBO_RPTAdministrativeBoundary.shp')
cprg_county <- readRDS("_meta/data/cprg_county.RDS")

wc_county_c <- readRDS('./_nature/data/county_landcover_sequestration_2021.rds')
cprg_county_emissions <- readRDS('./_meta/data/cprg_county_emissions.rds')


cprg_7 <- vect(cprg_county %>% filter(!NAME %in% c('Sherburne','Chisago','St. Croix','Pierce')))
cprg_county_emissions_7 <- cprg_county_emissions %>% filter(geog_name %in% cprg_7$NAME)

wc_county_c_7 <- wc_county_c %>% filter(county %in% cprg_7$NAME)

sum(expanse(parks, unit = 'km')) #307.684
sum(expanse(cprg_7, unit = 'km')) #7711.262
sum(expanse(parks, unit = 'km')) / sum(expanse(cprg_7, unit = 'km')) # 3.99%

park_area <- data.frame(agency = parks$AgencyMana, area = expanse(parks, unit = 'km')) %>% group_by(agency) %>%  summarize(area = sum(area))

sum(wc_park_c$sequestration_potential) / sum(wc_county_c_7$sequestration_potential) # 5.56%
sum(wc_park_c$stock_potential) / sum(wc_county_c_7$stock_potential) # 7.19%

wc_park_c <- left_join(wc_park_c,
                       data.frame(park = unique(wc_park_c$park),
                                  county = c('Anoka','Hennepin','Carver','Dakota','Hennepin','Ramsey','Ramsey','Scott','Hennepin','Washington',NA)))

wc_park_c_agg <- wc_park_c %>% filter(!is.na(park)) %>%  group_by(park, county) %>% 
  summarize(area = sum(area),sequestration_potential = sum(sequestration_potential), stock_potential = sum(stock_potential)) %>% 
  mutate(seq_area = sequestration_potential/area, stock_area = stock_potential/area)

comp_graph <- rbind(
  data.frame(type = c('sequestration','sequestration'),
             sector = c('wastewater emissions','parks sequestration'),
             value = c(
               sum(cprg_county_emissions_7$emissions_metric_tons_co2e[cprg_county_emissions_7$category == 'Wastewater']),
               sequestration = -1 * sum(wc_park_c$sequestration_potential))),
  data.frame(type = c('stock','stock'),
  sector = c('transportation emissions','parks carbon stock'),
  value = c(sum(cprg_county_emissions_7$emissions_metric_tons_co2e[cprg_county_emissions_7$sector == 'Transportation']),
                        stock = -1 * sum(wc_park_c$stock_potential))))

ggplot(comp_graph, aes(x = sector, y = value, fill = sector)) + geom_bar(stat = 'identity') + facet_wrap(type~., scales = 'free') + theme_bw()
