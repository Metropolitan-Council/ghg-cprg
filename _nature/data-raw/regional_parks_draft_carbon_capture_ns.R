source("R/_load_pkgs.R")
source("R/global_warming_potential.R")

wc_parks <- readRDS("./_nature/data/park_landcover_draft_2021.rds")
land_cover_c <- readRDS("./_nature/data/land_cover_carbon.rds")


wc_park_c <- left_join(wc_parks, land_cover_c) %>%
  filter(seq_mtco2e_sqkm < 0) %>%
  mutate(
    sequestration_potential = area * seq_mtco2e_sqkm,
    stock_potential = area * stock_mtco2e_sqkm
  ) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm))

# assign park implementing agencies to county
wc_park_c <- left_join(
  wc_park_c,
  data.frame(
    park = unique(wc_park_c$park),
    county = c(
      "Anoka", "Hennepin", "Carver", "Dakota", "Hennepin",
      "Ramsey", "Ramsey", "Scott", "Hennepin", "Washington", NA
    )
  )
)

### acquisition type by seq potential

wc_park_c_agg <- wc_park_c %>%
  filter(!is.na(park)) %>%
  group_by(park, park_implementa) %>%
  summarize(
    area = sum(area),
    sequestration_potential = sum(sequestration_potential),
    stock_potential = sum(stock_potential)
  ) %>%
  mutate(seq_area = sequestration_potential / area, stock_area = stock_potential / area)


ggplot(wc_park_c_agg, aes(x = park_implementa, y = sequestration_potential)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  facet_wrap(park ~ ., scales = "free_y") +
  xlab("Acquisition status") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

### land type by planned acq

wc_park_area <- wc_parks %>%
  filter(!is.na(park), park_implementa == "Planned") %>%
  mutate(land_cover_type = if_else(land_cover_type %in% c(
    "Grassland", "Urban_Grassland", "Urban_Tree", "Tree", "Wetland"
  ), "Natural area", land_cover_type)) %>% 
  group_by(park, land_cover_type) %>%
  summarize(area = sum(area))

wc_park_area
