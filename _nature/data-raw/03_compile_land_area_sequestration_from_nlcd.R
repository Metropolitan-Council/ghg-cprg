rm(list = ls())
source("R/_load_pkgs.R")

overwrite_RDS <- TRUE

nlcd_county <- readRDS("./_nature/data/nlcd_county_landcover_allyrs.rds")
nlcd_ctu <- readRDS("./_nature/data/nlcd_ctu_landcover_allyrs.rds")
land_cover_c <- readRDS("./_nature/data/land_cover_carbon.rds")


# Compute C sequestration and stock potential for natural systems sectors by county
nlcd_county_c <- nlcd_county %>%
  as_tibble() %>%
  left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
  filter(seq_mtco2e_sqkm < 0) %>%
  mutate(
    sequestration_potential = area * seq_mtco2e_sqkm,
    stock_potential = area * stock_mtco2e_sqkm
  ) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm)) 



# Compute C sequestration and stock potential for natural systems sectors by CTU
nlcd_ctu_c <- nlcd_ctu %>%
  as_tibble() %>%
  left_join(., land_cover_c, by = join_by(land_cover_type)) %>%
  filter(seq_mtco2e_sqkm < 0) %>%
  mutate(
    sequestration_potential = area * seq_mtco2e_sqkm,
    stock_potential = area * stock_mtco2e_sqkm
  ) %>%
  dplyr::select(-c(seq_mtco2e_sqkm, stock_mtco2e_sqkm)) 



nlcd_county_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county_id", class(nlcd_county_c$county_id), "County ID (5 digit)",
    "county_name", class(nlcd_county_c$county_name), "County name",
    "state_name", class(nlcd_county_c$state_name), "State name",
    "inventory_year", class(nlcd_county_c$inventory_year), "Year",
    "land_cover_main", class(nlcd_county_c$land_cover_main), "Land cover type from National Land Cover Database. This column ignores the 'Urban_' designation of land_cover_type (see below)",
    "land_cover_type", class(nlcd_county_c$land_cover_type), "Land cover type from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_county_c$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled based on the percentage of tree canopy cover within 'Developed' areas",
    "total_area", class(nlcd_county_c$total_area), "Sum of area from all cover types in square kilometers (by county)",
    "tcc_available", class(nlcd_county_c$tcc_available), "Indicates whether tree canopy data was available for the current year",
    "source", class(nlcd_county_c$source), "Indicates whether the area of 'Urban_Tree' or 'Urban_Grassland' is extrapolated or pulled directly from an NLCD layer",
    "sequestration_potential", class(nlcd_county_c$sequestration_potential), "Carbon sequestration potential of county land cover type in metric tons of CO2e per year",
    "stock_potential", class(nlcd_county_c$stock_potential), "Carbon stock potential of county land cover type in metric tons of CO2e"
  )



nlcd_ctu_c_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county_id", class(nlcd_ctu_c$county_id), "County ID (5 digit)",
    "ctu_id", class(nlcd_ctu_c$ctu_id), "CTU ID",
    "ctu_name", class(nlcd_ctu_c$ctu_name), "CTU name",
    "ctu_class", class(nlcd_ctu_c$county_name), "CTU class",
    "county_name", class(nlcd_ctu_c$county_name), "County name",
    "state_name", class(nlcd_ctu_c$state_name), "State name",
    "inventory_year", class(nlcd_ctu_c$inventory_year), "Year",
    "land_cover_main", class(nlcd_ctu_c$land_cover_main), "Land cover type from National Land Cover Database. This column ignores the 'Urban_' designation of land_cover_type (see below)",
    "land_cover_type", class(nlcd_ctu_c$land_cover_type), "Land cover type from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_ctu_c$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled based on the percentage of tree canopy cover within 'Developed' areas",
    "total_area", class(nlcd_ctu_c$total_area), "Sum of area from all cover types in square kilometers (by CTU)",
    "tcc_available", class(nlcd_ctu_c$tcc_available), "Indicates whether tree canopy data was available for the current year",
    "source", class(nlcd_ctu_c$source), "Indicates whether the area of 'Urban_Tree' or 'Urban_Grassland' is extrapolated or pulled directly from an NLCD layer",
    "sequestration_potential", class(nlcd_ctu_c$sequestration_potential), "Carbon sequestration potential of county land cover type in metric tons of CO2e per year",
    "stock_potential", class(nlcd_ctu_c$stock_potential), "Carbon stock potential of county land cover type in metric tons of CO2e"
  )



# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  saveRDS(nlcd_county_c, paste0("./_nature/data/nlcd_county_landcover_sequestration_allyrs.rds"))
  saveRDS(nlcd_county_c_meta, paste0("./_nature/data/nlcd_county_landcover_sequestration_allyrs_meta.rds"))

  saveRDS(nlcd_ctu_c, paste0("./_nature/data/nlcd_ctu_landcover_sequestration_allyrs.rds"))
  saveRDS(nlcd_ctu_c_meta, paste0("./_nature/data/nlcd_ctu_landcover_sequestration_allyrs_meta.rds"))
}




# nlcd_ctu_c %>%
#   filter(ctu_name %in% c("Minneapolis", "Edina", "Saint Paul")) %>%
#   ggplot() + theme_minimal() +
#   geom_point(data=. %>% filter(source!= "nlcd"),
#              mapping=aes(x=year,y=-sequestration_potential, fill=source, color=land_cover_type), shape=21) +
#   scale_fill_manual(breaks=c("interpolated", "extrapolated"), values=c("white","gray70")) +
#   ggnewscale::new_scale_fill() +
#   geom_point(data=. %>% filter(source == "nlcd"),
#              mapping=aes(x=year,y=-sequestration_potential, fill=land_cover_type, color=land_cover_type), shape=21) +
#   facet_wrap(~ctu_name)
