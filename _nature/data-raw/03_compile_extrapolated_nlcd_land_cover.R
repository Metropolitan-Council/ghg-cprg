rm(list = ls())
source("R/_load_pkgs.R")
source("R/cprg_colors.R")

overwrite_RDS <- TRUE


# Load the land cover dataset you just created
nlcd_county <- readRDS("_nature/data-raw/nlcd_county_landcover_allyrs_tmp.RDS") %>% ungroup()
nlcd_ctu <- readRDS("_nature/data-raw/nlcd_ctu_landcover_allyrs_tmp.RDS") %>% ungroup()

# Here we have data from 2001 to 2022, with tree canopy data spanning 2011 to 2021
# During the years in which tree canopy is available, we re-classify the NLCD land cover as follows:
# - when tree canopy > 0 and the pixel is classified as "Developed_[any]" we re-classify as Urban_Tree
# - when tree canopy = 0 and the pixel is classified as "Developed_Open" we re-classify as Urban_Grassland
# - when tree canopy = 0 and the pixel is classified as "Developed_[Low,Med,High]" we keep as is

# For the pixels re-classified as Urban_Tree, we take the percent tree cover and multiply it by the area of the pixel
# to get the area of tree cover in that pixel. The residual area is then classified as Urban_Grassland (IF the original
# classification was "Developed_Open"), otherwise, we keep the "Developed_[Low,Med,High]" classification for the residual.

# This creates an issue for when we DON'T have tree canopy data, as we don't know how much of the area is tree cover.
# One solution to this is to take the relative proportions of Urban_Tree, Urban_Grassland, and Developed_[Low,Med,High]
# from the years in which we DO have tree canopy data, and backcast those proportions to the years in which we don't have
# tree canopy data.

# However, we also want to preserve the actual proportions of "Developed_[Low,Med,High]" and "Developed_Open" in the years
# in which we DON'T have tree canopy data, yet we will also eliminate the "Developed_Open" classification.
# This creates a unique problem, as we need to re-classify the "Developed_Open" pixels into either Urban_Grassland or Urban_Tree.

# Importantly, we will not change any features outside of the Developed_[any] classification (including Urban_Tree and Urban_Grassland).

# First, let's grab the proportions of Urban_Tree, Urban_Grassland, and Developed_[Low,Med,High] for 2011, the first year that
# tree canopy data is available. We will use these proportions to backcast the proportions for the years in which we don't have
# tree canopy data.


# COUNTY LEVEL DATA -------------------------------------------------------
# Calculate reference proportions during tree canopy years ----------------

tree_start_year <- nlcd_county %>%
  filter(tcc_available) %>% head(1) %>% pull(inventory_year)

tree_end_year <- nlcd_county %>%
  filter(tcc_available) %>% tail(1) %>% pull(inventory_year)


# Reference proportions from the first year with tree canopy data (2011)
reference_proportions_start_county <- nlcd_county %>%
  filter(inventory_year == tree_start_year) %>%
  group_by(county_id, county_name, state_name, inventory_year) %>%
  summarise(
    # Compute the total area covered by Developed classes
    totalDevelopedArea_ref = sum(area[land_cover_type %in% c("Urban_Tree", "Urban_Grassland", "Developed_Low", "Developed_Med", "Developed_High")]),
    # Compute only the Developed_[Low,Med,High] classes
    lmhDevelopedArea_ref = sum(area[land_cover_type %in% c("Developed_Low", "Developed_Med", "Developed_High")]),
    # Compute the ratio of Developed_[Low,Med,High] classes relative to total
    lmhRatio_ref = lmhDevelopedArea_ref / totalDevelopedArea_ref,
    propUrbanTree_ref = sum(area[land_cover_type == "Urban_Tree"]) / totalDevelopedArea_ref,
    propUrbanGrass_ref = sum(area[land_cover_type == "Urban_Grassland"]) / totalDevelopedArea_ref,
    propLow_ref = sum(area[land_cover_type == "Developed_Low"]) / totalDevelopedArea_ref,
    propMed_ref = sum(area[land_cover_type == "Developed_Med"]) / totalDevelopedArea_ref,
    propHigh_ref = sum(area[land_cover_type == "Developed_High"]) / totalDevelopedArea_ref,
  .groups="keep") %>% 
  mutate(TOT = sum(propUrbanTree_ref,propUrbanGrass_ref,propLow_ref,propMed_ref,propHigh_ref)) %>% ungroup()



# Reference proportions from the last year with tree canopy data (2021)
reference_proportions_end_county <- nlcd_county %>%
  filter(inventory_year == tree_end_year) %>%
  group_by(county_id, county_name, state_name, inventory_year) %>%
  summarise(
    # Compute the total area covered by Developed classes
    totalDevelopedArea_ref = sum(area[land_cover_type %in% c("Urban_Tree", "Urban_Grassland", "Developed_Low", "Developed_Med", "Developed_High")]),
    # Compute only the Developed_[Low,Med,High] classes
    lmhDevelopedArea_ref = sum(area[land_cover_type %in% c("Developed_Low", "Developed_Med", "Developed_High")]),
    # Compute the ratio of Developed_[Low,Med,High] classes relative to total
    lmhRatio_ref = lmhDevelopedArea_ref / totalDevelopedArea_ref,
    propUrbanTree_ref = sum(area[land_cover_type == "Urban_Tree"]) / totalDevelopedArea_ref,
    propUrbanGrass_ref = sum(area[land_cover_type == "Urban_Grassland"]) / totalDevelopedArea_ref,
    propLow_ref = sum(area[land_cover_type == "Developed_Low"]) / totalDevelopedArea_ref,
    propMed_ref = sum(area[land_cover_type == "Developed_Med"]) / totalDevelopedArea_ref,
    propHigh_ref = sum(area[land_cover_type == "Developed_High"]) / totalDevelopedArea_ref,
    .groups="keep") %>% 
  mutate(TOT = sum(propUrbanTree_ref,propUrbanGrass_ref,propLow_ref,propMed_ref,propHigh_ref)) %>% ungroup()




# Calculate land cover proportions during non-tree years ------------------

# Here we've computed the total area of the Developed classes, as well as the area of the Developed_[Low,Med,High] classes
# which allows us to compute the ratio of Developed_[Low,Med,High] classes relative to total. For the years where we don't have tree canopy data,
# let's assume that the ratio of Developed_[Low,Med,High] classes relative to the total Developed area is constant.
# We'll apply this ratio to the total area of the Developed classes in the years where we don't have tree canopy data (the residual will be divided into Urban_Grassland and Urban_Tree).
# However, we'll retain the relative proportions of Developed_Low, Developed_Med, and Developed_High in the years where we don't have tree canopy data.

# In the end, the off years (years without tree data) should retain the same total Developed area as it started with,
# but the proportions of Urban_Tree, Urban_Grassland, and Developed_[Low,Med,High] will be based on the proportions from the year with tree canopy data.
# The proportions of Developed_[Low,Med,High] will be retained in the years where we don't have tree canopy data.
# Now, let's compute the proportions of Developed_[Low,Med,High] for the years where we don't have tree canopy data

actual_proportions_before_county <- nlcd_county %>% dplyr::select(-total_area) %>%
  filter(inventory_year < tree_start_year) %>%
  filter(grepl("Developed", land_cover_type)) %>%
  group_by(county_id, county_name, state_name, inventory_year) %>%
  summarise(totalDevelopedArea_orig = sum(area[land_cover_type %in% c("Developed_Open", "Developed_Low", "Developed_Med", "Developed_High")]),
            lmhDevelopedArea_orig = sum(area[land_cover_type %in% c("Developed_Low", "Developed_Med", "Developed_High")]),
            propLow_orig = sum(area[land_cover_type == "Developed_Low"]) / lmhDevelopedArea_orig,
            propMed_orig = sum(area[land_cover_type == "Developed_Med"]) / lmhDevelopedArea_orig,
            propHigh_orig = sum(area[land_cover_type == "Developed_High"]) / lmhDevelopedArea_orig,
            .groups="keep") %>% 
  mutate(TOT = sum(propLow_orig,propMed_orig,propHigh_orig)) %>% ungroup()


actual_proportions_after_county <- nlcd_county %>% dplyr::select(-total_area) %>%
  filter(inventory_year > tree_end_year) %>%
  filter(grepl("Developed", land_cover_type)) %>%
  group_by(county_id, county_name, state_name, inventory_year) %>%
  summarise(totalDevelopedArea_orig = sum(area[land_cover_type %in% c("Developed_Open", "Developed_Low", "Developed_Med", "Developed_High")]),
            lmhDevelopedArea_orig = sum(area[land_cover_type %in% c("Developed_Low", "Developed_Med", "Developed_High")]),
            propLow_orig = sum(area[land_cover_type == "Developed_Low"]) / lmhDevelopedArea_orig,
            propMed_orig = sum(area[land_cover_type == "Developed_Med"]) / lmhDevelopedArea_orig,
            propHigh_orig = sum(area[land_cover_type == "Developed_High"]) / lmhDevelopedArea_orig,
            .groups="keep") %>% 
  mutate(TOT = sum(propLow_orig,propMed_orig,propHigh_orig)) %>% ungroup()




# Recompute developed area during non-tree years --------------------------
rc_DevelopedArea_before_county <- actual_proportions_before_county %>% select(-TOT) %>%
  left_join(reference_proportions_start_county %>% select(-c(TOT,inventory_year)),
            by = join_by(county_id, county_name, state_name)) %>%
  mutate(
    # Using the ratio of LMH to total in 2011, recompute the total LMH area for 
    # years before 2011 by applying this ratio to the total of LMH + Open
    Developed_LMH = lmhRatio_ref*totalDevelopedArea_orig, 
    # Take the proportion of Low,Med,High during the non-tree years and apply them to the new Developed_LMH area
    Developed_Low = propLow_ref*totalDevelopedArea_orig,
    Developed_Med = propMed_ref*totalDevelopedArea_orig,
    Developed_High = propHigh_ref*totalDevelopedArea_orig,
    # check1 = Developed_Low + Developed_Med + Developed_High,
    Urban_Grassland = propUrbanGrass_ref*(totalDevelopedArea_orig),
    Urban_Tree = propUrbanTree_ref*(totalDevelopedArea_orig),
    # check2 = Developed_Low + Developed_Med + Developed_High + Urban_Grassland + Urban_Tree,
         .before=totalDevelopedArea_orig) %>%
  dplyr::select(county_id, county_name, state_name, inventory_year, 
                Developed_Low, Developed_Med, Developed_High, Urban_Grassland, Urban_Tree) %>%
  pivot_longer(cols = c(Developed_Low, Developed_Med, Developed_High, Urban_Grassland, Urban_Tree),
               names_to = "land_cover_type",
               values_to = "area") %>%
  mutate(total_area = NA, tcc_available = FALSE, source = "extrapolated")


rc_DevelopedArea_after_county <- actual_proportions_after_county %>% select(-TOT) %>%
  left_join(reference_proportions_end_county %>% select(-c(TOT,inventory_year)),
            by = join_by(county_id, county_name, state_name)) %>%
  mutate(
    # Using the ratio of LMH to total in 2011, recompute the total LMH area for 
    # years before 2011 by applying this ratio to the total of LMH + Open
    Developed_LMH = lmhRatio_ref*totalDevelopedArea_orig, 
    # Take the proportion of Low,Med,High during the non-tree years and apply them to the new Developed_LMH area
    Developed_Low = propLow_ref*totalDevelopedArea_orig,
    Developed_Med = propMed_ref*totalDevelopedArea_orig,
    Developed_High = propHigh_ref*totalDevelopedArea_orig,
    # check1 = Developed_Low + Developed_Med + Developed_High,
    Urban_Grassland = propUrbanGrass_ref*(totalDevelopedArea_orig),
    Urban_Tree = propUrbanTree_ref*(totalDevelopedArea_orig),
    # check2 = Developed_Low + Developed_Med + Developed_High + Urban_Grassland + Urban_Tree,
    .before=totalDevelopedArea_orig) %>%
  dplyr::select(county_id, county_name, state_name, inventory_year, 
                Developed_Low, Developed_Med, Developed_High, Urban_Grassland, Urban_Tree) %>%
  pivot_longer(cols = c(Developed_Low, Developed_Med, Developed_High, Urban_Grassland, Urban_Tree),
               names_to = "land_cover_type",
               values_to = "area") %>%
  mutate(total_area = NA, tcc_available = FALSE, source = "extrapolated")






# Bind your original data to your recomputed data -------------------------
nlcd_county_before <- rbind(
  rc_DevelopedArea_before_county,
  nlcd_county %>%
    filter(inventory_year < tree_start_year) %>%
    filter(!grepl("Developed", land_cover_type)) %>%
    mutate(source = "nlcd")
) %>%
  group_by(county_id, county_name, state_name, inventory_year) %>%
  mutate(total_area = sum(area)) %>%
  arrange(county_name, inventory_year, land_cover_type)


nlcd_county_after <- rbind(
  rc_DevelopedArea_after_county,
  nlcd_county %>%
    filter(inventory_year > tree_end_year) %>%
    filter(!grepl("Developed", land_cover_type)) %>%
    mutate(source = "nlcd")
) %>%
  group_by(county_id, county_name, state_name, inventory_year) %>%
  mutate(total_area = sum(area)) %>%
  arrange(county_name, inventory_year, land_cover_type)


nlcd_county_rc <- rbind(
  nlcd_county_before,
  nlcd_county %>% 
    filter(inventory_year >= tree_start_year & inventory_year <= tree_end_year) %>% 
    mutate(source="nlcd"),
  nlcd_county_after
) %>%
  arrange(county_name, inventory_year, land_cover_type)



# Check your results ------------------------------------------------------

# # check to make sure that the total developed area by year is the same in our original dataset
# # compared to our extrapolated dataset
# nlcd_county %>% ungroup() %>%
#   filter(grepl("Developed",land_cover_type) | grepl("Urban",land_cover_type)) %>%
#   group_by(county_name, state_name, inventory_year) %>%
#   summarise(areaDeveloped_orig=sum(area),.groups="keep") %>%
#   left_join(
#     nlcd_county_rc %>% ungroup() %>%
#       filter(grepl("Developed",land_cover_type) | grepl("Urban",land_cover_type)) %>%
#       group_by(county_name, state_name, inventory_year) %>%
#       summarise(areaDeveloped_rc=sum(area),.groups="keep")
#   ) %>%
#   ggplot() +
#   geom_point(aes(x=areaDeveloped_orig, y=areaDeveloped_rc))

  
nlcd_county_rc %>%
  ggplot() + theme_minimal() +
  geom_path(aes(x=inventory_year, y=area, color=land_cover_type)) +
  geom_point(data=. %>% filter(source=="extrapolated"), shape=21, fill="white",
             mapping=aes(x=inventory_year, y=area, color=land_cover_type), size=1.2) +
  geom_point(data=. %>% filter(source=="nlcd"), shape=21,
             mapping=aes(x=inventory_year, y=area, color=land_cover_type, fill=land_cover_type), size=1) +
  facet_wrap(~county_name)

  
nlcd_county_rc %>%
  filter(county_name %in% "Ramsey") %>%
  ggplot() + theme_minimal() +
  geom_path(aes(x=inventory_year, y=area, color=land_cover_type)) +
  geom_point(data=. %>% filter(source=="extrapolated"), shape=21, fill="white",
             mapping=aes(x=inventory_year, y=area, color=land_cover_type), size=1.2) +
  geom_point(data=. %>% filter(source=="nlcd"), shape=21,
             mapping=aes(x=inventory_year, y=area, color=land_cover_type, fill=land_cover_type), size=1) +
  
  nlcd_county %>%
  filter(county_name %in% "Ramsey") %>%
  ggplot() + theme_minimal() +
  geom_path(aes(x=inventory_year, y=area, color=land_cover_type)) +
  geom_point(shape=21, 
             mapping=aes(x=inventory_year, y=area, color=land_cover_type, fill=land_cover_type), size=1.2)

  

# Export final data -------------------------------------------------------
# create metadata
nlcd_county_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county_id", class(nlcd_county_rc$county_id), "County ID (5 digit)",
    "county_name", class(nlcd_county_rc$county_name), "County name",
    "state_name", class(nlcd_county_rc$state_name), "State name",
    "inventory_year", class(nlcd_county_rc$inventory_year), "Year",
    "land_cover_type", class(nlcd_county_rc$land_cover_type), "Land cover type from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_county_rc$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled based on the percentage of tree canopy cover within 'Developed' areas",
    "total_area", class(nlcd_county_rc$total_area), "Sum of area from all cover types in square kilometers (by county)",
    "tcc_available", class(nlcd_county_rc$tcc_available), "Indicates whether tree canopy data was available for the current year",
    "source", class(nlcd_county_rc$source), "Indicates whether the area of 'Urban_Tree' or 'Urban_Grassland' is extrapolated or pulled directly from an NLCD layer"
  )



# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  message("Exporting RDS files...")
  
  saveRDS(nlcd_county_rc, paste0("./_nature/data/nlcd_county_landcover_allyrs.rds"))
  saveRDS(nlcd_county_meta, paste0("./_nature/data/nlcd_county_landcover_allyrs_meta.rds"))
  
}













# CTU LEVEL DATA -------------------------------------------------------
# Calculate reference proportions during tree canopy years ----------------

# Reference proportions from the first year with tree canopy data (2011)
reference_proportions_start_ctu <- nlcd_ctu %>%
  filter(inventory_year == tree_start_year) %>%
  group_by(county_id, ctu_id, ctu_name, ctu_class, county_name, state_name, inventory_year) %>%
  summarise(
    # Compute the total area covered by Developed classes
    totalDevelopedArea_ref = sum(area[land_cover_type %in% c("Urban_Tree", "Urban_Grassland", "Developed_Low", "Developed_Med", "Developed_High")]),
    # Compute only the Developed_[Low,Med,High] classes
    lmhDevelopedArea_ref = sum(area[land_cover_type %in% c("Developed_Low", "Developed_Med", "Developed_High")]),
    # Compute the ratio of Developed_[Low,Med,High] classes relative to total
    lmhRatio_ref = lmhDevelopedArea_ref / totalDevelopedArea_ref,
    propUrbanTree_ref = sum(area[land_cover_type == "Urban_Tree"]) / totalDevelopedArea_ref,
    propUrbanGrass_ref = sum(area[land_cover_type == "Urban_Grassland"]) / totalDevelopedArea_ref,
    propLow_ref = sum(area[land_cover_type == "Developed_Low"]) / totalDevelopedArea_ref,
    propMed_ref = sum(area[land_cover_type == "Developed_Med"]) / totalDevelopedArea_ref,
    propHigh_ref = sum(area[land_cover_type == "Developed_High"]) / totalDevelopedArea_ref,
    .groups="keep") %>% 
  mutate(TOT = sum(propUrbanTree_ref,propUrbanGrass_ref,propLow_ref,propMed_ref,propHigh_ref)) %>% ungroup()



# Reference proportions from the last year with tree canopy data (2021)
reference_proportions_end_ctu <- nlcd_ctu %>%
  filter(inventory_year == tree_end_year) %>%
  group_by(county_id, ctu_id, ctu_name, ctu_class, county_name, state_name, inventory_year) %>%
  summarise(
    # Compute the total area covered by Developed classes
    totalDevelopedArea_ref = sum(area[land_cover_type %in% c("Urban_Tree", "Urban_Grassland", "Developed_Low", "Developed_Med", "Developed_High")]),
    # Compute only the Developed_[Low,Med,High] classes
    lmhDevelopedArea_ref = sum(area[land_cover_type %in% c("Developed_Low", "Developed_Med", "Developed_High")]),
    # Compute the ratio of Developed_[Low,Med,High] classes relative to total
    lmhRatio_ref = lmhDevelopedArea_ref / totalDevelopedArea_ref,
    propUrbanTree_ref = sum(area[land_cover_type == "Urban_Tree"]) / totalDevelopedArea_ref,
    propUrbanGrass_ref = sum(area[land_cover_type == "Urban_Grassland"]) / totalDevelopedArea_ref,
    propLow_ref = sum(area[land_cover_type == "Developed_Low"]) / totalDevelopedArea_ref,
    propMed_ref = sum(area[land_cover_type == "Developed_Med"]) / totalDevelopedArea_ref,
    propHigh_ref = sum(area[land_cover_type == "Developed_High"]) / totalDevelopedArea_ref,
    .groups="keep") %>% 
  mutate(TOT = sum(propUrbanTree_ref,propUrbanGrass_ref,propLow_ref,propMed_ref,propHigh_ref)) %>% ungroup()




# Calculate land cover proportions during non-tree years ------------------

# Here we've computed the total area of the Developed classes, as well as the area of the Developed_[Low,Med,High] classes
# which allows us to compute the ratio of Developed_[Low,Med,High] classes relative to total. For the years where we don't have tree canopy data,
# let's assume that the ratio of Developed_[Low,Med,High] classes relative to the total Developed area is constant.
# We'll apply this ratio to the total area of the Developed classes in the years where we don't have tree canopy data (the residual will be divided into Urban_Grassland and Urban_Tree).
# However, we'll retain the relative proportions of Developed_Low, Developed_Med, and Developed_High in the years where we don't have tree canopy data.

# In the end, the off years (years without tree data) should retain the same total Developed area as it started with,
# but the proportions of Urban_Tree, Urban_Grassland, and Developed_[Low,Med,High] will be based on the proportions from the year with tree canopy data.
# The proportions of Developed_[Low,Med,High] will be retained in the years where we don't have tree canopy data.
# Now, let's compute the proportions of Developed_[Low,Med,High] for the years where we don't have tree canopy data

actual_proportions_before_ctu <- nlcd_ctu %>% dplyr::select(-total_area) %>%
  filter(inventory_year < tree_start_year) %>%
  filter(grepl("Developed", land_cover_type)) %>%
  group_by(county_id, ctu_id, ctu_name, ctu_class, county_name, state_name, inventory_year) %>%
  summarise(totalDevelopedArea_orig = sum(area[land_cover_type %in% c("Developed_Open", "Developed_Low", "Developed_Med", "Developed_High")]),
            lmhDevelopedArea_orig = sum(area[land_cover_type %in% c("Developed_Low", "Developed_Med", "Developed_High")]),
            propLow_orig = sum(area[land_cover_type == "Developed_Low"]) / lmhDevelopedArea_orig,
            propMed_orig = sum(area[land_cover_type == "Developed_Med"]) / lmhDevelopedArea_orig,
            propHigh_orig = sum(area[land_cover_type == "Developed_High"]) / lmhDevelopedArea_orig,
            .groups="keep") %>% 
  mutate(TOT = sum(propLow_orig,propMed_orig,propHigh_orig)) %>% ungroup()


actual_proportions_after_ctu <- nlcd_ctu %>% dplyr::select(-total_area) %>%
  filter(inventory_year > tree_end_year) %>%
  filter(grepl("Developed", land_cover_type)) %>%
  group_by(county_id, ctu_id, ctu_name, ctu_class, county_name, state_name, inventory_year) %>%
  summarise(totalDevelopedArea_orig = sum(area[land_cover_type %in% c("Developed_Open", "Developed_Low", "Developed_Med", "Developed_High")]),
            lmhDevelopedArea_orig = sum(area[land_cover_type %in% c("Developed_Low", "Developed_Med", "Developed_High")]),
            propLow_orig = sum(area[land_cover_type == "Developed_Low"]) / lmhDevelopedArea_orig,
            propMed_orig = sum(area[land_cover_type == "Developed_Med"]) / lmhDevelopedArea_orig,
            propHigh_orig = sum(area[land_cover_type == "Developed_High"]) / lmhDevelopedArea_orig,
            .groups="keep") %>% 
  mutate(TOT = sum(propLow_orig,propMed_orig,propHigh_orig)) %>% ungroup()



# Recompute developed area during non-tree years --------------------------
rc_DevelopedArea_before_ctu <- actual_proportions_before_ctu %>% select(-TOT) %>%
  left_join(reference_proportions_start_ctu %>% select(-c(TOT,inventory_year)),
            by = join_by(county_id, ctu_id, ctu_name, ctu_class, county_name, state_name)) %>%
  mutate(
    # Using the ratio of LMH to total in 2011, recompute the total LMH area for 
    # years before 2011 by applying this ratio to the total of LMH + Open
    Developed_LMH = lmhRatio_ref*totalDevelopedArea_orig, 
    # Take the proportion of Low,Med,High during the non-tree years and apply them to the new Developed_LMH area
    Developed_Low = propLow_ref*totalDevelopedArea_orig,
    Developed_Med = propMed_ref*totalDevelopedArea_orig,
    Developed_High = propHigh_ref*totalDevelopedArea_orig,
    # check1 = Developed_Low + Developed_Med + Developed_High,
    Urban_Grassland = propUrbanGrass_ref*(totalDevelopedArea_orig),
    Urban_Tree = propUrbanTree_ref*(totalDevelopedArea_orig),
    # check2 = Developed_Low + Developed_Med + Developed_High + Urban_Grassland + Urban_Tree,
    .before=totalDevelopedArea_orig) %>%
  dplyr::select(county_id, ctu_id, ctu_name, ctu_class, county_name, state_name, inventory_year, 
                Developed_Low, Developed_Med, Developed_High, Urban_Grassland, Urban_Tree) %>%
  pivot_longer(cols = c(Developed_Low, Developed_Med, Developed_High, Urban_Grassland, Urban_Tree),
               names_to = "land_cover_type",
               values_to = "area") %>%
  mutate(total_area = NA, tcc_available = FALSE, source = "extrapolated")


rc_DevelopedArea_after_ctu <- actual_proportions_after_ctu %>% select(-TOT) %>%
  left_join(reference_proportions_end_ctu %>% select(-c(TOT,inventory_year)),
            by = join_by(county_id, ctu_id, ctu_name, ctu_class, county_name, state_name)) %>%
  mutate(
    # Using the ratio of LMH to total in 2011, recompute the total LMH area for 
    # years before 2011 by applying this ratio to the total of LMH + Open
    Developed_LMH = lmhRatio_ref*totalDevelopedArea_orig, 
    # Take the proportion of Low,Med,High during the non-tree years and apply them to the new Developed_LMH area
    Developed_Low = propLow_ref*totalDevelopedArea_orig,
    Developed_Med = propMed_ref*totalDevelopedArea_orig,
    Developed_High = propHigh_ref*totalDevelopedArea_orig,
    # check1 = Developed_Low + Developed_Med + Developed_High,
    Urban_Grassland = propUrbanGrass_ref*(totalDevelopedArea_orig),
    Urban_Tree = propUrbanTree_ref*(totalDevelopedArea_orig),
    # check2 = Developed_Low + Developed_Med + Developed_High + Urban_Grassland + Urban_Tree,
    .before=totalDevelopedArea_orig) %>%
  dplyr::select(county_id, ctu_id, ctu_name, ctu_class, county_name, state_name, inventory_year, 
                Developed_Low, Developed_Med, Developed_High, Urban_Grassland, Urban_Tree) %>%
  pivot_longer(cols = c(Developed_Low, Developed_Med, Developed_High, Urban_Grassland, Urban_Tree),
               names_to = "land_cover_type",
               values_to = "area") %>%
  mutate(total_area = NA, tcc_available = FALSE, source = "extrapolated")






# Bind your original data to your recomputed data -------------------------
nlcd_ctu_before <- rbind(
  rc_DevelopedArea_before_ctu,
  nlcd_ctu %>%
    filter(inventory_year < tree_start_year) %>%
    filter(!grepl("Developed", land_cover_type)) %>%
    mutate(source = "nlcd")
) %>%
  group_by(county_id, ctu_id, ctu_name, ctu_class, county_name, state_name, inventory_year) %>%
  mutate(total_area = sum(area)) %>%
  arrange(ctu_name, ctu_class, inventory_year, land_cover_type)


nlcd_ctu_after <- rbind(
  rc_DevelopedArea_after_ctu,
  nlcd_ctu %>%
    filter(inventory_year > tree_end_year) %>%
    filter(!grepl("Developed", land_cover_type)) %>%
    mutate(source = "nlcd")
) %>%
  group_by(county_id, ctu_id, ctu_name, ctu_class, county_name, state_name, inventory_year) %>%
  mutate(total_area = sum(area)) %>%
  arrange(ctu_name, ctu_class, inventory_year, land_cover_type)


nlcd_ctu_rc <- rbind(
  nlcd_ctu_before,
  nlcd_ctu %>% 
    filter(inventory_year >= tree_start_year & inventory_year <= tree_end_year) %>% 
    mutate(source="nlcd"),
  nlcd_ctu_after
) %>%
  arrange(ctu_name, ctu_class, inventory_year, land_cover_type)

# Check your results ------------------------------------------------------

# # check to make sure that the total developed area by year is the same in our original dataset
# # compared to our extrapolated dataset
# nlcd_ctu %>% ungroup() %>%
#   filter(grepl("Developed",land_cover_type) | grepl("Urban",land_cover_type)) %>%
#   group_by(county_id, ctu_id, ctu_name, ctu_class, county_name, state_name, inventory_year) %>%
#   summarise(areaDeveloped_orig=sum(area),.groups="keep") %>%
#   left_join(
#     nlcd_ctu_rc %>% ungroup() %>%
#       filter(grepl("Developed",land_cover_type) | grepl("Urban",land_cover_type)) %>%
#       group_by(county_id, ctu_id, ctu_name, ctu_class, county_name, state_name, inventory_year) %>%
#       summarise(areaDeveloped_rc=sum(area),.groups="keep")
#   ) %>%
#   ggplot() +
#   geom_point(aes(x=areaDeveloped_orig, y=areaDeveloped_rc))


nlcd_ctu_rc %>%
  ggplot() + theme_minimal() +
  geom_path(aes(x=inventory_year, y=area, color=land_cover_type)) +
  geom_point(data=. %>% filter(source=="extrapolated"), shape=21, fill="white",
             mapping=aes(x=inventory_year, y=area, color=land_cover_type), size=1.2) +
  geom_point(data=. %>% filter(source=="nlcd"), shape=21,
             mapping=aes(x=inventory_year, y=area, color=land_cover_type, fill=land_cover_type), size=1) +
  facet_wrap(~ctu_name)


nlcd_ctu_rc %>%
  filter(ctu_name %in% "Roseville") %>%
  ggplot() + theme_minimal() +
  geom_path(aes(x=inventory_year, y=area, color=land_cover_type)) +
  geom_point(data=. %>% filter(source=="extrapolated"), shape=21, fill="white",
             mapping=aes(x=inventory_year, y=area, color=land_cover_type), size=1.2) +
  geom_point(data=. %>% filter(source=="nlcd"), shape=21,
             mapping=aes(x=inventory_year, y=area, color=land_cover_type, fill=land_cover_type), size=1) +
  
  nlcd_ctu %>%
  filter(ctu_name %in% "Roseville") %>%
  ggplot() + theme_minimal() +
  geom_path(aes(x=inventory_year, y=area, color=land_cover_type)) +
  geom_point(shape=21, 
             mapping=aes(x=inventory_year, y=area, color=land_cover_type, fill=land_cover_type), size=1.2)



nlcd_ctu_rc %>% 
  filter(county_name == "Dakota") %>%
  group_by(inventory_year) %>%
  summarize(total_area = sum(area)) %>% print(n=22)


# Export final data -------------------------------------------------------
# create metadata
nlcd_ctu_meta <-
  tibble::tribble(
    ~"Column", ~"Class", ~"Description",
    "county_id", class(nlcd_ctu_rc$county_id), "County ID (5 digit)",
    "ctu_id", class(nlcd_ctu_rc$ctu_id), "CTU ID",
    "ctu_name", class(nlcd_ctu_rc$ctu_name), "CTU name",
    "ctu_class", class(nlcd_ctu_rc$county_name), "CTU class",
    "county_name", class(nlcd_ctu_rc$county_name), "County name",
    "state_name", class(nlcd_ctu_rc$state_name), "State name",
    "inventory_year", class(nlcd_ctu_rc$inventory_year), "Year",
    "land_cover_type", class(nlcd_ctu_rc$land_cover_type), "Land cover type from National Land Cover Database. 'Urban_' indicates a natural area within NLCD designated developed land cover",
    "area", class(nlcd_ctu_rc$area), "Area of land cover in square kilometers. 'Urban_Tree' is scaled based on the percentage of tree canopy cover within 'Developed' areas",
    "total_area", class(nlcd_ctu_rc$total_area), "Sum of area from all cover types in square kilometers (by CTU)",
    "tcc_available", class(nlcd_ctu_rc$tcc_available), "Indicates whether tree canopy data was available for the current year",
    "source", class(nlcd_ctu_rc$source), "Indicates whether the area of 'Urban_Tree' or 'Urban_Grassland' is extrapolated or pulled directly from an NLCD layer"
  )



# User chooses whether to overwrite the rds files
if (overwrite_RDS) {
  message("Exporting RDS files...")
  
  saveRDS(nlcd_ctu_rc, paste0("./_nature/data/nlcd_ctu_landcover_allyrs.rds"))
  saveRDS(nlcd_ctu_meta, paste0("./_nature/data/nlcd_ctu_landcover_allyrs_meta.rds"))
  
}

rm(list = ls())






