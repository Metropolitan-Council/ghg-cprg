## READ ME
# This script contains functions to run natural systems scenarios, including forest restoration.
# Originally in the development of the CCAP, these functions lived on a branch of ghg.ccap/ccap-graphics
# that branch has since been updated, so the functions themselves are outdated

# the outdated functions are preserved here in their original Fall 2025 form for reproducibility.

#+ logisticGrowth -----
logisticGrowth <- function(t,
                           K,
                           r,
                           t0,
                           start_year,
                           end_year) {
  # Standard logistic growth
  raw_growth <- K / (1 + exp(-r * (t - t0)))
  # Normalize to ensure it starts at 0 and ends at K
  growth_start <- K / (1 + exp(-r * (start_year - t0)))
  growth_end <- K / (1 + exp(-r * (end_year - t0)))
  normalized_growth <- (raw_growth - growth_start) / (growth_end - growth_start) * K
  return(normalized_growth)
}

#+ urban_tree_planting -----
urban_tree_planting <- function(df_hist,
                                df_null,
                                .urban_tree_start,
                                .urban_tree_time,
                                .urban_tree_area_perc) {
  # Input checks
  if (!is.numeric(.urban_tree_area_perc) || .urban_tree_area_perc < 0 || .urban_tree_area_perc > 100) {
    stop(".urban_tree_area_perc must be a number between 0 and 100.")
  }
  if (!is.numeric(.urban_tree_start) || .urban_tree_start < 2025 || .urban_tree_start > 2045) {
    stop(".urban_tree_start must be between 2025 and 2045.")
  }
  if (!is.numeric(.urban_tree_time) || .urban_tree_time < 5 || .urban_tree_time > 30) {
    stop(".urban_tree_time must be between 5 and 30 years.")
  }
  
  # Total Developed area in 2022
  # Want to determine how much developed area is available for tree planting based on
  # the degree of imperviousness (low, medium and high) where low is 20-49% impervious,
  # medium is 50-79% impervious and high is 80-100% impervious.
  
  # Plantable fractions per developed type
  plantable_fraction <- c(
    Developed_Low = 0.30, # 30% plantable area, 70% impervious
    Developed_Med = 0.15, # 15% plantable area, 85% impervious
    Developed_High = 0.05 #  5% plantable area, 95% impervious
  )
  
  # Get last year of inventory
  inventory_end <- df_hist %>% filter(inventory_year == max(inventory_year))
  
  # Calculate plantable area from each class
  developed_vals <- c(
    Developed_Low  = inventory_end$Developed_Low * plantable_fraction["Developed_Low"],
    Developed_Med  = inventory_end$Developed_Med * plantable_fraction["Developed_Med"],
    Developed_High = inventory_end$Developed_High * plantable_fraction["Developed_High"]
  )
  total_plantable <- sum(developed_vals)
  
  area_to_convert <- (.urban_tree_area_perc / 100) * total_plantable
  
  proportions <- developed_vals / total_plantable
  
  future_years <- sort(unique(df_null$inventory_year))
  
  # Early return if area_to_convert is zero
  if (area_to_convert == 0) {
    return(tibble(
      inventory_year = future_years,
      delta_Urban_Tree = rep(0, length(future_years)),
      delta_Developed_Low = rep(0, length(future_years)),
      delta_Developed_Med = rep(0, length(future_years)),
      delta_Developed_high = rep(0, length(future_years)),
      delta_total = rep(0, length(future_years))
    ))
  }
  
  
  # Logistic deltas for each developed class
  deltas <- lapply(names(proportions), function(class_name) {
    sapply(future_years, function(year) {
      if (year < .urban_tree_start) {
        0
      } else {
        reduction <- logisticGrowth(
          t = year,
          K = area_to_convert * proportions[[class_name]],
          r = 4 / .urban_tree_time,
          t0 = .urban_tree_start + .urban_tree_time / 2,
          start_year = .urban_tree_start,
          end_year = .urban_tree_start + .urban_tree_time
        )
        -reduction
      }
    })
  })
  
  # Adjusting the names of deltas to remove class name repetition
  names(deltas) <- c("delta_Developed_Low", "delta_Developed_Med", "delta_Developed_High")
  
  # Convert list to matrix to allow rowSums
  deltas_matrix <- do.call(cbind, deltas)
  
  # Urban_Tree gain is sum of absolute reductions
  delta_Urban_Tree <- rowSums(-deltas_matrix)
  
  # Assemble output tibble
  result <- tibble(
    inventory_year = future_years,
    delta_Urban_Tree = delta_Urban_Tree
  )
  
  # Add deltas for each developed class to result
  for (name in names(deltas)) {
    result[[name]] <- deltas[[name]]
  }
  
  # create mergeable data frame with df_null for new output
  result_out <- result %>%
    pivot_longer(
      cols = starts_with("delta_"),
      names_to = "land_cover",
      values_to = "delta"
    ) %>%
    mutate(
      land_cover = sub("delta_", "", land_cover) # Remove "delta_" prefix
    ) %>%
    right_join(
      df_null %>%
        pivot_longer(
          cols = Bare:TOTAL, # Assuming these are all land cover columns
          names_to = "land_cover",
          values_to = "value"
        ),
      by = c("inventory_year", "land_cover")
    ) %>%
    mutate(
      new_value = if_else(!is.na(delta), value + delta, value)
    ) %>%
    select(-value, -delta) %>%
    rename(value = new_value) %>%
    # turn back to wide form
    pivot_wider(
      names_from = land_cover,
      values_from = value
    )
  
  
  return(result_out)
}


#+ forest_restoration -----
forest_restoration <- function(df_hist,
                               df_null,
                               .restoration_start,
                               .restoration_time,
                               .grassland_area_perc,
                               .bare_area_perc,
                               .cropland_area_perc)  {
 

  future_years <- sort(unique(df_null$inventory_year))
  
  
  # Calculate total area available for restoration
  current_total <- filter(df_hist, inventory_year == max(inventory_year)) %>% pull(TOTAL)
  current_cropland <- filter(df_hist, inventory_year == max(inventory_year)) %>% pull(Cropland)
  current_bare <- filter(df_hist, inventory_year == max(inventory_year)) %>% pull(Bare)
  current_grassland <- filter(df_hist, inventory_year == max(inventory_year)) %>% pull(Grassland)
  current_forest <- filter(df_hist, inventory_year == max(inventory_year)) %>% pull(Tree)
  
  new_cropland <- current_cropland * .cropland_area_perc / 100
  new_bare <- current_bare * .bare_area_perc / 100
  new_grassland <- current_grassland * .grassland_area_perc / 100
  
  
  Cropland <- sapply(future_years, function(year) {
    if (year < .restoration_start) {
      current_cropland
    } else {
      total_reduction <- logisticGrowth(
        t = year,
        K = new_cropland,
        r = 4 / .restoration_time,
        t0 = .restoration_start + .restoration_time / 2,
        start_year = .restoration_start,
        end_year = .restoration_start + .restoration_time
      )
      max(current_cropland - total_reduction, current_cropland - new_cropland)
    }
  })
  
  
  
  Grassland <- sapply(future_years, function(year) {
    if (year < .restoration_start) {
      current_grassland
    } else {
      total_reduction <- logisticGrowth(
        t = year,
        K = new_grassland,
        r = 4 / .restoration_time,
        t0 = .restoration_start + .restoration_time / 2,
        start_year = .restoration_start,
        end_year = .restoration_start + .restoration_time
      )
      max(current_grassland - total_reduction, current_grassland - new_grassland)
    }
  })
  
  
  Bare <- sapply(future_years, function(year) {
    if (year < .restoration_start) {
      current_bare
    } else {
      total_reduction <- logisticGrowth(
        t = year,
        K = new_bare,
        r = 4 / .restoration_time,
        t0 = .restoration_start + .restoration_time / 2,
        start_year = .restoration_start,
        end_year = .restoration_start + .restoration_time
      )
      max(current_bare - total_reduction, current_bare - new_bare)
    }
  })
  
  
  
  result <- tibble(
    inventory_year = future_years,
    delta_Cropland = Cropland - df_null$Cropland,
    delta_Grassland = Grassland - df_null$Grassland,
    delta_Bare = Bare - df_null$Bare,
    delta_Tree = -(Cropland - df_null$Cropland) - (Bare - df_null$Bare) - (Grassland - df_null$Grassland),
    # delta_total = delta_Cropland + delta_Grassland + delta_Bare + delta_Tree
  )
  
  
  land_cover_colnames <- c(
    "Bare",
    "Cropland",
    "Developed_Low",
    "Developed_Med",
    "Developed_High",
    "Grassland",
    "Tree",
    "Urban_Grassland",
    "Urban_Tree",
    "Water",
    "Wetland","TOTAL"
  )
  
  
  
  result_out <- result %>%
    pivot_longer(
      cols = starts_with("delta_"),
      names_to = "land_cover",
      values_to = "delta"
    ) %>%
    mutate(
      land_cover = sub("delta_", "", land_cover) # Remove "delta_" prefix
    ) %>%
    
    right_join(
      df_null %>%
        pivot_longer(
          cols = land_cover_colnames, # Assuming these are all land cover columns
          names_to = "land_cover",
          values_to = "value"
        ),
      by = c("inventory_year", "land_cover")
    ) %>%
    mutate(
      new_value = if_else(!is.na(delta), value + delta, value)
    )  %>%
    select(-value, -delta) %>%
    rename(value = new_value) %>%
    # turn back to wide form
    pivot_wider(
      names_from = land_cover,
      values_from = value
    )
  
  
  return(result_out)
}

#+ wetland_restoration -----
wetland_restoration <- function(df_hist,
                                df_null,
                                .restoration_start,
                                .restoration_time,
                                .wetland_area_perc)  {
  
  future_years <- sort(unique(df_null$inventory_year))
  
  
  # Calculate total area available for restoration
  current_total <- filter(df_hist, inventory_year == max(inventory_year)) %>% pull(TOTAL)
  current_wetland <- filter(df_hist, inventory_year == max(inventory_year)) %>% pull(Wetland)
  
  new_wetland <- current_wetland * .wetland_area_perc / 100
  
  # at this point we know that wetlands need to increase by the specified amount, but
  # we need to decrease other land covers to make room for it. The problem is how to do that
  # in a way that makes sense.
  
  
  Wetland <- sapply(future_years, function(year) {
    if (year < .restoration_start) {
      current_wetland
    } else {
      total_gain <- logisticGrowth(
        t = year,
        K = new_wetland,
        r = 4 / .restoration_time,
        t0 = .restoration_start + .restoration_time / 2,
        start_year = .restoration_start,
        end_year = .restoration_start + .restoration_time
      )
      min(current_wetland + total_gain, current_wetland + new_wetland)
    }
  })
  
  
  result <- tibble(
    inventory_year = future_years,
    delta_Wetland = Wetland - df_null$Wetland
  )
  
  
  
  land_cover_colnames <- c(
    "Bare",
    "Cropland",
    "Developed_Low",
    "Developed_Med",
    "Developed_High",
    "Grassland",
    "Tree",
    "Urban_Grassland",
    "Urban_Tree",
    "Water",
    "Wetland","TOTAL"
  )
  
  
  
  result_out <- result %>%
    pivot_longer(
      cols = starts_with("delta_"),
      names_to = "land_cover",
      values_to = "delta"
    ) %>%
    mutate(
      land_cover = sub("delta_", "", land_cover) # Remove "delta_" prefix
    ) %>%
    
    right_join(
      df_null %>%
        pivot_longer(
          cols = land_cover_colnames, # Assuming these are all land cover columns
          names_to = "land_cover",
          values_to = "value"
        ),
      by = c("inventory_year", "land_cover")
    ) %>%
    mutate(
      new_value = if_else(!is.na(delta), value + delta, value)
    )  %>%
    select(-value, -delta) %>%
    rename(value = new_value) %>%
    # turn back to wide form
    pivot_wider(
      names_from = land_cover,
      values_from = value
    ) %>%
    mutate(
      newTotal = rowSums(across(all_of(land_cover_colnames[-length(land_cover_colnames)])))
    )
  
  
  
  return(result_out)
}



#+ run_scenario_natural_systems -----
run_scenario_natural_systems <- function(tb_inv = natural_systems_data$ctu_lc_inventory,
                                         tb_future = natural_systems_data$ctu_lc_null,
                                         tb_seq = natural_systems_data$land_cover_carbon,
                                         .selected_ctu = "all",
                                         .urban_tree_start = 2025,
                                         .urban_tree_time = 10,
                                         .urban_tree_area_perc = 0,
                                         .l2l_start = 2025,
                                         .l2l_time = 10,
                                         .l2l_area_perc = 0,
                                         .restoration_start = 2025,
                                         .restoration_time = 10,
                                         .restoration_area_perc = 0,
                                         .grassland_area_perc = 0,
                                         .bare_area_perc = 0,
                                         .cropland_area_perc = 0,
                                         .wetland_area_perc = 0,
                                         detail = FALSE) {
  # -------------------------------------------------------------------------
  
  if (.selected_ctu == "all") {
    df_hist <- filter(tb_inv, geog_level == "CITY")
    df_null <- filter(tb_future, geog_level == "CITY")
  } else if (.selected_ctu == "Regional") {
    df_hist <- filter(tb_inv, geog_name == "Regional")
    df_null <- filter(tb_future, geog_name == "Regional")
  } else {
    df_hist <- filter_ctu(tb_inv, .selected_ctu = .selected_ctu)
    df_null <- filter_ctu(tb_future, .selected_ctu = .selected_ctu)
  }
  

  tb01 <- if (.urban_tree_area_perc == 0) {
    df_null
  } else {
    urban_tree_planting(
      df_hist = df_hist,
      df_null = df_null,
      # .selected_ctu = .selected_ctu,
      .urban_tree_start = .urban_tree_start,
      .urban_tree_time = .urban_tree_time,
      .urban_tree_area_perc = .urban_tree_area_perc
    )
  }
  
  
  tb02 <- if (.grassland_area_perc == 0 & .bare_area_perc == 0 & .cropland_area_perc == 0) {
    tb01
  } else {
    forest_restoration(
      df_hist = df_hist,
      df_null = tb01,
      # .selected_ctu = .selected_ctu,
      .restoration_start = .restoration_start,
      .restoration_time = .restoration_time,
      .grassland_area_perc = .grassland_area_perc,
      .bare_area_perc = .bare_area_perc,
      .cropland_area_perc = .cropland_area_perc
    )
  }
  
  
  # here we want a module that will maximize the restoration potential of wetlands
  tb03 <- if (.wetland_area_perc == 0) {
    tb02
  } else {
    wetland_restoration(
      df_hist = df_hist,
      df_null = tb02,
      # .selected_ctu = .selected_ctu,
      .restoration_start = .restoration_start,
      .restoration_time = .restoration_time,
      .wetland_area_perc = .wetland_area_perc
    ) %>% dplyr::select(-newTotal)
  }
  
  
  
  # -------------------------------------------------------------------------
  # store carbon sequestration function output into variable
  carbon_sequestration_out <- rbind(df_hist, tb03) %>%
    dplyr::arrange(inventory_year) %>%
    tidyr::pivot_longer(
      # cols = natural_systems_data$land_cover_carbon$land_cover_type,
      cols = c(
        "Bare", "Cropland", "Developed_Low", "Developed_Med",
        "Developed_High", "Water", "Grassland", "Tree",
        "Urban_Grassland", "Urban_Tree", "Wetland"
      ),
      names_to = "land_cover_type",
      values_to = "area"
    ) %>%
    dplyr::left_join(tb_seq, by = c("land_cover_type")) %>%
    dplyr::mutate(
      value_emissions = area * seq_mtco2e_sqkm,
      value_stock_potential = area * stock_mtco2e_sqkm
    )
  
  
  
  # -------------------------------------------------------------------------
  return(carbon_sequestration_out)
}
