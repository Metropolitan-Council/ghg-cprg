# Process Xcel Community Reports data
source("R/_load_pkgs.R")
source("_energy/data-raw/_energy_emissions_factors.R")


# read in time series of eGRID emissions factor data and pivot wider to make one row per year with all three emission types
egridTimeSeries <- epa_ghg_factor_hub$egridTimeSeries %>%
  pivot_wider(names_from = emission, values_from = value)

# root directory with folders for each utility in scope (with each folder containing subfolders for all years which reporting to the state is available)
dir_xcel_communityReports <- here("_energy", "data-raw", "xcel_community_reports")


extract_city_name <- function(file_path) {
  # Read the first 3 rows of the Excel file
  data <- read_excel(file_path, range = cell_rows(1:3), col_names = FALSE)

  # Convert the data to a character matrix for easier searching
  data_char <- as.matrix(data)

  # Process row 2 (Excel row 3)
  row2 <- data_char[2, ] # Index 2 since R indexes rows starting at 1

  # Loop through each cell in row 2 to find 'Community'
  for (i in seq_along(row2)) {
    cell_content <- row2[i]
    if (!is.na(cell_content) && grepl("Community", cell_content, ignore.case = TRUE)) {
      # Case 1: City name is in the same cell
      if (grepl("Community[:]?\\s*.+", cell_content, ignore.case = TRUE)) {
        city_name <- sub(".*Community[:]?\\s*", "", cell_content, ignore.case = TRUE)
        city_name <- trimws(city_name)
        if (city_name != "") {
          return(city_name)
        }
      }

      # Case 2: City name is in the next cell(s)
      for (j in (i + 1):length(row2)) {
        next_cell <- row2[j]
        if (!is.na(next_cell) && next_cell != "") {
          city_name <- trimws(next_cell)
          if (city_name != "") {
            return(city_name)
          }
        }
      }
    }
  }

  # Return NA if 'Community' not found or city name is empty
  return(NA)
}

# Function to get file paths, city names, and years of Xcel utility reports in root directory
get_files <- function(root_dir) {
  file_info <- list()

  # Get list of year folders in root_dir (assuming folders are named as years)
  year_folders <- list.dirs(root_dir, recursive = FALSE)

  # Loop through each year folder
  for (year_folder in year_folders) {
    year <- basename(year_folder)

    # Get list of .xls files in the year folder
    files <- list.files(path = year_folder, pattern = "\\.xls$", full.names = TRUE)

    # Loop through each file in the year folder
    for (file in files) {
      # Extract city name from the file
      city_name <- extract_city_name(file)

      # Handle cases where city_name or year is missing
      if (is.na(city_name) || is.na(year)) {
        warning(paste("Missing city name or year in file:", file))
      }

      # Append the file information to the list
      file_info <- append(file_info, list(list(
        file_path = file,
        city_name = unname(city_name),
        year = as.numeric(year),
        utility = "Xcel Energy"
      )))
    }
  }
  return(file_info)
}



# function to dynamically read input from files until a stopping value is found
read_until_value <- function(file_path, sheet, start_cell, stop_value, columns) {
  print(paste("Reading file:", file_path))

  # Step 1: Read a broad range starting from the specified cell
  start_row <- as.numeric(gsub("[A-Z]", "", start_cell)) # Extract the row number from start_cell
  start_col <- gsub("[0-9]", "", start_cell) # Extract the column letter from start_cell
  broad_range <- paste0(start_col, start_row, ":", columns, start_row + 10) # Read 10 rows initially
  print(paste("Broad range:", broad_range))

  # Read the broad range
  data_broad <- read_excel(file_path, sheet = sheet, range = broad_range, col_names = FALSE)
  print(data_broad)

  # Step 2: Locate the stopping value
  stop_row_offset <- which(data_broad[[1]] == stop_value)[1] # Check the first column for stop_value
  print(paste("Stop row offset:", stop_row_offset))

  if (length(stop_row_offset) == 0) {
    stop("Stopping value not found in the specified range.")
  }

  # Step 3: Define the dynamic range
  stop_row <- start_row + stop_row_offset - 1 # Adjust for Excel indexing and remove the total row
  refined_range <- paste0(start_col, start_row, ":", columns, stop_row)
  print(refined_range)

  # Step 4: Read the refined range
  data <- read_excel(file_path, sheet = sheet, range = refined_range, col_names = TRUE)
  print(data)
  return(data)
}

# function to process the file associated with each utility-year combo and extract activity (mWh) at the utility-year-county granularity electricity data
# years 2015 to 2019 have constant format -- 2020 adds more info about renewables and clean energy
process_file <- function(file_info, start_cell) {
  # Extract file path, utility name, and year from file_info (output nested list structure from get_files)
  file_path <- file_info$file_path
  city_name <- file_info$city_name
  utility <- file_info$utility
  year <- file_info$year

  # Read in provided electricity sector data from each file and add file reference info
  city_data <- read_until_value(
    file_path = file_path,
    sheet = "Standard Community Report",
    start_cell = start_cell,
    stop_value = "Total:",
    columns = "H" # Read columns A to G
  ) %>%
    # clean up raw data -- rename columns as needed and drop unnecessary columns
    select(
      sector = Electricity,
      kwh_delivered = `Energy Consumption (kWh)`,
      util_reported_co2e = `Carbon Emissions (metric tons CO2) [6]`
    ) %>%
    mutate(
      mWh_delivered = kwh_delivered / 1000,
      city_name = city_name,
      utility_name = utility,
      year = year,
      source = "Electricity"
    )

  return(city_data)
}


# join with CTU-county reference
cprg_county <- readRDS("_meta/data/cprg_county.RDS")
cprg_ctu <- readRDS("_meta/data/cprg_ctu.RDS")
ctu_population <- readRDS("_meta/data/ctu_population.RDS") %>%
  filter(inventory_year > 2014) %>%
  left_join(cprg_county %>% select(geoid, county_name, state_abb), by = "geoid") %>%
  filter(state_abb == "MN") %>%
  rename(year = inventory_year)



if (file.exists("_energy/data/Xcel_activityData_2015_2023.RDS") == FALSE) {
  # Apply process_file to each file identified in get_files() in the nested structure and combine the results
  file_list <- get_files(dir_xcel_communityReports)

  # test that number of distinct city year combos = number of files
  # highlight which files if any have NA value for city_name or year
  testthat::test_that("Correct number of distinct city-year combos and identify missing ones", {
    # Extract combinations of city_name and year from file_list
    combinations <- lapply(file_list, function(x) {
      data.frame(city_name = x$city_name, year = x$year, stringsAsFactors = FALSE)
    })

    # Convert to a single data frame
    combinations_df <- do.call(rbind, combinations)

    # Count unique combinations
    unique_combinations <- combinations_df %>%
      distinct(city_name, year)

    # Identify duplicate or missing rows
    duplicate_combinations <- combinations_df %>%
      group_by(city_name, year) %>%
      filter(n() > 1)

    # Compare length of file list to unique combinations and find missing
    missing_combinations <- setdiff(
      combinations_df %>% distinct(city_name, year),
      unique_combinations
    )

    # Print missing combinations for inspection
    print("Missing combinations:")
    print(missing_combinations)

    # Assert equality of unique combinations to file list length
    testthat::expect_equal(
      nrow(unique_combinations),
      length(file_list) - 1,
      info = paste("Missing or duplicate city-year combinations found. Check missing_combinations and duplicate_combinations.")
    )
  })


  # Apply process_file_2015_2019 to each file for years 2015-19 identified in get_files() in the nested structure and combine the results
  file_list_2015_2019 <- Filter(function(x) x$year < 2020, file_list)
  file_list_2020_2023 <- Filter(function(x) x$year > 2019, file_list)

  combined_Xcel_activityData_2015_2019 <- do.call(
    rbind,
    lapply(file_list_2015_2019, function(file_info) {
      process_file(file_info, start_cell = "A20")
    })
  )

  combined_Xcel_activityData_2020_2023 <- do.call(
    rbind,
    lapply(file_list_2020_2023, function(file_info) {
      process_file(file_info, start_cell = "A39")
    })
  )


  # COCTU population proportion reference to parcel out Xcel CTU-level reporting to COCTUs based on population.
  # Assume sector breakouts are the same in all COCTU units tied to CTU reporting.

  # Calculate unique total population by city-year-county
  city_total_population <- ctu_population %>%
    distinct(ctu_name, ctu_class, year, county_name, ctu_population) %>% # Ensure unique rows per city-county-year
    group_by(ctu_name, ctu_class, year) %>%
    mutate(
      total_ctu_population = sum(ctu_population, na.rm = TRUE), # Sum populations across counties for each city-year
      multi_county = n_distinct(county_name) > 1
    ) %>%
    ungroup()


  Xcel_activityData_2015_2023 <- rbind(
    combined_Xcel_activityData_2015_2019,
    combined_Xcel_activityData_2020_2023
  ) %>%
    # Map Xcel-provided sectors to simplified CPRG sectors and "Business" for subsequent disaggregation
    mutate(
      sector_mapped = case_when(
        sector %in% c("Residential") ~ "residential",
        sector %in% c(
          "Street Lighting - Non-Metered/Customer Owned",
          "Street Lighting - Non-Metered/Xcel-Owned",
          "Street Lighting - Metered"
        ) ~ "commercial", # Map street lighting to Commercial
        sector == "Industrial" ~ "industrial",
        sector == "Commercial" ~ "commercial",
        sector == "Business *" ~ "Business",
        sector == "Business" ~ "Business", # Handle as disaggregation
        TRUE ~ NA_character_
      ),
      # Align ctu_name and ctu_class to CPRG format based on Xcel inputs
      ctu_class = case_when(
        grepl("^City of", city_name, ignore.case = TRUE) ~ "CITY",
        grepl("^Town of", city_name, ignore.case = TRUE) ~ "TOWNSHIP",
        city_name == "Village of Birchwood" ~ "CITY",
        TRUE ~ NA_character_
      ),
      ctu_name = case_when(
        grepl("^City of", city_name, ignore.case = TRUE) ~ sub("^City of\\s+", "", city_name),
        grepl("^Town of", city_name, ignore.case = TRUE) ~ sub("^Town of\\s+", "", city_name),
        city_name == "Village of Birchwood" ~ "Birchwood Village",
        TRUE ~ city_name
      )
    ) %>%
    # Remove records with no or unusable data
    filter(!is.na(sector_mapped)) %>%
    # Join city_total_population back to main dataset
    left_join(city_total_population,
      by = c("ctu_name", "ctu_class", "year"),
      relationship = "many-to-many"
    ) %>%
    # Calculate proportions and disaggregated values
    group_by(ctu_name, ctu_class, year, county_name) %>%
    mutate(
      ctu_population_proportion = ctu_population / total_ctu_population, # Calculate proportions
      disagg_util_reported_co2e = ifelse(
        multi_county,
        util_reported_co2e * ctu_population_proportion,
        NA
      ),
      disagg_mWh_delivered = ifelse(
        multi_county,
        mWh_delivered * ctu_population_proportion,
        NA
      )
    ) %>%
    ungroup() %>%
    # Filter to core metro counties while keeping `county_name` intact
    filter(county_name %in% c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington"))

  write_rds(Xcel_activityData_2015_2023, "_energy/data/Xcel_activityData_2015_2023.RDS")
}

Xcel_activityData_2015_2023 <- readRDS("_energy/data/Xcel_activityData_2015_2023.RDS")

# preprocess the NREL proportions dataset -- make duplicates of 2017 records for 2015 and 2016 to enable join to Xcel data
nrel_slope_city_emission_proportions_adjusted <- readRDS("_energy/data-raw/nrel_slope/nrel_slope_city_emission_proportions.RDS") %>%
  filter(source == "Electricity") %>%
  mutate(nrel_year = year) %>%
  # Duplicate rows for 2015 and 2016, mapping them to 2017
  bind_rows(
    filter(., year == 2017) %>% mutate(year = 2015),
    filter(., year == 2017) %>% mutate(year = 2016)
  )

xcel_activityData_NREL_QA_2015_2023 <- Xcel_activityData_2015_2023 %>%
  left_join(
    nrel_slope_city_emission_proportions_adjusted,
    # implicitly applies same proportions to COCTU splits
    by = join_by(
      ctu_name == city_name,
      ctu_class == ctu_class,
      year == year
    )
  ) %>%
  select(
    -source.x,
    -source.y,
    -kwh_delivered,
    -city_name
  )

xcel_activityData_NREL_2015_2023 <- bind_rows(
  # Keep original rows that are not 'Business'
  xcel_activityData_NREL_QA_2015_2023 %>%
    filter(sector_mapped != "Business"),

  # Expand `Business` rows into 'commercial*'
  xcel_activityData_NREL_QA_2015_2023 %>%
    filter(sector_mapped == "Business") %>%
    mutate(sector_mapped = "commercial*"),

  # Expand `Business` rows into 'industrial*'
  xcel_activityData_NREL_QA_2015_2023 %>%
    filter(sector_mapped == "Business") %>%
    mutate(sector_mapped = "industrial*")
) %>%
  # Mutate disaggregated or original values based on the value of `sector_mapped`
  mutate(
    util_co2e = coalesce(disagg_util_reported_co2e, util_reported_co2e),
    util_mWh = coalesce(disagg_mWh_delivered, mWh_delivered),

    # Adjust values for 'commercial*' and 'industrial*' rows
    # breaks out the Business records into two pieces based oon the proportional breakdown of NREL-modeled commercial/industrial
    util_co2e = case_when(
      sector_mapped == "commercial*" ~ util_co2e * coalesce(
        (commercial_city / (commercial_city + industrial_city)),
        (commercial_downscale / (commercial_downscale + industrial_downscale))
      ),
      sector_mapped == "industrial*" ~ util_co2e * coalesce(
        (industrial_city / (commercial_city + industrial_city)),
        (industrial_downscale / (commercial_downscale + industrial_downscale))
      ),
      TRUE ~ util_co2e
    ),
    util_mWh = case_when(
      sector_mapped == "commercial*" ~ util_mWh * coalesce(
        (commercial_city / (commercial_city + industrial_city)),
        (commercial_downscale / (commercial_downscale + industrial_downscale))
      ),
      sector_mapped == "industrial*" ~ util_mWh * coalesce(
        (industrial_city / (commercial_city + industrial_city)),
        (industrial_downscale / (commercial_downscale + industrial_downscale))
      ),
      TRUE ~ util_mWh
    ),

    # Create `nrel_breakout_source` column to identify which NREL data set was used to disaggregate BUSINESS records (if such disagg was done)
    # NA in this field + no asterisk on commercial/industrial means the breakout was directly provided by utility
    nrel_breakout_source = case_when(
      sector_mapped == "commercial*" & !is.na(commercial_city) ~ "CITY",
      sector_mapped == "industrial*" & !is.na(industrial_city) ~ "CITY",
      sector_mapped == "commercial*" & !is.na(commercial_downscale) ~ "COUNTY",
      sector_mapped == "industrial*" & !is.na(industrial_downscale) ~ "COUNTY",
      TRUE ~ NA_character_
    )
  )

# final cleanup of Xcel Activity data
xcel_activityData_NREL_2015_2023_process <- xcel_activityData_NREL_2015_2023 %>%
  select(
    -util_reported_co2e, # source numbers, not disaggregated by sector or COCTU
    -mWh_delivered, # source numbers, not disaggregated by sector or COCTU
    -c(10:22)
  ) # intermediate calculations

write_rds(xcel_activityData_NREL_2015_2023_process, "_energy/data/xcel_activityData_NREL_2015_2023_process.RDS")

ctu_commDesg <- read.csv("_meta/data-raw/ctus_summary_2018.csv") %>%
  select(
    ctu_name = Ctu.Name,
    community_designation = Community.Designation
  )

# Validate and repair geometries before dissolving
cprg_ctu <- cprg_ctu %>%
  filter(ctu_class == "CITY") %>%
  mutate(geometry = st_make_valid(geometry)) # Ensure all geometries are valid

# Dissolve polygons by ctu_name
dissolved_ctu <- cprg_ctu %>%
  select(ctu_name, geometry) %>% # Keep relevant columns
  group_by(ctu_name) %>% # Group by ctu_name
  summarize(geometry = st_union(geometry)) %>% # Merge geometries
  ungroup()

# plot(dissolved_ctu["geometry"]) # Quick visualization

# enable comparison of NREL breakdowns to actual breakdowns from Xcel
complete_city_years <- xcel_activityData_NREL_2015_2023 %>%
  filter(
    !is.na(util_mWh),
    !is.na(commercial_city),
    !is.na(industrial_city),
    !is.na(residential_city)
  ) %>%
  group_by(ctu_name, year) %>%
  summarize(
    has_residential = any(sector_mapped == "residential"),
    has_commercial = any(sector_mapped == "commercial"),
    has_industrial = any(sector_mapped == "industrial"),
    .groups = "drop"
  ) %>%
  filter(
    has_residential & has_commercial & has_industrial
  )

# Collapse rows to city-year grain
complete_city_NREL_comparison <- xcel_activityData_NREL_2015_2023 %>%
  semi_join(complete_city_years, by = c("ctu_name", "year")) %>%
  group_by(ctu_name, year) %>%
  summarize(
    total_util_mWh = sum(util_mWh, na.rm = TRUE),
    total_commercial_mWh = sum(util_mWh[sector_mapped == "commercial"], na.rm = TRUE),
    total_industrial_mWh = sum(util_mWh[sector_mapped == "industrial"], na.rm = TRUE),
    total_residential_mWh = sum(util_mWh[sector_mapped == "residential"], na.rm = TRUE),
    commercial_city = first(commercial_city), # Values are repeated, so take the first
    industrial_city = first(industrial_city),
    residential_city = first(residential_city),
    .groups = "drop"
  ) %>%
  mutate(
    actual_commercial_prop = total_commercial_mWh / total_util_mWh,
    actual_industrial_prop = total_industrial_mWh / total_util_mWh,
    actual_residential_prop = total_residential_mWh / total_util_mWh
  ) %>%
  left_join(ctu_commDesg,
    by = join_by(ctu_name)
  ) %>%
  left_join(dissolved_ctu,
    by = join_by(ctu_name)
  ) %>%
  st_as_sf()




# Assessment and analysis of Xcel data v. NREL data

# Data preparation: Transform data to long format, enrich with sector/metric source, calculate diffs for visualization
data_city_year_sector_props <- complete_city_NREL_comparison %>%
  st_transform(crs = 32615) %>% # Projected CRS (UTM Zone 15N for the Twin Cities area)
  # Pivot to long format to separate sectors and actual/modeled
  pivot_longer(
    cols = c(
      residential_city, commercial_city, industrial_city,
      actual_residential_prop, actual_commercial_prop, actual_industrial_prop
    ),
    names_to = "key",
    values_to = "value"
  ) %>%
  mutate(
    sector = case_when(
      str_detect(key, "residential") ~ "Residential",
      str_detect(key, "commercial") ~ "Commercial",
      str_detect(key, "industrial") ~ "Industrial"
    ),
    data_type = case_when(
      str_detect(key, "actual") ~ "Actual",
      TRUE ~ "Modeled"
    )
  )


data_city_year_sector_propDiffs <- data_city_year_sector_props %>%
  # Pivot wider to create columns for Modeled and Actual values
  pivot_wider(names_from = data_type, values_from = value) %>%
  group_by(ctu_name, year, sector, community_designation, geometry) %>%
  summarize(
    Modeled = mean(Modeled, na.rm = TRUE),
    Actual = mean(Actual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    difference = Modeled - Actual
  )

# Remove year and take an overall look at trends in modeled v. actuals by community designation (averages all years -- biases cities with more data)
commDesg_diff_agg <- data_city_year_sector_propDiffs %>%
  st_drop_geometry() %>%
  group_by(community_designation, sector) %>%
  summarize(
    avgPropDiff = mean(ifelse(is.na(difference), 0, difference), na.rm = TRUE),
    .groups = "drop"
  )



# #PLOT ONE -- Bar graphs showing Average Proportion Differences by Community Designation and Sector
# ggplot(commDesg_diff_agg, aes(x = community_designation, y = avgPropDiff, fill = sector)) +
#   geom_bar(stat = "identity", position = "dodge") +  # Side-by-side bars
#   scale_y_continuous() +  # Allow positive and negative values
#   labs(
#     title = "Average Proportion Differences by Community Designation and Sector",
#     x = "Community Designation",
#     y = "Average Proportion Difference",
#     fill = "Sector"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
#   )
#
#
# # PLOT TWO -- Map showing Spatial Distribution of Proportion Differences (Modeled - Actual)
# ggplot(data_city_year_sector_propDiffs) +
#   geom_sf(aes(geometry = geometry, fill = difference), color = "black") +
#   geom_sf_text(aes(geometry = geometry, label =  ctu_name), size = 3) +
#   scale_fill_distiller(palette = "RdBu", oob = scales::squish) +
#   facet_wrap(~sector) +
#   labs(
#     title = "Spatial Distribution of Proportion Differences (Modeled - Actual)",
#     fill = "Difference"
#   ) +
#   theme_minimal()
#
# # PLOT THREE - Scatter plots comparing Modeled vs Actual Proportions by Sector
# ggplot(data_city_year_sector_propDiffs, aes(x = Actual, y = Modeled, color = community_designation)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   facet_wrap(~sector, scales = "free") +
#   labs(
#     title = "Modeled vs Actual Proportions by Sector",
#     x = "Actual Proportion",
#     y = "Modeled Proportion",
#     color = "Community Designation"
#   ) +
#   theme_minimal()
#
