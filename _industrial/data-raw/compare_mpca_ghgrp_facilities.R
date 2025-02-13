##### look for commonality in MPCA dataset and GHGRP data

mpca_emissions <- readRDS(file.path(here::here(), "_industrial/data/mpca_fuel_emissions.RDS")) %>%
  filter(sector == "Industrial") %>%
  mutate(facility_match = str_to_title(source_name))
ghgrp_emissions <- readRDS(file.path(here::here(), "_industrial/data/ghgrp_industrial_point_sources_ctu.rds")) %>%
  mutate(facility_match = str_to_title(facility_name))

library(fuzzyjoin)
library(stringdist)


# Create a function for fuzzy matching within subsets
fuzzy_match_within <- function(facility_subset, source_subset) {
  # Extract names for the subset
  ghgrp_facility <- unique(facility_subset$facility_match)
  mpca_facility <- unique(source_subset$facility_match)

  # Compute pairwise distance matrix
  distance_matrix <- stringdistmatrix(ghgrp_facility, mpca_facility, method = "jw")

  # Find the best match for each facility_name
  best_matches <- apply(distance_matrix, 1, function(row) {
    best_index <- which.min(row) # Index of the smallest distance
    list(
      mpca_facility = mpca_facility[best_index],
      match_quality = row[best_index]
    )
  })

  # Return a data frame for this subset
  tibble(
    ghgrp_facility = ghgrp_facility,
    mpca_facility = sapply(best_matches, function(x) x$mpca_facility),
    match_quality = sapply(best_matches, function(x) x$match_quality),
    # naics = unique(facility_subset$primary_naics_code),
    city_name = unique(facility_subset$city_name)
  )
}

# Get unique combinations of NAICS and city_name from ghgrp_emissions
unique_combinations <- ghgrp_emissions %>%
  distinct(primary_naics_code, city_name) %>%
  rename(naics = primary_naics_code)

# Initialize an empty results list
results <- list()

# Loop through each unique combination
for (i in seq_len(nrow(unique_combinations))) {
  combo <- unique_combinations[i, ]

  # Subset ghgrp_emissions and mpca_emissions by NAICS and city_name
  facility_subset <- ghgrp_emissions %>%
    filter(city_name == combo$city_name)

  source_subset <- mpca_emissions %>%
    filter(ctu_name == combo$city_name)

  # Perform fuzzy matching if both subsets are non-empty
  if (nrow(facility_subset) > 0 & nrow(source_subset) > 0) {
    result <- fuzzy_match_within(facility_subset, source_subset)
    results[[i]] <- result
  }
}

# Combine results into a single data frame
final_matches <- bind_rows(results)

# Sort results by match_quality for review
final_matches <- final_matches %>%
  arrange(match_quality)

# View the matches
print(final_matches, n = 100) # Adjust as needed

### lots of mismatches and issues here...
