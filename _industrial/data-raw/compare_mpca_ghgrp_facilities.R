##### look for commonality in MPCA dataset and GHGRP data

mpca_emissions <- readRDS(file.path(here::here(), "_industrial/data/mpca_fuel_emissions.RDS"))
ghgrp_emissions <- readRDS(file.path(here::here(), "_industrial/data/ghgrp_industrial_point_sources_ctu.rds"))

library(fuzzyjoin)
library(stringdist)

#Extract unique facility names from ghgrp_emissions
facility_names <- unique(ghgrp_emissions$facility_name)

# Extract unique source names from mpca_emissions
source_names <- unique(mpca_emissions$source_name)

# Create a pairwise distance matrix between facility_names and source_names
distance_matrix <- stringdistmatrix(facility_names, source_names, method = "jw")

# Find the best match for each facility_name
best_matches <- apply(distance_matrix, 1, function(row) {
  best_index <- which.min(row) # Index of the smallest distance
  list(
    source_name = source_names[best_index],
    match_quality = row[best_index]
  )
})

# Convert results to a data frame
matches_df <- tibble(
  facility_name = facility_names,
  source_name = sapply(best_matches, function(x) x$source_name),
  match_quality = sapply(best_matches, function(x) x$match_quality)
)

# Sort by match_quality for review
matches_df <- matches_df %>%
  arrange(match_quality)

# View the matches
print(matches_df, n = 20)  # Adjust as needed

# View likely matches
head(likely_matches, 20)  # Adjust number of rows displayed
