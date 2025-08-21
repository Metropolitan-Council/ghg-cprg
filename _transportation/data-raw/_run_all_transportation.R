# Run once scripts-----
# # wisdot processing
# source("_transportation/data-raw/wisdot_stations.R")
# source("_transportation/data-raw/wisdot_vmt_county.R")
#
# # mndot processing
# source("_transportation/data-raw/mndot_extract_yearly_volume_trends.R")
source("_transportation/data-raw/mndot_vmt_county.R")
#
# # dot combination and processing
source("_transportation/data-raw/dot_vmt.R")

# CTU level data and modeling
source("_transportation/data-raw/mndot_vmt_ctu.R")
source("_transportation/data-raw/vmt_model_data.R")
source("_transportation/data-raw/mndot_vmt_ctu_gap_fill_model.R")

#
# # streetlight upload, run, and fetch analyses
# source("_transportation/data-raw/stl_calibration_lines_points.R")
# source("_transportation/data-raw/stl_upload_zone_sets.R")
# source("_transportation/data-raw/stl_run_analyses.R")
# source("_transportation/data-raw/stl_fetch_analyses.R")

# re-run scripts -----
# epa moves processing
source("_transportation/data-raw/epa_moves.R")

# key script that outputs VMT and emissions
# source("_transportation/data-raw/vmt_emissions.R")

# corresponding datasets
# these are currently commented out pending TBI updates
# See https://github.com/Metropolitan-Council/ghg-cprg/issues/125
#
# source("_transportation/data-raw/tbi_vehicle_stats.R")
# source("_transportation/data-raw/tbi_survey_stats.R")
# source("_transportation/data-raw/tbi_trip_length.R")

# LGGIT verification needs to be updated
# See https://github.com/Metropolitan-Council/ghg-cprg/issues/122
# source("_transportation/data-raw/epa_lggit.R")
# source("_transportation/data-raw/epa_nei_envirofacts.R")

source("_transportation/data-raw/epa_pollutants.R")
source("_transportation/data-raw/epa_source_classification_codes.R")
source("_transportation/data-raw/epa_onroad_emissions_compile.R")


source("_transportation/data-raw/_transportation_compile.R")
source("_meta/data-raw/compile_county_emissions.R")
source("_meta/data-raw/compile_ctu_emissions.R")
