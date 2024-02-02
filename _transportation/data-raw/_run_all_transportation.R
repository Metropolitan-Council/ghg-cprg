# Run once scripts-----
# # wisdot processing
# source("_transportation/data-raw/wisdot_stations.R")
# source("_transportation/data-raw/wisdot_vmt_county.R")
# 
# # mndot processing
# source("_transportation/data-raw/mndot_extract_yearly_volume_trends.R")
# source("_transportation/data-raw/mndot_vmt_county.R")
# 
# # dot combination and processing
# source("_transportation/data-raw/dot_vmt.R")
# source("_transportation/data-raw/calibration_lines_points.R")
# 
# # streetlight upload, run, and fetch analyses
# source("_transportation/data-raw/stl_upload_zone_sets.R")
# source("_transportation/data-raw/stl_run_analyses.R")
# source("_transportation/data-raw/stl_fetch_analyses.R")

# re-run scripts -----
# epa moves processing
source("_transportation/data-raw/epa_moves.R")

# key script that outputs VMT and emissions
source("_transportation/data-raw/vmt_emissions.R")

# corresponding datasets
source("_transportation/data-raw/tbi_vehicle_stats.R")
source("_transportation/data-raw/epa_lggit.R")
source("_transportation/data-raw/epa_nei.R")
