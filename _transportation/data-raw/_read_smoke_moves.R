#' Read, clean, and save SMOKE/MOVES data
#'
#' @param file_location character, location of SMOKE/MOVES file
#' @param n_skip_rows numeric, how many header rows to skip
#' @param out_directory character, output location
read_smoke_moves <- function(file_location,
                             n_skip_rows,
                             out_directory) {
  
  # read and complete initial cleaning
  smoke_moves_table <- data.table::fread(
    file = file_location,
    skip = n_skip_rows,
    header = FALSE,
    colClasses = "character",
    col.names = c(
      "country_cd", "region_cd", "tribal_code",
      "census_tract_cd", "shape_id", "scc", "emis_type",
      "poll", "ann_value", "ann_pct_red", "control_ids",
      "control_measures", "current_cost",
      "cumulative_cost", "projection_factor",
      "reg_codes", "calc_method", "calc_year",
      "date_updated", "data_set_id", "jan_value",
      "feb_value", "mar_value", "apr_value", "may_value",
      "jun_value", "jul_value", "aug_value", "sep_value",
      "oct_value", "nov_value", "dec_value", "jan_pctred",
      "feb_pctred", "mar_pctred", "apr_pctred", "may_pctred",
      "jun_pctred", "jul_pctred", "aug_pctred", "sep_pctred",
      "oct_pctred", "nov_pctred", "dec_pctred", "comment"
    )
  ) %>%
    # only include counties in MN and WI
    dplyr::filter(
      region_cd %in% c(
        "27095", "27045", "27073", "27085", "27153", "27105", "27001",
        "27057", "27063", "27039", "27047", "27121", "27143", "27109",
        "27067", "27133", "27161", "27033", "27071", "27165", "27171",
        "27027", "27139", "27005", "27107", "27025", "27089", "27169",
        "27007", "27083", "27049", "27061", "27093", "27065", "27077",
        "27081", "27111", "27021", "27037", "27075", "27003", "27099",
        "27101", "27123", "27167", "27059", "27009", "27159", "27113",
        "27157", "27163", "27035", "27155", "27173", "27131", "27145",
        "27141", "27079", "27137", "27031", "27051", "27023", "27043",
        "27017", "27115", "27149", "27069", "27091", "27097", "27117",
        "27129", "27087", "27053", "27015", "27103", "27041", "27151",
        "27147", "27125", "27029", "27011", "27013", "27119", "27127",
        "27055", "27019", "27135", "55111", "55093", "55063", "55033",
        "55053", "55047", "55127", "55123", "55059", "55079", "55003",
        "55085", "55137", "55129", "55065", "55135", "55125", "55089",
        "55117", "55131", "55007", "55097", "55039", "55061", "55067",
        "55105", "55023", "55035", "55083", "55041", "55113", "55121",
        "55095", "55045", "55087", "55001", "55119", "55073", "55037",
        "55005", "55051", "55081", "55101", "55115", "55027", "55025",
        "55015", "55055", "55013", "55017", "55031", "55077", "55009",
        "55103", "55141", "55139", "55069", "55091", "55049", "55075",
        "55099", "55043", "55021", "55019", "55109", "55057", "55107",
        "55133", "55011", "55078", "55071", "55029"
      ),
      # only include certain pollutants
      poll %in% c(
        "CH4", "N2O",
        "CO2", "NO", "NOX", "SO2", "NH3",
        "HFC", "VOC", "O3", "CO", "PFC", "SF6",
        "PM10-PRI", "PM25-PRI", "PM-CON"
      )
    ) %>%
    dplyr::mutate(
      # convert all value columns to numeric
      dplyr::across(tidyr::ends_with("value"), as.numeric),
      # create scc6 column
      scc6 = stringr::str_sub(scc, 1, 6),
      # record original file path/location
      file_location = file_location,
      # specify unit of measure in column title
      emissions_short_tons = ann_value
    ) %>%
    dplyr::select(
      -tribal_code, -census_tract_cd,
      -shape_id, -country_cd,
      -date_updated, -data_set_id,
      -current_cost,
      -cumulative_cost, -reg_codes,
      -ann_pct_red,
      -projection_factor, -calc_method,
      -control_measures, -control_ids,
      -tidyr::starts_with(tolower(month.abb))
    )
  
  # create the output file name from the input file name
  # same name, but ending with ".RDS" instead of ".csv"
  out_file_name <- stringr::str_split(file_location, pattern = "/") %>%
    last() %>% last() %>%
    stringr::str_remove(".csv") %>% 
    paste0(".RDS")
  
  # save
  saveRDS(smoke_moves_table, 
          file.path(paste0(out_directory, 
                           out_file_name)))
  
  # ensure removed from environment
  rm(smoke_moves_table)
}
