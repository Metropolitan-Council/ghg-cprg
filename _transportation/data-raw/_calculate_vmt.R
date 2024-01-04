#' Calculate vehicle miles traveled
#'
#' @param data_list list, named list of metrics returned from a StreetLight analysis
#'
#' @return
#' @export
#'
#' @examples
calculate_vmt <- function(data_list){
  
  # Get origin-destination volume
  od_all <- data_list["od_all"][[1]] %>% 
    select(analysis_name, metric_group,
           mode_of_travel, origin_zone_name,
           destination_zone_name, day_type, day_part, 
           average_daily_o_d_traffic_st_l_volume
           ) %>% 
    filter(day_type == "0: All Days (M-Su)",
           day_part == "0: All Day (12am-12am)")
  
  
  # get trip lengths
  od_trip_all <- data_list["od_trip_all"][[1]] %>% 
    select(analysis_name, 
           origin_zone_name,
           destination_zone_name,
           day_type,day_part, avg_trip_length_mi,
           avg_all_trip_length_mi) %>% 
    filter(day_type == "0: All Days (M-Su)",
           day_part == "0: All Day (12am-12am)")
  
  # calculate origin-destination VMT from volume and avg trip length
  od_trips <- od_all %>% 
    left_join(od_trip_all,
              by = join_by(analysis_name, origin_zone_name, destination_zone_name, day_type, day_part)) %>% 
    rowwise() %>% 
    mutate(vmt = average_daily_o_d_traffic_st_l_volume * avg_all_trip_length_mi,
           vmt_year = vmt * 365,
           vmt_year_half = vmt_year * 0.5) %>% 
    select(analysis_name,
           mode_of_travel,
           origin_zone_name,
           destination_zone_name, day_type, day_part,
           average_daily_o_d_traffic_st_l_volume,
           avg_all_trip_length_mi,
           avg_trip_length_mi,
           vmt,
           vmt_year,
           vmt_year_half)
  
  
  
  # calculate totals for each zone -----

  od_same <- od_trips %>% 
    # filter to only trips that have the same origin and destination zones
    filter(origin_zone_name == destination_zone_name) %>% 
    dplyr::mutate(
      zone = origin_zone_name, # create shorter name zone name variable 
      vmt_same = vmt_year) %>%  # create new variable with the vmt for same origin destination trips
    dplyr::select(analysis_name, mode_of_travel, zone, vmt_same) %>% # select only the zone and vmt columns
    droplevels() # remove extraneous data
  
  od_origin <- od_trips %>% 
    filter(origin_zone_name != destination_zone_name) %>% 
    dplyr::ungroup() %>%
    # get only origin zones
    dplyr::mutate(zone = origin_zone_name) %>% 
    dplyr::select(analysis_name, mode_of_travel, zone, vmt_year_half) %>% 
    dplyr::group_by(analysis_name, mode_of_travel, zone) %>% 
    # find total VMT when given zone is an *origin*
    dplyr::summarize(vmt_origin = sum(vmt_year_half), .groups = "keep") %>% 
    droplevels() # remove extraneous data
  
  
  od_destination <- od_trips %>% 
    # filter to only trips that have different origin and destination
    filter(origin_zone_name != destination_zone_name) %>% 
    dplyr::ungroup() %>%
    # get only destination zones
    dplyr::mutate(zone = destination_zone_name) %>% 
    dplyr::select(analysis_name, mode_of_travel, zone, vmt_year_half) %>% 
    dplyr::group_by(analysis_name, mode_of_travel, zone) %>% 
    # find total VMT when given zone is a *destination*
    dplyr::summarize(vmt_destination = sum(vmt_year_half), .groups = "keep") %>% 
    droplevels() # remove extraneous data
  
  vmt_all <- 
  left_join(od_same, od_origin,
            by = join_by(zone, analysis_name, mode_of_travel)) %>% 
    left_join(od_destination,
              by = join_by(zone, analysis_name, mode_of_travel))
  
  
  return(vmt_all)
  
}
