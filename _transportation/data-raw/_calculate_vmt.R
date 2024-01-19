#' Calculate vehicle miles traveled
#'
#' @param data_list list, named list of metrics returned from a StreetLight analysis
#'
#' @return
#' @export
#'
#' @examples
calculate_vmt <- function(data_list, class = "passenger"){
  
  if(class == "passenger"){
    od_table <- "od_all"
    trip_table <- "od_trip_all"
    trip_col <- "average_daily_o_d_traffic_st_l_volume"
    
    data_list <- purrr::map(data_list,
                            function(x){
                              x %>% 
                                mutate(vehicle_weight = "Passenger")
                            })
  } else {
    od_table <- "od_comm"
    trip_table <- "od_trip_comm"
    trip_col <- "average_daily_o_d_traffic_st_l_calibrated_index"
  }
  
  
  
  
  # Get origin-destination volume
  od_all <- data_list[od_table][[1]] %>% 
    select(analysis_name, metric_group,
           mode_of_travel, origin_zone_name,
           destination_zone_name, day_type, day_part, 
           estimated_trips = any_of(trip_col),
           vehicle_weight
    ) %>% 
    filter(day_type == "0: All Days (M-Su)",
           day_part == "0: All Day (12am-12am)")
  
  
  # get trip lengths
  od_trip_all <- data_list[trip_table][[1]] %>% 
    select(analysis_name, 
           origin_zone_name,
           destination_zone_name,
           day_type, day_part, 
           avg_trip_length_mi,
           avg_all_trip_length_mi,
           vehicle_weight) %>% 
    filter(day_type == "0: All Days (M-Su)",
           day_part == "0: All Day (12am-12am)")
  
  # calculate origin-destination VMT from volume and avg trip length
  od_trips <- od_all %>% 
    left_join(od_trip_all, 
              join_by(analysis_name, 
                      origin_zone_name, destination_zone_name, 
                      day_type, day_part,
                      vehicle_weight)) %>% 
    rowwise() %>% 
    mutate(vmt = estimated_trips * avg_all_trip_length_mi,
           vmt_year = vmt * 365,
           vmt_year_half = vmt_year * 0.5) %>% 
    select(analysis_name,
           mode_of_travel,
           origin_zone_name,
           destination_zone_name, day_type, day_part,
           vehicle_weight,
           estimated_trips,
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
    dplyr::select(analysis_name, mode_of_travel, zone, 
                  vehicle_weight, vmt_same) %>% # select only the zone and vmt columns
    droplevels() # remove extraneous data
  
  od_origin <- od_trips %>% 
    filter(origin_zone_name != destination_zone_name) %>% 
    dplyr::ungroup() %>%
    # get only origin zones
    dplyr::mutate(zone = origin_zone_name) %>% 
    dplyr::select(analysis_name, mode_of_travel, zone, 
                  vehicle_weight, vmt_year_half) %>% 
    dplyr::group_by(analysis_name, mode_of_travel, vehicle_weight, zone) %>% 
    # find total VMT when given zone is an *origin*
    dplyr::summarize(vmt_origin = sum(vmt_year_half), .groups = "keep") %>% 
    droplevels() # remove extraneous data
  
  
  od_destination <- od_trips %>% 
    # filter to only trips that have different origin and destination
    filter(origin_zone_name != destination_zone_name) %>% 
    dplyr::ungroup() %>%
    # get only destination zones
    dplyr::mutate(zone = destination_zone_name) %>% 
    dplyr::select(analysis_name, mode_of_travel, zone,
                  vehicle_weight, vmt_year_half) %>% 
    dplyr::group_by(analysis_name, mode_of_travel, zone, vehicle_weight) %>% 
    # find total VMT when given zone is a *destination*
    dplyr::summarize(vmt_destination = sum(vmt_year_half), .groups = "keep") %>% 
    droplevels() # remove extraneous data
  
  vmt_all <- 
    left_join(od_same,
              od_origin,
              join_by(analysis_name, mode_of_travel, zone, vehicle_weight)) %>% 
    left_join(od_destination,
              join_by(analysis_name, mode_of_travel, zone, vehicle_weight)) %>% 
    rowwise() %>% 
    # if heavy duty, 
    # then vmt_origin and vmt_destination are 0
    # because we assume the origin or destination is outside the region
    # only vmt_same is counted for 
    mutate(vmt_origin = ifelse(vehicle_weight == "Heavy", 0, vmt_origin),
           vmt_destination = ifelse(vehicle_weight == "Heavy", 0, vmt_destination),
           vmt_total = sum(vmt_origin, vmt_destination, vmt_same),
           vehicle_weight = factor(vehicle_weight,
                                   levels = c(
                                     "Passenger",
                                     "Medium",
                                     "Heavy"
                                   ),
                                   ordered = TRUE
           ),
           vehicle_weight_label = case_when(
             vehicle_weight == "Passenger" ~ "Light-duty",
             TRUE ~ paste0(vehicle_weight, "-duty")) %>%
             factor(
               levels = c(
                 "Light-duty",
                 "Medium-duty",
                 "Heavy-duty"
               ),
               ordered = TRUE
             )) %>% 
    ungroup()
  
  
  return(vmt_all)
  
}
