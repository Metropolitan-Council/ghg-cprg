source("R/_load_pkgs.R")
# takes traffic ratios used for calibrating StreetLight and finds the average
# proportional breakdown for passenger, medium, and heavy duty traffic.
#
## read in traffic ratios
traffic_ratios <- readRDS(paste0("_transportation/data-raw/mndot/most_recent_yearly_volume_percentage_by_class.RDS"))

cdp_freight <- traffic_ratios %>%
  summarize(
    `Medium Goods Vehicles` = mean(medium_duty),
    `Heavy Goods Vehicles` = mean(heavy_duty)
  ) %>% 
  summarize(
    `Medium Goods Freight` = `Medium Goods Vehicles`/(`Medium Goods Vehicles` +
                                                        `Heavy Goods Vehicles`),
    `Heavy Goods Freight` = `Heavy Goods Vehicles`/(`Medium Goods Vehicles` +
                                                      `Heavy Goods Vehicles`)
  )

cdp_freight %>% kable(format = "markdown")
