# Clear old vars
rm(list = ls())


## Start here:
source(file.path(here::here(), "R/_load_pkgs.R"))
source(file.path(here::here(), "R/_quarto_helpers.R"))
source(file.path(here::here(), "R/plot_county_emissions.R"))




county_data <- readRDS("_meta/data/cprg_county.RDS")
LIDAC_polygons <- readRDS("_meta/data/lidac_block_groups.RDS")
CEJST_map_data <- readRDS("_meta/data/ejscreen_block_groups.RDS")




LIDAC_polygons_dissolved <- LIDAC_polygons %>%
  dplyr::group_by(LIDAC) %>%
  summarize(do_union = TRUE)


## ggnewscale, patchwork, classInt


mutate_jenks_brks <- function(input, variable, ...) {
  var_string <- deparse(substitute(variable))
  # var_name <- enquo(variable)
  
  # add breaks
  jenks <- classInt::classIntervals(input[[var_string]], style = "jenks", ...)$brks
  
  if (any(0 == jenks)) brk_vec <- jenks
  else brk_vec <- c(0,jenks)
  
  # set labels
  lab_length <- length(brk_vec) - 1
  brk_labs <- vector(mode = "character", length = lab_length)
  
  # create labels dependent on the number of breaks specified
  for (i in seq(lab_length)) {
    
    if (any(brk_vec %% 1 != 0)) {#rounding to 2 decimal places
      brk_labs[i] <- paste0(round(brk_vec[i], 2), "-", round(brk_vec[i + 1], 2))
      
    } else if (any(brk_vec %% 1 == 0)) {
      brk_labs[i] <- paste0(brk_vec[i], "-", brk_vec[i + 1])
      
    }
  }
  
  # create labels using `cut()`
  input[[paste0(var_string, "_brks")]] <-
    cut(input[[var_string]],
        breaks = brk_vec,
        labels = brk_labs,
        include.lowest = TRUE)
  
  return(input)
  
  # labels can be changed directly by levels(input$variable) <- c("here","there","somewhere", ...)
  
}


convert_brks_strings <- function(data, column_name) {
  # Function to convert the format
  convert_format <- function(string) {
    parts <- strsplit(as.character(string), "-")[[1]]  # Ensure conversion to character
    
    if (length(parts) != 2) {
      cat("Error: Unexpected input format:", string, "\n")
      return(NA) # Return NA for unexpected inputs
    }
    
    start_num <- as.numeric(parts[1])
    end_num <- as.numeric(parts[2])
    
    # Function to determine suffix
    determine_suffix <- function(num) {
      if (num == 0) {
        return("")  # No suffix for 0
      } else if (num %% 10 == 1 && num != 11) {
        return("st")
      } else if (num %% 10 == 2 && num != 12) {
        return("nd")
      } else if (num %% 10 == 3 && num != 13) {
        return("rd")
      } else {
        return("th")
      }
    }
    
    # Convert start number to the desired format
    start_suffix <- determine_suffix(start_num)
    start_formatted <- ifelse(start_num == 0, "0", paste0(start_num, start_suffix))
    
    # Convert end number to the desired format
    end_suffix <- determine_suffix(end_num)
    end_formatted <- paste0(end_num, end_suffix)
    
    # Concatenate the formatted strings
    formatted_string <- paste0(start_formatted, " to ", end_formatted)
    return(formatted_string)
  }
  
  # Apply the conversion function to the specified column
  data <- data %>%
    mutate(!!paste0(column_name, "_formatted") := sapply(!!sym(column_name), convert_format))
  
  return(data)
}

sort_factor_column <- function(data, column_name) {
  data <- data %>%
    mutate(numeric_value = as.numeric(str_extract(!!sym(column_name), "\\d+"))) %>%
    arrange(numeric_value) %>%
    mutate(!!sym(column_name) := factor(!!sym(column_name), levels = unique(!!sym(column_name))))
  
  return(data)
}

plot_design <- "
  AAAAAAAAAAAA
  AAAAAAAAAAAA
  AAAAAAAAAAAA
  AAAAAAAAAAAA
"


map_PM2.5 <- 
  ggplot() +
  # geom_sf(data = CEJST_map_data %>%
  #           mutate_jenks_brks(PM2.5.in.the.air..percentile., n = 4) %>%
  #           convert_brks_strings(data=., "PM2.5.in.the.air..percentile._brks") %>% 
  #           dplyr::group_by(PM2.5.in.the.air..percentile._brks_formatted) %>%
  #           summarize(do_union = TRUE) %>% sort_factor_column(., "PM2.5.in.the.air..percentile._brks_formatted") %>%
  #           filter(!is.na(PM2.5.in.the.air..percentile._brks_formatted)),
  #         aes(fill=PM2.5.in.the.air..percentile._brks_formatted), alpha=0.8, lwd=0)  +
  geom_sf(data = county_data, color="steelblue4", fill=NA, lwd=0.4) +
  scale_fill_brewer(name=expression(PM[2.5]~"percentile"), type = "seq", palette = 3, guide = guide_legend(order = 1)) +
  councilR::theme_council_geo(use_showtext=T) +
  
  ggnewscale::new_scale_fill() + ## geoms added after this will use a new scale definition
  
  geom_sf(data = LIDAC_polygons_dissolved %>% filter(LIDAC == "Yes"),
          aes(fill="orange"), color="goldenrod1", 
          # color="goldenrod1", fill="orange", 
          alpha=0.3, lwd=0.3) +
  scale_fill_identity(name='', guide='legend', labels=c('LIDAC')) +
  labs(title=expression(bold("Exposure to inhalable particulate matter ≤ 2.5"~mu*"m ("*PM[2.5]*")")),
       caption="Source: Climate and Economic Justice Screening Tool (https://screeningtool.geoplatform.gov) \nEPA Inflation Reduction Act Disadvantaged Communities Map (https://www.epa.gov/environmentaljustice/inflation-reduction-act-disadvantaged-communities-map)") + 
  theme(plot.title = element_text(hjust=0, size=14),
        plot.subtitle = element_text(hjust=0, size=11),
        plot.caption = element_text(hjust=0, vjust=1, face="italic", size=7)) 


map_PM2.5 <- map_PM2.5 + patchwork::plot_layout(design = plot_design)




