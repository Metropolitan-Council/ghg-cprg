### function for plotting sector projections

plot_emissions_pathways <- function(
    base_data,
    diverging_data,
    target_value,
    target_year = 2050,
    base_cutoff_year = 2025,
    ppp_bau_color = NULL,
    y_max = NULL,
    title = "Emissions Pathways",
    y_label = "",
    x_limits = c(2005, 2051)
) {
  #' Plot emissions pathways with BAU and PPP scenarios
  #' 
  #' @param base_data Data frame with columns: emissions_year/inventory_year, value_emissions/total_emissions
  #' @param diverging_data Data frame with columns: emissions_year/inventory_year, scenario, value_emissions/total_emissions
  #' @param target_value Numeric value for the net-zero target
  #' @param target_year Year to place the target marker (default: 2050)
  #' @param base_cutoff_year Year where historical data ends (default: 2025)
  #' @param ppp_bau_color Color for the ribbon between PPP and BAU lines
  #' @param y_max Optional maximum value for y-axis
  #' @param title Plot title
  #' @param y_label Y-axis label
  #' @param x_limits Vector of length 2 for x-axis limits
  #' 
  #' @return ggplot object
  
  # Detect column names
  year_col <- if ("emissions_year" %in% names(base_data)) "emissions_year" else "inventory_year"
  emissions_col <- if ("value_emissions" %in% names(base_data)) "value_emissions" else "total_emissions"
  
  # Prepare data for PPP ribbon (between PPP and BAU)
  ppp_ribbon_data <- diverging_data %>%
    filter(scenario %in% c("ppp", "bau")) %>%
    select(!!sym(year_col), scenario, !!sym(emissions_col)) %>%
    pivot_wider(names_from = scenario, values_from = !!sym(emissions_col)) %>%
    filter(!is.na(ppp), !is.na(bau))
  
  # Create the plot
  p <- ggplot() +
    # Gray fill for all historical and under PPP
    geom_ribbon(
      data = base_data,
      aes(x = !!sym(year_col), ymin = 0, ymax = !!sym(emissions_col)),
      fill = "gray80", alpha = 0.7
    ) +
    geom_ribbon(
      data = diverging_data %>% filter(scenario == "ppp"),
      aes(x = !!sym(year_col), ymin = 0, ymax = !!sym(emissions_col)),
      fill = "gray80", alpha = 0.7
    ) +
    
    # Ribbon between PPP and BAU
    geom_ribbon(
      data = ppp_ribbon_data,
      aes(x = !!sym(year_col), ymin = ppp, ymax = bau),
      fill = ppp_bau_color, alpha = 0.5
    ) +
    
    # Base line (historical)
    geom_line(
      data = base_data,
      aes(x = !!sym(year_col), y = !!sym(emissions_col)),
      color = "black", linewidth = 1
    ) +
    
    # Diverging scenario lines
    geom_line(
      data = diverging_data %>% filter(scenario == "bau"),
      aes(x = !!sym(year_col), y = !!sym(emissions_col), color = "Business as usual"),
      linetype = "dashed", linewidth = 1
    ) +
    geom_line(
      data = diverging_data %>% filter(scenario == "ppp"),
      aes(x = !!sym(year_col), y = !!sym(emissions_col), color = "Potential policy pathways"),
      linewidth = 1
    ) +
    
    # Net-zero target as thick horizontal bar
    geom_segment(
      aes(x = target_year - 3, xend = target_year + 1, 
          y = target_value, yend = target_value,
          linetype = "Net zero target"),
      color = "black", linewidth = 1.5
    ) +
    
    # Manual color and linetype scales
    scale_color_manual(
      name = "Scenarios",
      values = c(
        "Business as usual" = "black",
        "Potential policy pathways" = ppp_bau_color
      ),
      breaks = c("Business as usual", "Potential policy pathways")
    ) +
    
    scale_linetype_manual(
      name = "Target",
      values = c("Net zero target" = "solid"),
      guide = guide_legend(override.aes = list(linewidth = 2))
    ) +
    
    # Legend for scenario lines
    guides(
      color = guide_legend(
        title = "Scenarios",
        order = 1,
        override.aes = list(
          linetype = c("dashed", "solid"),
          linewidth = c(1, 1)
        )
      ),
      linetype = guide_legend(
        title = "",
        order = 2
      )
    ) +
    
    labs(
      x = "Year",
      y = y_label,
      title = title
    ) +
    
    scale_y_continuous(labels = label_number(scale = 1e-6)) +
    scale_x_continuous(limits = x_limits) +
    
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),  # Remove vertical gridlines
      legend.position = "bottom",
      plot.title = element_text(size = 18),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 18),
      legend.key.width = unit(1.2, "cm"),
      legend.box = "vertical"
    )
  
  # Add y_max if specified
  if (!is.null(y_max)) {
    p <- p + coord_cartesian(ylim = c(0, y_max))
  }
  
  return(p)
}

