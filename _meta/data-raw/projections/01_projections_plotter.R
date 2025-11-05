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
    x_limits = c(2005, 2059)
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
  
  # Calculate position for text annotation
  text_bau <- diverging_data %>% 
    filter(!!sym(year_col) == 2050, scenario == "bau") %>% 
    pull(!!sym(emissions_col))
  
  # Calculate position for text annotation
  text_ppp <- diverging_data %>% 
    filter(!!sym(year_col) == 2040, scenario == "ppp") %>% 
    pull(!!sym(emissions_col))
  
  # Create the plot
  p <- ggplot() +
    # Gray fill for all historical and under PPP
    geom_ribbon(
      data = base_data,
      aes(x = !!sym(year_col), ymin = 0, ymax = !!sym(emissions_col), fill = "Emissions"),
      alpha = 0.7
    ) +
    geom_ribbon(
      data = diverging_data %>% filter(scenario == "ppp"),
      aes(x = !!sym(year_col), ymin = 0, ymax = !!sym(emissions_col), fill = "Emissions"),
      alpha = 0.7
    ) +
    
    # Ribbon between PPP and BAU
    geom_ribbon(
      data = ppp_ribbon_data,
      aes(x = !!sym(year_col), ymin = ppp, ymax = bau, 
          fill = "Potential policy pathway emissions reductions"),
      alpha = 0.5
    ) +
    
    # Base line (historical)
    geom_line(
      data = base_data,
      aes(x = !!sym(year_col), y = !!sym(emissions_col)),
      color = "black", linewidth = 1, linetype = "dashed"
    ) +
    
    # Diverging scenario lines
    geom_line(
      data = diverging_data %>% filter(scenario == "bau"),
      aes(x = !!sym(year_col), y = !!sym(emissions_col), linetype = "Business as usual"),
      color = "black", linewidth = 1
    ) +
    geom_line(
      data = diverging_data %>% filter(scenario == "ppp"),
      aes(x = !!sym(year_col), y = !!sym(emissions_col)),  # Remove linetype aesthetic
      color = "black", linewidth = 1, linetype = "dotted"  # Set linetype directly
    ) +
    
    # Net-zero target as thick horizontal bar
    geom_segment(
      aes(x = target_year - .5, xend = target_year + 1.5, 
          y = target_value, yend = target_value),
      color = "black", linewidth = 1.5, linetype = "solid"
    ) +
    
    # Axis lines
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_vline(xintercept = 2005, color = "black", linewidth = 0.5) +
    
    # Manual fill scale for ribbons
    scale_fill_manual(
      name = "",
      values = c(
        "Emissions" = "gray80",
        "Potential policy pathway emissions reductions" = ppp_bau_color
      ),
      breaks = c("Emissions", "Potential policy pathway emissions reductions"),
      guide = "none"
    ) +
    
    # # Manual linetype scale (single scale for all linetypes)
    scale_linetype_manual(
      name = "",
      values = c(
        "Business as usual" = "dashed"
      ),
      breaks = c("Business as usual"),
      guide = "none"
    ) +
    # 
    # # Combine legends
    # guides(
    #   linetype = guide_legend(order = 1)
    # ) +
    # 
    labs(
      x = "",
      y = y_label,
      title = title
    ) +
    
    scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
    scale_x_continuous(
      limits = x_limits,
      breaks = seq(2010, x_limits[2], by = 10)
    ) +
    
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(size = 18),
      axis.text = element_text(size = 14, color = "black"),
      legend.text = element_text(size = 18),
      legend.key.width = unit(1.2, "cm"),
      legend.box = "vertical",
      plot.margin = ggplot2::margin(5.5, 5.5, 30, 5.5, "pt")
    ) +

    # Add text annotation for BAU
    annotate("text", x = 2050.5, y = text_bau, 
             label = "Business-as-usual",
             size = 5, hjust = 0, vjust = 0.5, fontface = "bold") +
    
    # for PPP wedge
    annotate("text", x = 2050.5, y = text_ppp, 
             label = "Emissions reduced by\npotential policy pathway",
             size = 5, hjust = 0, vjust = 0.5, fontface = "bold") +
    # and net-zero
    annotate("text", x = 2052, y = target_value, 
             label = "Net zero target",
             size = 5, hjust = 0, vjust = 0, fontface = "bold") +
    
    # Add "Inventory" and "Projections" annotations below x-axis
    annotation_custom(
      grob = grid::textGrob("Inventory", gp = grid::gpar(fontsize = 14), vjust = 3),
      xmin = 2010, xmax = 2010, ymin = -Inf, ymax = -Inf
    ) +
    annotation_custom(
      grob = grid::textGrob("Projections", gp = grid::gpar(fontsize = 14), vjust = 3),
      xmin = 2030, xmax = 2030, ymin = -Inf, ymax = -Inf
    )
  
  # Add y_max if specified
  if (!is.null(y_max)) {
    p <- p + coord_cartesian(ylim = c(0, y_max), clip = "off")
  } else {
    p <- p + coord_cartesian(clip = "off")
  }
  
  return(p)
}
