source("R/_load_pkgs.R")
waste_comp <- readRDS(file.path(here::here(), "_waste/data-raw/solid_waste/mpca_waste_composition.RDS"))

waste_comp <- waste_comp %>%
  mutate(
    Category = case_when(
      Category == "Organics (Non-Food)" ~ "Yard Waste",
      Category == "Organics (Food Waste)" ~ "Food Waste",
      Category == "Textiles" ~ "Cloth",
      Category == "Household Waste" ~ "HHW",
      .default = Category
    )
  ) %>%
  filter(Category != "Electronics") %>%
  group_by(Category) %>%
  summarize(percentage = sum(Mean))


fig_waste_composition <-
  plot_ly(
    type = "bar",
    # source = "fig-solid-waste-emissions",
    data = waste_comp,
    x = ~ reorder(Category, percentage, decreasing = TRUE),
    y = ~percentage
    # color = ~Category
  ) %>%
  plotly_layout(
    main_title = "2013 Minnesota Waste Composition",
    subtitle = "",
    y_title = "Percentage of Total Waste",
    x_title = "Source"
  ) %>%
  layout(
    # barmode = "stack",
    legend = list(
      traceorder = "reversed"
    ),
    yaxis = list(
      range = c(0, .25),
      tickformat = ".0%"
    )
  )
fig_waste_composition
