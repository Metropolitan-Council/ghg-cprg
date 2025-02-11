
### holding for reintegration into data_nhd_waterways later
```{r include=FALSE}
# Plot NHD waterways map
waterway_colors <- c(
  "LakePond" = "steelblue",
  "StreamRiver" = "lightskyblue",
  "Reservoir" = "maroon3",
  "SwampMarsh" = "palegreen3",
  "Lock Chamber" = "purple3",
  "DamWeir" = "darkslategray4",
  "Connector" = "red",
  "CanalDitch" = "goldenrod",
  "Underground Conduit" = "darkorchid1",
  "Pipeline" = "darkslategray",
  "ArtificialPath" = "green"
)

p_nhd_waterways <- ggplot(cprg_county) +
  geom_sf(color = "gray20", fill = NA, lwd = 0.5) +
  councilR::theme_council_geo() +
  geom_sf(data = nhd_area, aes(fill = FTYPE, color = FTYPE), lwd = 0.1) +
  scale_fill_manual("Waterway Type",
                    breaks = names(waterway_colors),
                    values = as.character(waterway_colors), guide = guide_legend(order = 1)
  ) +
  scale_color_manual("Waterway Type",
                     breaks = names(waterway_colors),
                     values = as.character(waterway_colors), guide = guide_legend(order = 1)
  ) +
  ggnewscale::new_scale_fill() + ## geoms added after this will use a new scale definition
  ggnewscale::new_scale_color() + ## geoms added after this will use a new scale definition
  
  geom_sf(data = nhd_flowlines, aes(color = FTYPE), lwd = 0.2, alpha = 0.7) +
  scale_color_manual("",
                     breaks = names(waterway_colors),
                     values = as.character(waterway_colors), guide = guide_legend(order = 2, title = NULL)
  ) +
  geom_sf(data = cprg_ctu, fill = NA, color = councilR::colors$suppGray, alpha = 0.8, lwd = 0.2) +
  geom_sf(color = councilR::colors$suppGray, fill = NA, lwd = 0.6) + # add county lines
  
  ggspatial::annotation_scale(
    location = "br", width_hint = 0.3,
    pad_x = unit(0.23, "in"), unit_category = "imperial"
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    height = unit(0.33, "in"), width = unit(0.35, "in"),
    pad_x = unit(1.4, "in"), pad_y = unit(0.35, "in"),
    style = ggspatial::north_arrow_minimal
  )
```

```{r fig-nhd-water}
#| fig-cap: "USGS National Hydrography Dataset (NHD) - Waterways"
#| out-width: "95%"
#| echo: false
p_nhd_waterways
```
