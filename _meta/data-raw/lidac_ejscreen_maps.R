# Clear old vars
rm(list = ls())


## Start here:
source(file.path(here::here(), "R/_load_pkgs.R"))
source(file.path(here::here(), "R/_quarto_helpers.R"))
source(file.path(here::here(), "R/plot_county_emissions.R"))


export.plots <- TRUE
outpath <- file.path(here::here(), "assets/maps/")


county_data <- readRDS("_meta/data/cprg_county.RDS")
LIDAC_polygons <- readRDS("_meta/data/lidac_block_groups.RDS")
CEJST_map_data <- readRDS("_meta/data/ejscreen_block_groups.RDS")


# Read the Metro Council logo
metc_logo <- png::readPNG(file.path(here::here(), "style/logo-no-text.png"))

# Dissolve the LIDAC boundaries
LIDAC_polygons_dissolved <- LIDAC_polygons %>%
  dplyr::group_by(LIDAC) %>%
  summarize(do_union = TRUE)


## png, ggnewscale, patchwork, classInt



# CUSTOM FUNS -------------------------------------------------------------
mutate_jenks_brks <- function(input, variable, ...) {
  var_string <- deparse(substitute(variable))
  # var_name <- enquo(variable)

  # add breaks
  jenks <- classInt::classIntervals(input[[var_string]], style = "jenks", ...)$brks

  if (any(0 == jenks)) {
    brk_vec <- jenks
  } else {
    brk_vec <- c(0, jenks)
  }

  # set labels
  lab_length <- length(brk_vec) - 1
  brk_labs <- vector(mode = "character", length = lab_length)

  # create labels dependent on the number of breaks specified
  for (i in seq(lab_length)) {
    if (any(brk_vec %% 1 != 0)) { # rounding to 2 decimal places
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
      include.lowest = TRUE
    )

  return(input)

  # labels can be changed directly by levels(input$variable) <- c("here","there","somewhere", ...)
}

sort_factor_column <- function(data, column_name) {
  data <- data %>%
    mutate(numeric_value = as.numeric(str_extract(!!sym(column_name), "\\d+"))) %>%
    arrange(numeric_value) %>%
    mutate(!!sym(column_name) := factor(!!sym(column_name), levels = unique(!!sym(column_name))))

  return(data)
}

palette_no <- "Blues"




# Make some maps! ---------------------------------------------------------
# plot_template
plot_template <- ggplot(county_data) +
  geom_sf(color = NA, fill = NA, lwd = 0) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.3, pad_x = unit(0.23, "in"), unit_category = "imperial") +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    height = unit(0.33, "in"), width = unit(0.35, "in"),
    pad_x = unit(1.65, "in"), pad_y = unit(0.35, "in"),
    style = ggspatial::north_arrow_minimal
  ) +
  annotation_custom(grid::rasterGrob(metc_logo),
    ymin = 44.48,  ymax = 44.6,
    xmin = -92.7,  xmax = -92.55
  ) +
  councilR::theme_council_geo()



# LIDAC boundary map
map_lidac <- plot_template +
  geom_sf(data = LIDAC_polygons, fill = "#FFFFFF", color = "gray80", alpha = 0.8, show.legend = F, lwd = 0.2) +
  geom_sf(color = "gray65", fill = NA, lwd = 0.7) +

  ggnewscale::new_scale_fill() + ## geoms added after this will use a new scale definition

  geom_sf(
    data = LIDAC_polygons_dissolved %>% filter(LIDAC == "Yes"),
    aes(fill = "#005DAA"), color = "#005DAA", alpha = 0.6, lwd = 0.3
  ) +
  scale_fill_identity(name = "", guide = "legend", labels = c("Low Income or \nDisadvantaged \nCommunities (LIDAC)")) +
  theme(
    legend.position = c(-0.08, 0.655),
    legend.direction = "vertical",
    legend.justification = c("left", "top"),
    legend.margin = margin(2, 0, 0, 0),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.box = "vertical",
    legend.box.just = "left"
  )

mini_map_lidac <- ggplot(county_data) +
  geom_sf(color = NA, fill = NA) +
  councilR::theme_council_geo()


map_lidac <- map_lidac + patchwork::inset_element(mini_map_lidac, left = 0.7, bottom = 0.5, right = 1.05, top = 1.05)



if (export.plots) png(paste0(outpath, "map_lidac.png"), width = 7, height = 5.5, units = "in", res = 600)
invisible(print(map_lidac))
dev.off()









# PM2.5.in.the.air..percentile.
map_PM2.5 <- plot_template +
  geom_sf(
    data = CEJST_map_data %>% # take your CEJST data
      mutate_jenks_brks(PM2.5.in.the.air..percentile., n = 4) %>% # create discrete breaks using n=4 breaks
      dplyr::group_by(PM2.5.in.the.air..percentile._brks) %>% # group by the new breaks
      summarize(do_union = TRUE) %>% sort_factor_column(., "PM2.5.in.the.air..percentile._brks") %>% # dissolve boundaries
      filter(!is.na(PM2.5.in.the.air..percentile._brks)), # remove NAs
    aes(fill = PM2.5.in.the.air..percentile._brks), alpha = 0.8, lwd = 0
  ) +
  geom_sf(color = "gray65", fill = NA, lwd = 0.4) + # add county lines

  scale_fill_brewer(
    name = expression(
      atop(
        textstyle("Inhalable fine particulate               "), # bit of a hacky solution but the extra spaces help with justifying this particular text
        atop(textstyle("matter ≤ 2.5" ~ mu * "m (" * PM[2.5] * ") percentile"), scriptscriptstyle(""))
      )
    ),
    type = "seq", palette = palette_no, guide = guide_legend(order = 1)
  ) +

  ggnewscale::new_scale_fill() + ## geoms added after this will use a new scale definition

  geom_sf(
    data = LIDAC_polygons_dissolved %>% filter(LIDAC == "Yes"),
    aes(fill = "orange"), color = "goldenrod1", alpha = 0.3, lwd = 0.3
  ) +
  scale_fill_identity(name = "", guide = "legend", labels = c("Low Income or \nDisadvantaged \nCommunities (LIDAC)")) +
  theme(
    legend.position = c(-0.08, 0.97),
    legend.direction = "vertical",
    legend.justification = c("left", "top"),
    legend.margin = margin(2, 0, 0, 0),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.box = "vertical",
    legend.box.just = "left"
  )

# Make a mini map while retaining block group boundaries (don't dissolve)
mini_map_PM2.5 <- ggplot(county_data) +
  geom_sf(
    data = CEJST_map_data %>% mutate_jenks_brks(PM2.5.in.the.air..percentile., n = 4),
    aes(fill = PM2.5.in.the.air..percentile._brks), color = "gray75", alpha = 0.8, show.legend = F, lwd = 0.07
  ) +
  geom_sf(color = "gray60", fill = NA, lwd = 0.2) +
  scale_fill_brewer(type = "seq", palette = palette_no) +
  councilR::theme_council_geo()


map_PM2.5 <- map_PM2.5 + patchwork::inset_element(mini_map_PM2.5, left = 0.7, bottom = 0.5, right = 1.05, top = 1.05)

if (export.plots) png(paste0(outpath, "map_PM2.5.png"), width = 7, height = 5.5, units = "in", res = 600)
invisible(print(map_PM2.5))
dev.off()




# Diesel.particulate.matter.exposure..percentile.
map_diesel <- plot_template +
  geom_sf(
    data = CEJST_map_data %>%
      mutate_jenks_brks(Diesel.particulate.matter.exposure..percentile., n = 4) %>%
      dplyr::group_by(Diesel.particulate.matter.exposure..percentile._brks) %>%
      summarize(do_union = TRUE) %>% sort_factor_column(., "Diesel.particulate.matter.exposure..percentile._brks") %>%
      filter(!is.na(Diesel.particulate.matter.exposure..percentile._brks)),
    aes(fill = Diesel.particulate.matter.exposure..percentile._brks), alpha = 0.8, lwd = 0
  ) +
  geom_sf(color = "gray65", fill = NA, lwd = 0.4) +

  scale_fill_brewer(
    name = expression(
      atop(
        textstyle("Inhalable diesel particulate"),
        atop(textstyle("matter exposure percentile"), scriptscriptstyle(""))
      )
    ),
    type = "seq", palette = palette_no, guide = guide_legend(order = 1)
  ) +

  ggnewscale::new_scale_fill() + ## geoms added after this will use a new scale definition

  geom_sf(
    data = LIDAC_polygons_dissolved %>% filter(LIDAC == "Yes"),
    aes(fill = "orange"), color = "goldenrod1", alpha = 0.3, lwd = 0.3
  ) +
  scale_fill_identity(name = "", guide = "legend", labels = c("Low Income or \nDisadvantaged \nCommunities (LIDAC)")) +
  theme(
    legend.position = c(-0.08, 0.97),
    legend.direction = "vertical",
    legend.justification = c("left", "top"),
    legend.margin = margin(2, 0, 0, 0),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.box = "vertical",
    legend.box.just = "left"
  )


mini_map_diesel <- ggplot(county_data) +
  geom_sf(
    data = CEJST_map_data %>% mutate_jenks_brks(Diesel.particulate.matter.exposure..percentile., n = 4),
    aes(fill = Diesel.particulate.matter.exposure..percentile._brks), color = "gray75", alpha = 0.8, show.legend = F, lwd = 0.07
  ) +
  geom_sf(color = "gray60", fill = NA, lwd = 0.2) +
  scale_fill_brewer(type = "seq", palette = palette_no) +
  councilR::theme_council_geo()



map_diesel <- map_diesel + patchwork::inset_element(mini_map_diesel, left = 0.7, bottom = 0.5, right = 1.05, top = 1.05)


if (export.plots) png(paste0(outpath, "map_diesel.png"), width = 7, height = 5.5, units = "in", res = 600)
invisible(print(map_diesel))
dev.off()




# Current.asthma.among.adults.aged.greater.than.or.equal.to.18.years..percentile.
map_asthma <- plot_template +
  geom_sf(
    data = CEJST_map_data %>%
      mutate_jenks_brks(Current.asthma.among.adults.aged.greater.than.or.equal.to.18.years..percentile., n = 4) %>%
      dplyr::group_by(Current.asthma.among.adults.aged.greater.than.or.equal.to.18.years..percentile._brks) %>%
      summarize(do_union = TRUE) %>% sort_factor_column(., "Current.asthma.among.adults.aged.greater.than.or.equal.to.18.years..percentile._brks") %>%
      filter(!is.na(Current.asthma.among.adults.aged.greater.than.or.equal.to.18.years..percentile._brks)),
    aes(fill = Current.asthma.among.adults.aged.greater.than.or.equal.to.18.years..percentile._brks), alpha = 0.8, lwd = 0
  ) +
  geom_sf(color = "gray65", fill = NA, lwd = 0.4) +

  scale_fill_brewer(
    name = expression(
      atop(
        textstyle("Asthma prevalence among"),
        atop(textstyle("adults (≥ 18 yrs) percentile"), scriptscriptstyle(""))
      )
    ),
    type = "seq", palette = palette_no, guide = guide_legend(order = 1)
  ) +

  ggnewscale::new_scale_fill() + ## geoms added after this will use a new scale definition

  geom_sf(
    data = LIDAC_polygons_dissolved %>% filter(LIDAC == "Yes"),
    aes(fill = "orange"), color = "goldenrod1", alpha = 0.3, lwd = 0.3
  ) +
  scale_fill_identity(name = "", guide = "legend", labels = c("Low Income or \nDisadvantaged \nCommunities (LIDAC)")) +
  theme(
    legend.position = c(-0.08, 0.97),
    legend.direction = "vertical",
    legend.justification = c("left", "top"),
    legend.margin = margin(2, 0, 0, 0),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.box = "vertical",
    legend.box.just = "left"
  )


mini_map_asthma <- ggplot(county_data) +
  geom_sf(
    data = CEJST_map_data %>% mutate_jenks_brks(Current.asthma.among.adults.aged.greater.than.or.equal.to.18.years..percentile., n = 4),
    aes(fill = Current.asthma.among.adults.aged.greater.than.or.equal.to.18.years..percentile._brks), color = "gray75", alpha = 0.8, show.legend = F, lwd = 0.07
  ) +
  geom_sf(color = "gray60", fill = NA, lwd = 0.2) +
  scale_fill_brewer(type = "seq", palette = palette_no) +
  councilR::theme_council_geo()

map_asthma <- map_asthma + patchwork::inset_element(mini_map_asthma, left = 0.7, bottom = 0.5, right = 1.05, top = 1.05)

if (export.plots) png(paste0(outpath, "map_asthma.png"), width = 7, height = 5.5, units = "in", res = 600)
invisible(print(map_asthma))
dev.off()




# Energy.burden..percentile.
map_energy.burden <- plot_template +
  geom_sf(
    data = CEJST_map_data %>%
      mutate_jenks_brks(Energy.burden..percentile., n = 5) %>%
      dplyr::group_by(Energy.burden..percentile._brks) %>%
      summarize(do_union = TRUE) %>% sort_factor_column(., "Energy.burden..percentile._brks") %>%
      filter(!is.na(Energy.burden..percentile._brks)),
    aes(fill = Energy.burden..percentile._brks), alpha = 0.8, lwd = 0
  ) +
  geom_sf(color = "gray65", fill = NA, lwd = 0.4) +

  scale_fill_brewer(
    name = expression(
      atop(
        textstyle("Energy cost burdened"),
        atop(textstyle("households percentile"), scriptscriptstyle(""))
      )
    ),
    type = "seq", palette = palette_no, guide = guide_legend(order = 1)
  ) +

  ggnewscale::new_scale_fill() + ## geoms added after this will use a new scale definition

  geom_sf(
    data = LIDAC_polygons_dissolved %>% filter(LIDAC == "Yes"),
    aes(fill = "orange"), color = "goldenrod1", alpha = 0.3, lwd = 0.3
  ) +
  scale_fill_identity(name = "", guide = "legend", labels = c("Low Income or \nDisadvantaged \nCommunities (LIDAC)")) +
  theme(
    legend.position = c(-0.08, 0.97),
    legend.direction = "vertical",
    legend.justification = c("left", "top"),
    legend.margin = margin(2, 0, 0, 0),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.box = "vertical",
    legend.box.just = "left"
  )


mini_map_energy.burden <- ggplot(county_data) +
  geom_sf(
    data = CEJST_map_data %>% mutate_jenks_brks(Energy.burden..percentile., n = 5),
    aes(fill = Energy.burden..percentile._brks), color = "gray75", alpha = 0.8, show.legend = F, lwd = 0.07
  ) +
  geom_sf(color = "gray60", fill = NA, lwd = 0.2) +
  scale_fill_brewer(type = "seq", palette = palette_no) +
  councilR::theme_council_geo()


map_energy.burden <- map_energy.burden + patchwork::inset_element(mini_map_energy.burden, left = 0.7, bottom = 0.5, right = 1.05, top = 1.05)

if (export.plots) png(paste0(outpath, "map_energy.burden.png"), width = 7, height = 5.5, units = "in", res = 600)
invisible(print(map_energy.burden))
dev.off()


# Housing.burden..percent...percentile.
map_housing.burden <- plot_template +
  geom_sf(
    data = CEJST_map_data %>%
      mutate_jenks_brks(Housing.burden..percent...percentile., n = 5) %>%
      dplyr::group_by(Housing.burden..percent...percentile._brks) %>%
      summarize(do_union = TRUE) %>% sort_factor_column(., "Housing.burden..percent...percentile._brks") %>%
      filter(!is.na(Housing.burden..percent...percentile._brks)),
    aes(fill = Housing.burden..percent...percentile._brks), alpha = 0.8, lwd = 0
  ) +
  geom_sf(color = "gray65", fill = NA, lwd = 0.4) +

  scale_fill_brewer(
    name = expression(
      atop(
        textstyle("Housing cost burdened"),
        atop(textstyle("households percentile  "), scriptscriptstyle(""))
      )
    ),
    type = "seq", palette = palette_no, guide = guide_legend(order = 1)
  ) +

  ggnewscale::new_scale_fill() + ## geoms added after this will use a new scale definition

  geom_sf(
    data = LIDAC_polygons_dissolved %>% filter(LIDAC == "Yes"),
    aes(fill = "orange"), color = "goldenrod1", alpha = 0.3, lwd = 0.3
  ) +
  scale_fill_identity(name = "", guide = "legend", labels = c("Low Income or \nDisadvantaged \nCommunities (LIDAC)")) +
  theme(
    legend.position = c(-0.08, 0.97),
    legend.direction = "vertical",
    legend.justification = c("left", "top"),
    legend.margin = margin(2, 0, 0, 0),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.box = "vertical",
    legend.box.just = "left"
  )


mini_map_housing.burden <- ggplot(county_data) +
  geom_sf(
    data = CEJST_map_data %>% mutate_jenks_brks(Housing.burden..percent...percentile., n = 5),
    aes(fill = Housing.burden..percent...percentile._brks), color = "gray75", alpha = 0.8, show.legend = F, lwd = 0.07
  ) +
  geom_sf(color = "gray60", fill = NA, lwd = 0.2) +
  scale_fill_brewer(type = "seq", palette = palette_no) +
  councilR::theme_council_geo()


map_housing.burden <- map_housing.burden + patchwork::inset_element(mini_map_housing.burden, left = 0.7, bottom = 0.5, right = 1.05, top = 1.05)

if (export.plots) png(paste0(outpath, "map_housing.burden.png"), width = 7, height = 5.5, units = "in", res = 600)
invisible(print(map_housing.burden))
dev.off()





# Share.of.the.tract.s.land.area.that.is.covered.by.impervious.surface.or.cropland.as.a.percent..percentile.
map_impervious <- plot_template +
  geom_sf(
    data = CEJST_map_data %>%
      mutate_jenks_brks(Share.of.the.tract.s.land.area.that.is.covered.by.impervious.surface.or.cropland.as.a.percent..percentile., n = 4) %>%
      dplyr::group_by(Share.of.the.tract.s.land.area.that.is.covered.by.impervious.surface.or.cropland.as.a.percent..percentile._brks) %>%
      summarize(do_union = TRUE) %>% sort_factor_column(., "Share.of.the.tract.s.land.area.that.is.covered.by.impervious.surface.or.cropland.as.a.percent..percentile._brks") %>%
      filter(!is.na(Share.of.the.tract.s.land.area.that.is.covered.by.impervious.surface.or.cropland.as.a.percent..percentile._brks)),
    aes(fill = Share.of.the.tract.s.land.area.that.is.covered.by.impervious.surface.or.cropland.as.a.percent..percentile._brks), alpha = 0.8, lwd = 0
  ) +
  geom_sf(color = "gray65", fill = NA, lwd = 0.4) +

  scale_fill_brewer(
    name = expression(
      atop(
        textstyle("Share of tract covered by impervious"),
        atop(textstyle("surface or cropland percentile           "), scriptscriptstyle(""))
      )
    ),
    type = "seq", palette = palette_no, guide = guide_legend(order = 1)
  ) +

  ggnewscale::new_scale_fill() + ## geoms added after this will use a new scale definition

  geom_sf(
    data = LIDAC_polygons_dissolved %>% filter(LIDAC == "Yes"),
    aes(fill = "orange"), color = "goldenrod1", alpha = 0.3, lwd = 0.3
  ) +
  scale_fill_identity(name = "", guide = "legend", labels = c("Low Income or \nDisadvantaged \nCommunities (LIDAC)")) +
  theme(
    legend.position = c(-0.08, 0.97),
    legend.direction = "vertical",
    legend.justification = c("left", "top"),
    legend.margin = margin(2, 0, 0, 0),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.box = "vertical",
    legend.box.just = "left"
  )


mini_map_impervious <- ggplot(county_data) +
  geom_sf(
    data = CEJST_map_data %>% mutate_jenks_brks(Share.of.the.tract.s.land.area.that.is.covered.by.impervious.surface.or.cropland.as.a.percent..percentile., n = 4),
    aes(fill = Share.of.the.tract.s.land.area.that.is.covered.by.impervious.surface.or.cropland.as.a.percent..percentile._brks), color = "gray75", alpha = 0.8, show.legend = F, lwd = 0.07
  ) +
  geom_sf(color = "gray60", fill = NA, lwd = 0.2) +
  scale_fill_brewer(type = "seq", palette = palette_no) +
  councilR::theme_council_geo()


map_impervious <- map_impervious + patchwork::inset_element(mini_map_impervious, left = 0.7, bottom = 0.5, right = 1.05, top = 1.05)

if (export.plots) png(paste0(outpath, "map_impervious.png"), width = 7, height = 5.5, units = "in", res = 600)
invisible(print(map_impervious))
dev.off()
