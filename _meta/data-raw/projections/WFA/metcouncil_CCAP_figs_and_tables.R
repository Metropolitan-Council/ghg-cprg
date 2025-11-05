# Figures & Tables for Met Council CCAP
# Purpose: Produce industry figures from CCAP scopes,
# occupation tables
# occupation wages
# occupation demographics
# training programs
# traning program graduates (vacancies in related SOCs?)
# Input:
# Dependencies: final ccap scopes, QCEW, Projections, OEWS, priority occupation data...

# ---- Setup ----
library(tidyverse)
library(readxl)
library(writexl)
library(fuzzyjoin)
library(janitor)
library(scales)
library(ggrepel)
library(ggtext)
library(patchwork)
library(Polychrome)
library(viridis)
library(tidygraph)
library(igraph)
library(ggraph)

data_path <- "_meta/data-raw/projections/WFA"
csv_files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)

for (file in csv_files) {
  name <- tools::file_path_sans_ext(basename(file))
  assign(name, read_csv(file), envir = .GlobalEnv)
}


# ---- Overview of Workforce Demand (industry employment) ----
metro_sector_ind
metro_sector_emp
#
# # Metro build donut
# p_ind <- ggplot(metro_sector_ind, aes(x = 2, y = naics_2_share, fill = naics_2_label)) +
#   geom_col(width = 1, color = "white") +
#   geom_text(
#     data = metro_ind_total ,
#     aes(x = 0.5, y = 0, label = n),  # center
#     inherit.aes = FALSE,
#     size = 4, fontface = "bold"
#   ) +
#   coord_polar(theta = "y") +
#   xlim(0.5, 2.5) +
#   scale_fill_manual(name = "Industry Sector",
#                     values = unname(palette36.colors((n_distinct(metro_sector_ind$naics_2_label))))) +
#   theme_void() +
#   labs(title = "Scoped Industries",
#        subtitle = "Number in center = total NAICS codes")
#
# p_emp <- ggplot(metro_sector_emp, aes(x = 2, y = naics_2_share, fill = naics_2_label)) +
#   geom_col(width = 1, color = "white") +
#   geom_text(
#     data = metro_emp_total ,
#     aes(x = 0.5, y = 0, label = comma(total_emp)),  # center
#     inherit.aes = FALSE,
#     size = 4, fontface = "bold"
#   ) +
#   coord_polar(theta = "y") +
#   xlim(0.5, 2.5) +
#   scale_fill_manual(name = "Industry Sector",
#                     values = unname(palette36.colors((n_distinct(metro_sector_emp$naics_2_label))))) +
#   theme_void() +
#   labs(title = "Scoped Employment",
#        subtitle = "Number in center = total employment")
#
#
# final_plot <- p_ind + p_emp +
#   plot_layout(guides = "collect") &
#   plot_annotation(
#     caption = "Data source: QCEW") &
#   theme(legend.position = "bottom",
#         legend.title.position = "top",
#         legend.title = element_text(hjust = 0.5))
#
# final_plot
#
# ggsave(file.path(output_path, "fig1_metro_scope_ind_emp_donuts.png"), width = 6, height = 5.5, dpi = 300)
#
# saveRDS(final_plot, file.path(output_path,"fig1.rds"))


## ---- Industry employment time trends ----

#### ---- timetrends figure ----

in_scope_emp <- ind_emp %>%
  filter(scope == "in scope")

in_scope_plot <- ggplot(in_scope_emp, aes(x = year, y = employment)) +
  geom_point() +
  geom_line(data = subset(in_scope_emp, year <= 2024), size = 1, color = "black") +
  geom_line(data = subset(in_scope_emp, year >= 2024), size = 1, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = comma, limits = c(0, 150000)) +
  labs(
    title = paste0("Employment projections for in scope industries"),
    caption = "Source: QCEW + Projections",
    subtitle = "Historical and projected employment",
    y = "",
    x = "Year",
    color = ""
  ) +
  theme_minimal(base_size = 11) +
  guides(
    color = "none"
  ) +
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
  )

in_scope_plot

ggsave(
  plot = in_scope_plot,
  filename = paste0(here::here(), "/imgs/in_scope_emp.png"), # add your file path here
  width = 10,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

ind_emp_graph <- ind_emp_naics2_inscope %>%
  mutate(naics_graph = case_when(
    naics_2_label == "Construction" ~ "Construction",
    naics_2_label == "Manufacturing" ~ "Manufacturing",
    TRUE ~ "Other"
  )) %>%
  group_by(year, naics_graph) %>%
  summarize(employment = sum(employment)) %>%
  ungroup()

ind_naics_plot <- ggplot(
  ind_emp_graph,
  aes(x = year, y = employment, col = naics_graph)
) +
  geom_point() +
  geom_line(data = subset(ind_emp_graph, year <= 2024), size = 1) +
  geom_line(data = subset(ind_emp_graph, year >= 2024), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_y_continuous(labels = comma, limits = c(0, 100000)) +
  labs(
    title = paste0("Employment by In Scope by Industry Sector"),
    caption = "Source: QCEW + Projections",
    subtitle = "Historical and projected employment",
    x = "Year",
    y = "",
    color = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 14, color = "black"),
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.2, "cm"),
    legend.box = "vertical",
    plot.margin = ggplot2::margin(5.5, 5.5, 30, 5.5, "pt")
  )

ind_naics_plot

ggsave(
  plot = ind_naics_plot,
  filename = paste0(here::here(), "/imgs/in_scope_emp_naics.png"), # add your file path here
  width = 10,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)



## ---- Wages ----
wage_dumbell <- onet_scored_OID_occdetails %>%
  filter(soc_code %in% c(priority_underBA$`SOC Code`, priority_overBA$`SOC Code`)) %>%
  arrange(desc(priority_score)) %>%
  select(soc_code, soc_title, priority_score) %>%
  left_join(
    oes_6_for_loop_msa %>%
      select(soc_code, starts_with("h")),
    by = "soc_code"
  ) %>%
  mutate(soc_title_wrapped = str_wrap(soc_title, width = 40)) %>%
  mutate(soc_title_wrapped = fct_reorder(soc_title_wrapped, desc(priority_score)))


cost_of_living_7co <- 24.53


p_underBA <- ggplot(
  wage_dumbell %>% filter(soc_code %in% priority_underBA$`SOC Code`),
  aes(x = soc_title_wrapped)
) +
  geom_linerange(aes(ymin = h_pct25, ymax = h_pct75, colour = "25th–75th Percentile"),
    linewidth = 1.2
  ) +
  geom_point(aes(y = h_median, color = "Median"), size = 3) +
  geom_hline(yintercept = cost_of_living_7co, linetype = "dashed", color = "darkgreen") +
  annotate("text",
    x = 1, y = 12,
    label = "Cost of Living, Metro",
    size = 3.5, color = "darkgreen"
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 110, by = 20),
    labels = scales::dollar_format()
  ) +
  scale_color_manual(
    values = c("25th–75th Percentile" = "steelblue", "Median" = "red"),
    name = NULL
  ) +
  labs(
    title = "Occupations without required bachelors",
    x = NULL,
    y = "Hourly Wage ($)"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 14, color = "black"),
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.2, "cm"),
    legend.box = "vertical",
    plot.margin = ggplot2::margin(5.5, 5.5, 30, 5.5, "pt")
  )

p_overBA <- ggplot(
  wage_dumbell %>% filter(soc_code %in% priority_overBA$`SOC Code`),
  aes(x = soc_title_wrapped)
) +
  geom_linerange(aes(ymin = h_pct25, ymax = h_pct75, colour = "25th–75th Percentile"),
    linewidth = 1.2
  ) +
  geom_point(aes(y = h_median, color = "Median"), size = 3) +
  geom_hline(yintercept = cost_of_living_7co, linetype = "dashed", color = "darkgreen") +
  annotate("text",
    x = 1, y = 12,
    label = "Cost of Living, Metro",
    size = 3.5, color = "darkgreen"
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 110, by = 20),
    labels = scales::dollar_format()
  ) +
  scale_color_manual(
    values = c("25th–75th Percentile" = "steelblue", "Median" = "red"),
    name = NULL
  ) +
  labs(
    title = "Occupations with required bachelors",
    x = NULL,
    y = "Hourly Wage ($)"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 14, color = "black"),
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.2, "cm"),
    legend.box = "vertical",
    plot.margin = ggplot2::margin(5.5, 5.5, 30, 5.5, "pt")
  )

# Stack vertically (ncol = 1)
final_plot <- p_underBA + p_overBA +
  plot_layout(guides = "collect", ncol = 1) +
  plot_annotation(caption = "Source: OEWS") &
  theme(legend.position = "right")

final_plot


ggsave(
  plot = final_plot,
  filename = paste0(here::here(), "/imgs/priority_wages.png"), # add your file path here
  width = 14,
  height = 12,
  units = "in",
  dpi = 300,
  bg = "white"
)



## ---- work context & job quality framing

## ---- demographics ----
data <- read_csv(file.path(data_path, "TCMSA_acs_2023_occ_demo.csv"))

soc_2018_df <- read_xlsx(file.path(gen_data_path, "soc_2018_definitions.xlsx")) %>%
  clean_names() %>%
  select(!c(soc_code, soc_definition)) %>%
  rename(soc_code = soc_code_num) %>%
  mutate(
    across(everything(), as.character),
    across(everything(), str_squish)
  )

df_occ <- data %>%
  filter(OCCSOC != "0" & !is.na(OCCSOC)) %>%
  mutate(soc_code = str_replace_all(OCCSOC, "[XY]", "0")) %>%
  select(-OCCSOC) %>%
  left_join(soc_2018_df, by = "soc_code") %>%
  mutate(
    age_group = cut(AGE,
      breaks = c(15, 24, 34, 44, 54, 64, Inf),
      labels = c(
        "16–24", "25–34", "35–44",
        "45–54", "55–64", "65+"
      ),
      right = TRUE
    ),
    race_label = case_when(
      RACE == 1 ~ "White",
      RACE == 2 ~ "Black or African American",
      RACE == 3 ~ "American Indian or Alaska Native",
      RACE == 4 ~ "Chinese",
      RACE == 5 ~ "Japanese",
      RACE == 6 ~ "Other Asian or Pacific Islander",
      RACE == 7 ~ "Other race, nec",
      RACE == 8 ~ "Two major races",
      RACE == 9 ~ "Three or more major races",
      TRUE ~ NA_character_
    ),
    sex_label = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      SEX == 9 ~ "Missing/blank",
      TRUE ~ NA_character_
    ),
    educ_group = case_when(
      EDUC %in% c(0, 1, 2, 3, 4, 5) ~ "< High school", # less than HS
      EDUC == 6 ~ "High school diploma or GED",
      EDUC %in% c(7, 8, 9) ~ "Some college / Associate's",
      EDUC == 10 ~ "Bachelor's degree",
      EDUC == 11 ~ "Graduate degree",
      EDUC == 99 ~ "Missing/blank",
      TRUE ~ NA_character_
    )
  )


occ_emp <- df_occ %>%
  group_by(soc_code) %>%
  summarise(
    workers = sum(PERWT, na.rm = TRUE),
    .groups = "drop"
  )


occ_crosstab <- function(df, demo_var) {
  df %>%
    group_by(soc_code, !!sym(demo_var)) %>%
    summarise(
      workers = sum(PERWT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(soc_code) %>%
    mutate(pct = workers / sum(workers) * 100) %>%
    ungroup()
}

# Summarize by SOC
summarise_soc <- function(df, soc, demo_vars = c("sex_label", "race_label", "educ_group", "age_group")) {
  df <- df %>% filter(soc_code == soc)

  results <- lapply(demo_vars, function(var) {
    out <- occ_crosstab(df, var)
    out$demo_var <- var
    out
  })

  bind_rows(results)
}

summarise_soc_multi <- function(df, socs, demo_vars = c("sex_label", "race_label", "educ_group", "age_group")) {
  bind_rows(lapply(socs, function(s) summarise_soc(df, s, demo_vars)))
}

# Sum stats for 11, 13, 15, 17, 47, 49, 51, 53

occ_soc2_priority <- df_occ %>%
  mutate(
    soc_group2 = str_sub(soc_code, 1, 2) # 2-digit SOC family
  ) %>%
  filter(soc_group2 %in% c(
    "11", "13", "15", "17",
    "47", "49", "51", "53"
  )) %>%
  mutate(soc_code = paste0(soc_group2, "0000")) %>%
  select(-soc_group2)

results <- summarise_soc_multi(occ_soc2_priority,
  socs = c(
    "110000", "130000", "150000", "170000",
    "470000", "490000", "510000", "530000"
  )
)

make_demo_block <- function(df, soc) {
  demo <- df %>%
    filter(soc_code == soc) %>%
    mutate(group = coalesce(sex_label, race_label, educ_group, as.character(age_group))) %>%
    select(demo_var, group, pct)

  sex <- demo %>%
    filter(demo_var == "sex_label") %>%
    select(Sex = group, Sex_pct = pct)
  age <- demo %>%
    filter(demo_var == "age_group") %>%
    select(Age = group, Age_pct = pct)
  edu <- demo %>%
    filter(demo_var == "educ_group") %>%
    select(Edu = group, Edu_pct = pct)
  race <- demo %>%
    filter(demo_var == "race_label") %>%
    select(Race = group, Race_pct = pct)

  # Find max length
  max_len <- max(nrow(sex), nrow(age), nrow(edu), nrow(race))

  # Pad each df to max_len rows
  pad_df <- function(x, n, cols) {
    if (nrow(x) < n) {
      rbind(x, as.data.frame(matrix(NA, n - nrow(x), length(cols),
        dimnames = list(NULL, cols)
      )))
    } else {
      x
    }
  }

  sex <- pad_df(sex, max_len, c("Sex", "Sex_pct"))
  age <- pad_df(age, max_len, c("Age", "Age_pct"))
  edu <- pad_df(edu, max_len, c("Edu", "Edu_pct"))
  race <- pad_df(race, max_len, c("Race", "Race_pct"))

  # Combine into one block
  bind_cols(sex, age, edu, race)
}

soc2_codes <- unique(results$soc_code)

export_list <- setNames(
  lapply(soc2_codes, function(code) make_demo_block(results, code)),
  soc2_codes
)

write_xlsx(export_list, file.path(output_path, "SOC2_demographics_blocks.xlsx"))

# Occ group titles
maj_occs <- soc_2018_df %>%
  filter(soc_code %in% c(
    "110000", "130000", "150000", "170000",
    "470000", "490000", "510000", "530000"
  )) %>%
  select(soc_code, soc_title)


# ---- Current Workforce & Training Landscape ----

# Green occupation by min education requirement?
table(scope_scored_OID$onet, scope_scored_OID$education_requirements, useNA = "ifany")

all_underBA <- onet_scored_OID_occdetails %>%
  filter(!(education_requirements == "Bachelor's degree" |
    education_requirements == "Doctoral or professional degree" |
    education_requirements == "Graduate or professional degree"))

table(all_underBA$education_requirements)

all_overBA <- onet_scored_OID_occdetails %>%
  filter((education_requirements == "Bachelor's degree" |
    education_requirements == "Doctoral or professional degree" |
    education_requirements == "Graduate or professional degree"))

table(all_overBA$education_requirements)

## ---- Transferable skills data & figures (jobSTAT & cip-soc crosswalk & certicifactions)----
jobstat <- read_csv(file.path(occ_data_path, "jobstat_skillsmatch.csv")) %>%
  clean_names() %>%
  mutate(
    code_x = as.character(code_x),
    code_y = as.character(code_y)
  )

cip_soc <- read_xlsx(file.path(gen_data_path, "CIP2020_SOC2018_Crosswalk.xlsx"), sheet = "CIP-SOC") %>%
  clean_names() %>%
  mutate(soc_code = str_remove_all(soc2018code, "-"))

cip_6digit <- cip_soc %>%
  filter(!str_ends(cip2020code, "0000")) %>%
  mutate(
    cip_2digit = paste0(str_sub(cip2020code, 1, 2), ".0000"),
    cip_4digit = paste0(str_sub(cip2020code, 1, 5), "00")
  )

cert_files <- list.files(cert_data_path, full.names = TRUE)
cert_names <- tools::file_path_sans_ext(basename(cert_files))

for (i in 1:length(cert_files)) {
  df <- read_xlsx(cert_files[i])

  assign(cert_names[i], df)
}

online_cert_ids <- CERTIFICATIONS %>%
  filter(SUPPRESS == 0 & DELETED == 0 & VERIFIED == 1) %>%
  select(CERT_ID)

cert_indemand <- CERTXACC %>%
  mutate(indemand = ifelse(ACCRED_ID == 8, 1, 0)) %>%
  select(!ACCRED_ID)

cert_details <- CERT_ONET_ASSIGN %>%
  select(!ACTIVE_YN) %>%
  left_join(cert_indemand, by = "CERT_ID") %>%
  left_join(CERTXTYPE, by = "CERT_ID") %>%
  filter(CERT_ID %in% online_cert_ids$CERT_ID) %>%
  mutate(soccode = as.numeric(str_remove(substr(ONETCODE, 1, 7), "-"))) %>%
  select(!ONETCODE) %>%
  arrange(soccode, CERT_ID, RELATION) %>%
  distinct() %>%
  mutate(onet = soccode %in% all_onet$soc_code)



### --- Set focus occupation, make cip dfs & figure ----
focus <- "499041" # Industrial machinary mechanics

focus_cips <- cip_6digit %>%
  filter(soc_code == focus)

nodes_df <- jobstat %>%
  filter(code_x == focus) %>%
  distinct(code_x, code_y, shine, soctitle_x, soctitle_y) %>%
  arrange(desc(shine)) %>%
  head(16)

nodes_cips <- cip_6digit %>%
  filter(soc_code %in% nodes_df$code_y)

edges_df <- nodes_cips %>%
  left_join(nodes_cips %>% select(cip2020code, cip_4digit, soc_code, soc2018title),
    by = "cip_4digit", suffix = c("_from", "_to")
  ) %>%
  filter(soc_code_from != soc_code_to) %>%
  mutate(edge_type = ifelse(cip2020code_from == cip2020code_to, "specific", "field")) %>%
  mutate(
    pair_min = pmin(soc_code_from, soc_code_to),
    pair_max = pmax(soc_code_from, soc_code_to)
  ) %>%
  distinct(pair_max, pair_min, cip2020code_from, cip2020code_to, .keep_all = TRUE) %>%
  group_by(pair_min, pair_max, edge_type) %>%
  summarise(shared_cips = n_distinct(cip2020code_to)) %>%
  rename()


nodes_clean <- tibble(soc_code = unique(c(edges_df$pair_min, edges_df$pair_max))) %>%
  full_join(nodes_df %>% select(code_y, soctitle_y, shine),
    by = c("soc_code" = "code_y")
  )

# rename columns to expected format
nodes_ready <- nodes_clean %>%
  rename(name = soc_code, soc_title = soctitle_y) %>%
  mutate(soc_label = str_wrap(soc_title, width = 20))

edges_ready <- edges_df %>%
  rename(from = pair_min, to = pair_max)

# layout, position but shine score
layout <- nodes_ready %>%
  mutate(
    radius = (100 - shine) * .5, # smaller shine -> farther
    angle = seq(0, 2 * pi, length.out = n()), # spread around circle
    x = radius * cos(angle),
    y = radius * sin(angle)
  )

# jitter layout
set.seed(123)
layout <- layout %>%
  arrange(desc(shine)) %>%
  mutate(
    angle = angle + runif(n(), -0.15, 0.15)
  ) %>%
  mutate(
    x = ifelse(name == focus, 0, x),
    y = ifelse(name == focus, 0, y)
  )

# build graph
graph <- tbl_graph(
  nodes = layout, # contains x/y already
  edges = edges_ready,
  directed = FALSE
)


plot <- ggraph(graph, layout = layout) +

  # --- Edges ---
  geom_edge_link(
    aes(
      width = shared_cips,
      colour = edge_type,
      alpha = edge_type
    )
  ) +
  scale_edge_colour_manual(
    values = c("field" = "green", "specific" = "blue"),
    labels = c(
      "field" = "Shared field of study (CIP-4)",
      "specific" = "Shared instructional \n program (CIP-6)"
    ),
    name = ""
  ) +
  scale_edge_alpha_manual(
    values = c("field" = .5, "specific" = .7), guide = "none"
  ) +
  scale_edge_width(range = c(0.5, 2), guide = "none") +

  # --- Nodes ---
  geom_node_point(size = 4) +
  # scale_color_manual(
  #   values = c("TRUE" = "orange", "FALSE" = "steelblue"),
  #   guide = "none"
  # ) +
  geom_node_text(
    aes(label = soc_label),
    repel = TRUE,
    size = 2.8
  ) +

  # --- Styling ---
  theme_void() +
  labs(
    title = "Network of Shared Instructional Programs",
    subtitle = "Distances reflect skill overlap with center occupation\nLinewidth reflect number of programs in common btwn linked occupations",
    caption = "Source: JobSTAT, CIP-SOC Crosswalk"
  ) +
  guides(edge_colour = guide_legend(override.aes = list(edge_width = 2, edge_alpha = 1))) +
  theme(legend.position = "bottom")

plot

ggsave(file.path(output_path, "fig7_cip_network.png"), width = 8, height = 6, dpi = 300)

saveRDS(plot, file.path(output_path, "fig7.rds"))

### --- Set focus occupation, make cert dfs & figure ----
focus <- "499041" # Industrial machinary mechanics

focus_cips <- cip_6digit %>%
  filter(soc_code == focus)

nodes_df <- jobstat %>%
  filter(code_x == focus) %>%
  distinct(code_x, code_y, shine, soctitle_x, soctitle_y) %>%
  arrange(desc(shine)) %>%
  head(16)

nodes_certs <- cert_details %>%
  filter(soccode %in% nodes_df$code_y)

cert_df <- nodes_certs %>%
  left_join(nodes_certs %>% select(CERT_ID, soccode),
    by = "CERT_ID", suffix = c("_from", "_to")
  ) %>%
  filter(soccode_from != soccode_to) %>%
  mutate(
    pair_min = as.character(pmin(soccode_from, soccode_to)),
    pair_max = as.character(pmax(soccode_from, soccode_to))
  ) %>%
  distinct(pair_max, pair_min, CERT_ID, .keep_all = TRUE) %>%
  group_by(pair_min, pair_max) %>%
  summarise(shared_cert = n_distinct(CERT_ID))


nodes_clean <- tibble(soc_code = unique(c(cert_df$pair_min, cert_df$pair_max))) %>%
  full_join(nodes_df %>% select(code_y, soctitle_y, shine),
    by = c("soc_code" = "code_y")
  )

# rename columns to expected format
nodes_ready <- nodes_clean %>%
  rename(name = soc_code, soc_title = soctitle_y) %>%
  mutate(soc_label = str_wrap(soc_title, width = 20))

cert_ready <- cert_df %>%
  rename(from = pair_min, to = pair_max)

# layout, position but shine score
layout <- nodes_ready %>%
  arrange(desc(shine)) %>%
  mutate(
    radius = (100 - shine) * .5, # smaller shine -> farther
    angle = seq(0, 2 * pi, length.out = n()), # spread around circle
    x = radius * cos(angle),
    y = radius * sin(angle)
  )

# jitter layout
set.seed(123)
layout <- layout %>%
  mutate(
    angle = angle + runif(n(), -0.15, 0.15)
  ) %>%
  mutate(
    x = ifelse(name == focus, 0, x),
    y = ifelse(name == focus, 0, y)
  )

# build graph
graph <- tbl_graph(
  nodes = layout, # contains x/y already
  edges = cert_ready,
  directed = FALSE
)


plot <- ggraph(graph, layout = layout) +

  # --- Edges ---
  geom_edge_link(
    aes(
      edge_width = log(shared_cert + 1)
    ),
    edge_colour = "purple", edge_alpha = .5
  ) +
  scale_edge_width(range = c(0.5, 2), guide = "none") +

  # --- Nodes ---
  geom_node_point(size = 4) +
  # scale_color_manual(
  #   values = c("TRUE" = "orange", "FALSE" = "steelblue"),
  #   guide = "none"
  # ) +
  geom_node_text(
    aes(label = soc_label),
    repel = TRUE,
    size = 2.8
  ) +

  # --- Styling ---
  theme_void() +
  labs(
    title = "Network of Shared Certificates",
    subtitle = "Distances reflect skill overlap with center occupation\nLinewidth reflect number of certificates in common btwn linked occupations",
    caption = "Source: JobSTAT, CareerOneStop"
  )

plot

ggsave(file.path(output_path, "fig8_cert_network.png"), width = 8, height = 6, dpi = 300)

saveRDS(plot, file.path(output_path, "fig8.rds"))


# ---- Graduate/Program outcomes  ----
geodata_underBA <- read_csv(file.path(data_path, "GEOAnnualWageandEmployment_underBA.csv")) %>%
  clean_names()

geodata_overBA <- read_csv(file.path(data_path, "GEOAnnualWageandEmployment_overBA.csv")) %>%
  clean_names()

cip_soc <- read_xlsx(file.path(gen_data_path, "CIP2020_SOC2018_Crosswalk.xlsx"), sheet = "CIP-SOC") %>%
  mutate(
    soc_code = str_remove(SOC2018Code, "-"),
    cip_2 = substr(CIP2020Code, 1, 2),
    cip_4 = substr(CIP2020Code, 1, 5)
  ) %>%
  rename(soc_title = SOC2018Title) %>%
  distinct(soc_code, soc_title, cip_2, cip_4)

occ_geodata_underBA <- geodata_underBA %>%
  left_join(cip_soc %>% select(-cip_2), by = c("cip" = "cip_4")) %>%
  left_join(cip_soc %>% select(-cip_4),
    by = c("cip" = "cip_2"),
    suffix = c("_4", "_2")
  ) %>%
  mutate(
    soc_code = coalesce(soc_code_4, soc_code_2),
    soc_title = coalesce(soc_title_4, soc_title_2)
  ) %>%
  select(-matches("(_4|_2)$")) %>%
  select(-matches("(_3rd_year|_4th_year)$"))

occ_geodata_overBA <- geodata_overBA %>%
  left_join(cip_soc %>% select(-cip_2), by = c("cip" = "cip_4")) %>%
  left_join(cip_soc %>% select(-cip_4),
    by = c("cip" = "cip_2"),
    suffix = c("_4", "_2")
  ) %>%
  mutate(
    soc_code = coalesce(soc_code_4, soc_code_2),
    soc_title = coalesce(soc_title_4, soc_title_2)
  ) %>%
  select(-matches("(_4|_2)$")) %>%
  select(-matches("(_3rd_year|_4th_year)$"))



edu_programs_priorityoccs_underBA <- occ_geodata_underBA %>%
  filter(soc_code %in% priority_underBA$`SOC Code`) %>%
  select(soc_code, soc_title, cip, instructional_program) %>%
  distinct() %>%
  left_join(
    occ_geodata_underBA %>% filter(soc_code %in% all_occ_underBA$soc_code) %>%
      select(cip, soc_code, soc_title),
    by = "cip", suffix = c("", "_related")
  ) %>%
  filter(soc_code != soc_code_related) %>%
  group_by(instructional_program) %>%
  summarize(
    list_priorityocc = paste(sort(unique(soc_title)), collapse = "\n"),
    list_related_priorityocc = paste(sort(unique(soc_title_related)), collapse = "\n"),
    .groups = "drop"
  )

write_csv(edu_programs_priorityoccs_underBA, file.path(output_path, "eduprogram_list_occs_underBA.csv"))

underBA_program_outcomes <- occ_geodata_underBA %>%
  filter(soc_code %in% priority_underBA$`SOC Code`) %>%
  filter(program_level == "4-digit program detail") %>%
  select(
    graduation_year, school_location, instructional_program, award, graduates, graduates_employed_in_mn_during_the_2nd_year,
    pct_working_full_time_and_year_round_in_2nd_year, full_time_year_round_median_wage_in_2nd_year,
    pct_working_part_time_and_or_seasonally_in_2nd_year,
    annual_median_earnings_of_all_employed_graduates_during_the_2nd_year_regardless_of_hours_and_continuity_of_employment,
    soc_title
  ) %>%
  arrange(instructional_program, award) %>%
  select(-soc_title) %>%
  distinct() %>%
  mutate(across(
    c(
      graduates_employed_in_mn_during_the_2nd_year,
      pct_working_full_time_and_year_round_in_2nd_year,
      full_time_year_round_median_wage_in_2nd_year,
      pct_working_part_time_and_or_seasonally_in_2nd_year,
      annual_median_earnings_of_all_employed_graduates_during_the_2nd_year_regardless_of_hours_and_continuity_of_employment
    ),
    ~ str_replace_all(., "[\\$,%,,*]", "") |> # remove $, %, commas, asterisks
      str_replace_all(",", "") |> # just in case extra commas remain
      na_if("N/A") |> # replace N/A or N/A* with NA
      na_if("NA") |>
      as.numeric()
  )) %>%
  group_by(award) %>%
  summarise(
    grads = sum(graduates), grads_employed_2yrsout = sum(graduates_employed_in_mn_during_the_2nd_year, na.rm = TRUE),
    pct_ft = weighted.mean(pct_working_full_time_and_year_round_in_2nd_year, w = graduates_employed_in_mn_during_the_2nd_year, na.rm = TRUE),
    med_wage_FT = weighted.mean(full_time_year_round_median_wage_in_2nd_year, w = graduates_employed_in_mn_during_the_2nd_year * pct_working_full_time_and_year_round_in_2nd_year, na.rm = TRUE),
    pct_PTSsn = weighted.mean(pct_working_part_time_and_or_seasonally_in_2nd_year, w = graduates_employed_in_mn_during_the_2nd_year, na.rm = TRUE),
    med_awage = weighted.mean(annual_median_earnings_of_all_employed_graduates_during_the_2nd_year_regardless_of_hours_and_continuity_of_employment,
      w = graduates_employed_in_mn_during_the_2nd_year, na.rm = TRUE
    )
  )


write_csv(underBA_program_outcomes, file.path(output_path, "underBA_program_outcomes.csv"))


edu_programs_priorityoccs_overBA <- occ_geodata_overBA %>%
  filter(soc_code %in% priority_overBA$`SOC Code`) %>%
  select(soc_code, soc_title, cip, instructional_program) %>%
  distinct() %>%
  left_join(
    occ_geodata_underBA %>% filter(soc_code %in% all_occ_overBA$soc_code) %>%
      select(cip, soc_code, soc_title),
    by = "cip", suffix = c("", "_related")
  ) %>%
  filter(soc_code != soc_code_related) %>%
  group_by(instructional_program) %>%
  summarize(
    list_priorityocc = paste(sort(unique(soc_title)), collapse = "\n"),
    list_related_priorityocc = paste(sort(unique(soc_title_related)), collapse = "\n"),
    .groups = "drop"
  )

overBA_program_outcomes <- occ_geodata_overBA %>%
  filter(soc_code %in% priority_overBA$`SOC Code`) %>%
  filter(program_level == "4-digit program detail") %>%
  select(
    graduation_year, school_location, instructional_program, award, graduates, graduates_employed_in_mn_during_the_2nd_year,
    pct_working_full_time_and_year_round_in_2nd_year, full_time_year_round_median_wage_in_2nd_year,
    pct_working_part_time_and_or_seasonally_in_2nd_year,
    annual_median_earnings_of_all_employed_graduates_during_the_2nd_year_regardless_of_hours_and_continuity_of_employment
  ) %>%
  arrange(instructional_program, award) %>%
  distinct() %>%
  mutate(across(
    c(
      graduates_employed_in_mn_during_the_2nd_year,
      pct_working_full_time_and_year_round_in_2nd_year,
      full_time_year_round_median_wage_in_2nd_year,
      pct_working_part_time_and_or_seasonally_in_2nd_year,
      annual_median_earnings_of_all_employed_graduates_during_the_2nd_year_regardless_of_hours_and_continuity_of_employment
    ),
    ~ str_replace_all(., "[\\$,%,,*]", "") |> # remove $, %, commas, asterisks
      str_replace_all(",", "") |> # just in case extra commas remain
      na_if("N/A") |> # replace N/A or N/A* with NA
      na_if("NA") |>
      as.numeric()
  )) %>%
  group_by(award) %>%
  summarise(
    grads = sum(graduates), grads_employed_2yrsout = sum(graduates_employed_in_mn_during_the_2nd_year, na.rm = TRUE),
    pct_ft = weighted.mean(pct_working_full_time_and_year_round_in_2nd_year, w = graduates_employed_in_mn_during_the_2nd_year, na.rm = TRUE),
    med_wage_FT = weighted.mean(full_time_year_round_median_wage_in_2nd_year, w = graduates_employed_in_mn_during_the_2nd_year * pct_working_full_time_and_year_round_in_2nd_year, na.rm = TRUE),
    pct_PTSsn = weighted.mean(pct_working_part_time_and_or_seasonally_in_2nd_year, w = graduates_employed_in_mn_during_the_2nd_year, na.rm = TRUE),
    med_awage = weighted.mean(annual_median_earnings_of_all_employed_graduates_during_the_2nd_year_regardless_of_hours_and_continuity_of_employment,
      w = graduates_employed_in_mn_during_the_2nd_year, na.rm = TRUE
    )
  )

write_csv(overBA_program_outcomes, file.path(output_path, "overBA_program_outcomes.csv"))


## ---- # of training programs in Metro, map? & cert reqs, apprenticeship acknowledgement ----


## ---- Training program outcomes  (2024 vacancies for reference?) ----
