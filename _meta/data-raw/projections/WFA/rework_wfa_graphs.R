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
source("R/_load_pkgs.R")
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

## ---- Pie charts industry and employment count ----


### --- Employment data (qcew) ----


# Load QCEW 2017 - 2024


### ---- Piecharts ----

metro_sector_ind <- metro_scope %>%
  group_by(naics_2_label) %>%
  summarise(
    naics_2_n = n(),
    .groups = "drop"
  ) %>%
  mutate(naics_2_share = naics_2_n / sum(naics_2_n))

metro_ind_total <- metro_scope %>%
  count()

metro_sector_emp <- metro_scope %>%
  group_by(naics_2_label) %>%
  summarise(
    naics_2_employment = sum(employment),
    .groups = "drop"
  ) %>%
  mutate(naics_2_share = naics_2_employment / sum(naics_2_employment))

metro_emp_total <- metro_scope %>%
  summarise(total_emp = sum(employment))


# Metro build donut
p_ind <- ggplot(metro_sector_ind, aes(x = 2, y = naics_2_share, fill = naics_2_label)) +
  geom_col(width = 1, color = "white") +
  geom_text(
    data = metro_ind_total,
    aes(x = 0.5, y = 0, label = n), # center
    inherit.aes = FALSE,
    size = 4, fontface = "bold"
  ) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(
    name = "Industry Sector",
    values = unname(palette36.colors((n_distinct(metro_sector_ind$naics_2_label))))
  ) +
  theme_void() +
  labs(
    title = "Scoped Industries",
    subtitle = "Number in center = total NAICS codes"
  )

p_emp <- ggplot(metro_sector_emp, aes(x = 2, y = naics_2_share, fill = naics_2_label)) +
  geom_col(width = 1, color = "white") +
  geom_text(
    data = metro_emp_total,
    aes(x = 0.5, y = 0, label = comma(total_emp)), # center
    inherit.aes = FALSE,
    size = 4, fontface = "bold"
  ) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(
    name = "Industry Sector",
    values = unname(palette36.colors((n_distinct(metro_sector_emp$naics_2_label))))
  ) +
  theme_void() +
  labs(
    title = "Scoped Employment",
    subtitle = "Number in center = total employment"
  )


final_plot <- p_ind + p_emp +
  plot_layout(guides = "collect") &
  plot_annotation(
    caption = "Data source: QCEW"
  ) &
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5)
  )

final_plot

ggsave(file.path(output_path, "fig1_metro_scope_ind_emp_donuts.png"), width = 6, height = 5.5, dpi = 300)

saveRDS(final_plot, file.path(output_path, "fig1.rds"))


## ---- Industry employment time trends ----
### ---- Adding projections and binding with qcew ----
industry_occ_links <- read_xlsx(file.path(gen_data_path, "iowage_LEWIS_NAICS4.xlsx"), sheet = 1)

safe_extract_naics <- function(x) {
  res <- str_extract_all(x, "\\d{4}")
  if (length(res) == 0 || length(res[[1]]) == 0) {
    return(NA_character_)
  } else {
    return(res[[1]])
  }
}

naics_proj_crosswalk <- industry_occ_links %>%
  distinct(indcode, IndTitle) %>%
  rowwise() %>%
  mutate(
    naics_4 = case_when(
      is.na(indcode) ~ list(NA_character_),
      str_detect(indcode, "A") ~ list(safe_extract_naics(IndTitle)),
      TRUE ~ list(indcode)
    )
  ) %>%
  unnest(naics_4) %>%
  ungroup()

qcew_2024_flagged <- qcew_scoped %>%
  filter(year == 2024) %>%
  mutate(naics_4 = str_sub(naics, 1, 4))

# checking on rolled up projection industries
qcew_2024_scoped_projections <- qcew_2024_flagged %>%
  left_join(naics_proj_crosswalk, by = "naics_4") %>%
  mutate(
    in_proj = !is.na(indcode),
    proj_rollup = case_when(
      naics_4 != indcode ~ "rolled",
      naics_4 == indcode ~ "not rolled",
      in_proj == FALSE ~ "not in proj",
      TRUE ~ NA_character_
    )
  )


qcew_scoped_4 <- qcew_scoped %>%
  mutate(naics_4 = str_sub(naics, 1, 4)) %>%
  group_by(year, naics_4, scope) %>%
  summarise(
    employment = sum(employment, na.rm = TRUE),
    .groups = "drop"
  )

qcew_2022_4digit <- read_csv(file.path(data_path, "QCEWResults_17-24_7coMetro_4dig.csv")) %>%
  clean_names() %>%
  filter(ownership == "00" & indlevel == 4) %>%
  select(year = periodyear, naics_4 = indcode, emp_naics_4 = emp_year) %>%
  filter(year >= 2021) %>%
  pivot_wider(
    names_from = year,
    values_from = emp_naics_4,
    names_prefix = "emp_"
  ) %>%
  mutate(emp_2022_est = coalesce(emp_2022, emp_2021, emp_2023, emp_2024)) %>%
  select(naics_4, emp_naics_4 = emp_2022_est) # replaced missing 2022's with first non-missing 21, 23, 24 emp


# Scope & 4digit for all 6digits with employment over 2017-2024 period

ind_projections <- read_csv(file.path(data_path, "LongTermIndustriesProj_7coMetro.csv")) %>%
  clean_names() %>%
  mutate(across(
    where(is.character),
    ~ iconv(.x, from = "UTF-8", to = "UTF-8", sub = "")
  )) %>%
  select(projected_year, title, naics_code, emp_pctchange = percent_change) %>%
  mutate(
    across(c(naics_code, title), as.character),
    across(where(is.character), str_squish),
    ind_proj_code_lvl = nchar(naics_code)
  ) %>%
  left_join(naics_proj_crosswalk %>% rename(proj_ind_title = IndTitle),
    by = c("naics_code" = "indcode")
  ) %>%
  filter(!is.na(naics_4) & ind_proj_code_lvl == 4)

ind_emp_shares <- qcew_scoped %>%
  filter(year == 2022) %>%
  mutate(naics_4 = str_sub(naics, 1, 4)) %>%
  left_join(qcew_2022_4digit %>% select(naics_4, emp_naics_4), by = "naics_4") %>%
  filter(!is.na(emp_naics_4)) %>%
  mutate(emp_share_4digit = employment / emp_naics_4) %>%
  group_by(naics_4, scope) %>%
  summarise(
    scoped_emp_share_in2022 = sum(emp_share_4digit, na.rm = TRUE),
    scoped_emp_in2022 = sum(employment, na.rm = TRUE),
    .groups = "drop"
  )

proj_full_scope_4_adj <- ind_projections %>%
  left_join(ind_emp_shares %>%
    select(naics_4, scoped_emp_in2022, scope), by = "naics_4") %>%
  select(-ind_proj_code_lvl) %>%
  rename(
    naics_title_4 = title,
    year = projected_year
  ) %>%
  mutate(
    employment = scoped_emp_in2022 * (1 + (emp_pctchange / 100))
  ) %>%
  filter(!is.na(employment)) %>%
  select(year, naics_4, naics_title_4, employment, emp_pctchange, scope)
# note 5 industries show 0 as the employment because they are 0s
# in the 2022 qcew BUT were in projections, eg logging...
# doesn't seem right for metro projections... logging in 7coMetro?
# leaving as is for now - trusting QCEW

title_lookup <- ind_projections %>%
  distinct(naics_code, title) %>%
  rename(naics_4 = naics_code, naics_title_4 = title)

ind_emp_withproj <- qcew_scoped_4 %>%
  left_join(title_lookup, by = "naics_4") %>%
  bind_rows(proj_full_scope_4_adj) %>%
  arrange(naics_4, year) %>%
  filter(!is.na(naics_title_4))

#### ---- timetrends figure ----

ind_emp <- ind_emp_withproj %>%
  group_by(year, scope) %>%
  summarise(employment = sum(employment))

plot <- ggplot(ind_emp, aes(x = year, y = employment, color = scope)) +
  geom_point() +
  geom_line(data = subset(ind_emp, year <= 2024), size = 1) +
  geom_line(data = subset(ind_emp, year >= 2024), size = 1, linetype = "dashed") +
  facet_wrap(~scope, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(
    title = paste0("Employment by In & Out of Scope Industries"),
    caption = "Source: QCEW + Projections",
    subtitle = "Historical and projected employment",
    x = "Year",
    y = "Employment)",
    color = ""
  ) +
  theme_minimal(base_size = 11) +
  guides(
    color = "none"
  ) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  )

plot

ggsave(file.path(output_path, "fig2_metro_scope_timetrend.png"), width = 8, height = 4, dpi = 300)

saveRDS(plot, file.path(output_path, "fig2.rds"))

ind_emp_naics2_inscope <- ind_emp_withproj %>%
  mutate(
    naics_2 = substr(naics_4, 1, 2),
    naics_2_label = case_when(
      naics_2 == "11" ~ "Ag, Forestry,\nFishing",
      naics_2 == "21" ~ "Mining &\nExtraction",
      naics_2 == "22" ~ "Utilities",
      naics_2 == "23" ~ "Construction",
      naics_2 %in% c("31", "32", "33") ~ "Manufacturing",
      naics_2 %in% c("42", "44", "45") ~ "Wholesale &\nRetail Trade",
      naics_2 %in% c("48", "49") ~ "Transport &\nWarehousing",
      naics_2 == "51" ~ "Information",
      naics_2 == "52" ~ "Finance &\nInsurance",
      naics_2 == "53" ~ "Real Estate &\nLeasing",
      naics_2 == "54" ~ "Professional\nServices",
      naics_2 == "55" ~ "Company\nManagement",
      naics_2 == "56" ~ "Admin &\nWaste Svcs",
      naics_2 == "61" ~ "Education",
      naics_2 == "62" ~ "Health &\nSocial Svcs",
      naics_2 == "71" ~ "Arts &\nRecreation",
      naics_2 == "72" ~ "Accommodation\n& Food",
      naics_2 == "81" ~ "Other\nServices",
      naics_2 == "92" ~ "Public Admin",
      TRUE ~ "Other / Unknown"
    )
  ) %>%
  group_by(year, scope, naics_2_label) %>%
  summarise(
    employment = sum(employment),
    .groups = "drop"
  ) %>%
  filter(scope == "in scope")

plot <- ggplot(
  ind_emp_naics2_inscope,
  aes(x = year, y = employment, color = naics_2_label)
) +
  geom_point() +
  geom_line(data = subset(ind_emp_naics2_inscope, year <= 2024), size = 1) +
  geom_line(data = subset(ind_emp_naics2_inscope, year >= 2024), size = 1, linetype = "dashed") +
  scale_color_manual(values = unname(palette36.colors(n_distinct(ind_emp_naics2_inscope$naics_2_label)))) +
  scale_y_continuous(labels = comma) +
  labs(
    title = paste0("Employment by In Scope by Industry Sector"),
    caption = "Source: QCEW + Projections",
    subtitle = "Historical and projected employment",
    x = "Year",
    y = "Employment",
    color = ""
  ) +
  theme_minimal(base_size = 11) +
  guides(
    color = guide_legend(nrow = 2)
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  )

plot

ggsave(file.path(output_path, "fig3_metro_scope_timetrend_naics2.png"), width = 5, height = 5, dpi = 300)

saveRDS(plot, file.path(output_path, "fig3.rds"))

### ---- Current employment stats ----

table(qcew_2024_scoped_projections$year)

emp_by_scope <- qcew_2024_scoped_projections %>%
  group_by(scope) %>%
  summarise(
    emp = sum(employment),
    .groups = "drop"
  ) %>%
  mutate(
    total_6digit = sum(emp),
    total_metro = metro_total_emp,
    metro_share = emp / metro_total_emp
  )

emp_by_scope


### ---- BAU Employment growth ----

growth_24to32 <- ind_emp_withproj %>%
  filter(year %in% c(2024, 2032)) %>%
  pivot_wider(names_from = year, values_from = employment, names_prefix = "emp_") %>%
  filter(!is.na(emp_2024) & !is.na(emp_2032)) %>%
  group_by(scope) %>%
  summarise(emp_2024 = sum(emp_2024), emp_2032 = sum(emp_2032)) %>%
  mutate(
    total_growth = emp_2032 - emp_2024,
    pct_growth   = (emp_2032 - emp_2024) / emp_2024 * 100
  )

growth_24to32

### ---- Industry details
ind_growth_24to32 <- ind_emp_withproj %>%
  filter(year %in% c(2024, 2032)) %>%
  pivot_wider(names_from = year, values_from = employment, names_prefix = "emp_") %>%
  filter(!is.na(emp_2024) & !is.na(emp_2032)) %>%
  group_by(scope, naics_4, naics_title_4) %>%
  summarise(emp_2024 = sum(emp_2024), emp_2032 = sum(emp_2032)) %>%
  mutate(
    total_growth = emp_2032 - emp_2024,
    pct_growth   = (emp_2032 - emp_2024) / emp_2024 * 100
  )

ind_growth_22to32 <- ind_emp_withproj %>%
  filter(year %in% c(2022, 2032)) %>%
  pivot_wider(names_from = year, values_from = employment, names_prefix = "emp_") %>%
  filter(!is.na(emp_2022) & !is.na(emp_2032)) %>%
  group_by(scope, naics_4, naics_title_4) %>%
  summarise(emp_2022 = sum(emp_2022), emp_2032 = sum(emp_2032)) %>%
  mutate(
    total_growth = emp_2032 - emp_2022,
    pct_growth   = (emp_2032 - emp_2022) / emp_2022 * 100
  )


ind_growth_24to32 %>%
  filter(scope == "in scope") %>%
  arrange(desc(emp_2024)) %>%
  head()

qcew_2024_scoped_projections %>%
  filter(scope == "in scope") %>%
  group_by(naics_4) %>%
  summarise(employment = sum(employment)) %>%
  arrange(desc(employment)) %>%
  head()


ind_growth_24to32 %>%
  filter(scope == "in scope") %>%
  arrange(desc(pct_growth)) %>%
  head(10)

ind_growth_22to32 %>%
  filter(scope == "in scope") %>%
  arrange(desc(pct_growth)) %>%
  head(10)

ind_growth_24to32 %>%
  filter(scope == "in scope") %>%
  arrange(desc(emp_2024)) %>%
  head()

ind_growth_22to32 %>%
  filter(scope == "in scope") %>%
  arrange(desc(emp_2022)) %>%
  head()

ind_growth_24to32 %>%
  filter(scope == "in scope") %>%
  arrange((pct_growth)) %>%
  head(10)

ind_growth_22to32 %>%
  filter(scope == "in scope") %>%
  arrange((pct_growth)) %>%
  head(10)

ind_growth_24to32 %>%
  filter(scope == "in scope") %>%
  arrange((total_growth)) %>%
  head()

ind_growth_22to32 %>%
  filter(scope == "in scope") %>%
  arrange((total_growth)) %>%
  head()

# Naics 2 digit growth rates

largest_2dig <- ind_growth_24to32 %>%
  mutate(
    naics_2 = substr(naics_4, 1, 2),
    naics_2 = ifelse(naics_2 %in% c("31", "32", "33"), "33", naics_2)
  ) %>% # combining manuf for ease of interpreting
  group_by(scope, naics_2) %>%
  summarise(
    emp_2024 = sum(emp_2024), emp_2032 = sum(emp_2032),
    .groups = "drop"
  ) %>%
  group_by(scope) %>%
  mutate(
    total_growth = emp_2032 - emp_2024,
    pct_growth = (emp_2032 - emp_2024) / emp_2024 * 100,
    scope_share_emp2024 = emp_2024 / sum(emp_2024),
    scope_share_emp2032 = emp_2032 / sum(emp_2032)
  )

largest_2dig %>% arrange(scope, desc(emp_2024))

largest_2dig %>% arrange(scope, desc(total_growth))

largest_2dig %>% arrange(scope, desc(pct_growth))


# ---- Priority Occupations ----
### --- Load occupation data ----
# Scope
ccap_soc_scope_final <- read_rds(file.path(scope_path, "ccap_soc_scope_final.rds"))

full_onet2022_scope <- read_xlsx(file.path(gen_data_path, "onet_greentopic_occupations.xlsx"), sheet = 1, skip = 2) %>%
  mutate(soc_code = as.character(soc_code)) %>%
  distinct(soc_code, soc_title)

full_onet2009_scope <- read_csv(file = file.path(gen_data_path, "onet_greening_occupations_soc2018.csv")) %>%
  mutate(soc_code = as.character(soc_code)) %>%
  select(-source)

table(ccap_soc_scope_final$source)

condensed_ccap_soc_scope <- ccap_soc_scope_final %>%
  group_by(soc_code) %>%
  summarize(
    soc_title = soc_title[which.max(nchar(soc_title))], # Keep longest version of the cleaned title
    list_ccap_sectors = paste(sort(unique(ccap_sector)), collapse = ", "),
    .groups = "drop"
  )

all_onet <- full_onet2009_scope %>%
  rbind(full_onet2022_scope) %>%
  distinct(soc_code)

# Scored data from build_occupation_table script
scope_scored_OID <- read_rds(file.path(data_path, "msa_scope_scored_OID.rds")) %>%
  mutate(
    soc_2 = substr(soc_code, 1, 2),
    soc_2_label = case_when(
      soc_2 == "11" ~ "Management",
      soc_2 == "13" ~ "Business &\nFinancial Ops",
      soc_2 == "15" ~ "Computer &\nMath",
      soc_2 == "17" ~ "Architecture &\nEngineering",
      soc_2 == "19" ~ "Life, Physical,\n& Social Science",
      soc_2 == "21" ~ "Community &\nSocial Service",
      soc_2 == "23" ~ "Legal",
      soc_2 == "25" ~ "Education,\nTraining, & Library",
      soc_2 == "27" ~ "Arts, Design,\nEntertainment, Sports,\n& Media",
      soc_2 == "29" ~ "Healthcare\nPractitioners & Tech",
      soc_2 == "31" ~ "Healthcare\nSupport",
      soc_2 == "33" ~ "Protective Service",
      soc_2 == "35" ~ "Food Prep &\nServing",
      soc_2 == "37" ~ "Building &\nGrounds Cleaning,\n& Maintenance",
      soc_2 == "39" ~ "Personal Care\n& Service",
      soc_2 == "41" ~ "Sales &\nRelated",
      soc_2 == "43" ~ "Office &\nAdministrative Support",
      soc_2 == "45" ~ "Farming,\nFishing, & Forestry",
      soc_2 == "47" ~ "Construction\n& Extraction",
      soc_2 == "49" ~ "Installation,\nMaintenance, & Repair",
      soc_2 == "51" ~ "Production",
      soc_2 == "53" ~ "Transportation\n& Material Moving",
      TRUE ~ "Other / Unknown"
    )
  )

# OES MSA Employment
oews_q12025_MSA <- read_xlsx(file.path(data_path, "OESResults_Q12025_MSA.xlsx")) %>%
  clean_names() %>%
  mutate(
    across(c(emp_count, h_mean:h_pct90, a_mean:a_pct90), as.numeric),
    soc_code = as.character(occ_code),
    across(where(is.character), str_squish)
  )

msa_oes_6 <- oews_q12025_MSA %>%
  filter(soc_level == "6")

msa_oes_024 <- oews_q12025_MSA %>%
  filter(soc_level != "6")

# Checking SOC codes missing from OES Twin City MSA
non_oes_msa <- ccap_soc_scope_final %>%
  anti_join(msa_oes_6, by = "soc_code") %>%
  distinct(soc_code, soc_title)

# Fuzzy join non-matched SOCs to group-level OEWS
oes_fallback_matches_msa <- non_oes_msa %>%
  stringdist_left_join(
    msa_oes_024 %>% select(
      soc_code, soc_title_l,
      starts_with("emp"), starts_with("h_"), starts_with("a_")
    ),
    by = c("soc_title" = "soc_title_l"),
    method = "jw",
    max_dist = 0.15
  ) # review best match per row

# Correcting fuzzy match
oes_fallback_best_msa <- oes_fallback_matches_msa %>%
  filter(soc_code.x == soc_code.y) %>%
  select(soc_code = soc_code.x, starts_with("emp"), starts_with("h_"), starts_with("a_"))


# Join   w/ data
stat_vars <- c(
  "emp_count", "h_mean", "h_median", "h_pct10", "h_pct25",
  "h_pct75", "h_pct90", "a_mean", "a_median", "a_pct10",
  "a_pct25", "a_pct75", "a_pct90"
)

msa_oes_6_adj <- msa_oes_6 %>%
  left_join(oes_fallback_best_msa, by = "soc_code", suffix = c("", ".y")) %>%
  mutate(
    across(
      all_of(stat_vars),
      ~ coalesce(.x, get(paste0(cur_column(), ".y")))
    )
  ) %>%
  select(-ends_with(".y"))

# Backfills to consider

soc_4dig <- oes_fallback_matches_msa %>%
  filter(is.na(soc_code.y)) %>% # no fuzzy match so check if should backfill with 4dig stats
  mutate(soc_4 = substr(soc_code.x, 1, 4)) %>%
  distinct(soc_4)

for (soc4 in soc_4dig$soc_4) {
  cat("\n\n========== SOC 4-digit group:", soc4, "==========\n")

  cat("\n-- OES 6-digit matches (area_oes_6_adj):\n")
  print(
    msa_oes_6_adj %>%
      filter(str_starts(soc_code, soc4)) %>%
      distinct(soc_code, emp_count)
  )

  cat("\n-- OES aggregated (area_oes_024):\n")
  print(
    msa_oes_024 %>%
      filter(str_starts(soc_code, soc4))
  )
}


oes_patch_msa <- msa_oes_024 %>%
  filter(soc_code %in% c("474090")) %>%
  mutate(soc_code6 = case_when(
    soc_code == "474090" ~ "474099"
  ))

# Finalize by adding patches

msa_oes_6_adj <- msa_oes_6_adj %>%
  left_join(oes_patch_msa,
    by = c("soc_code" = "soc_code6"),
    suffix = c("", "_patch")
  ) %>%
  mutate(
    across(
      all_of(stat_vars),
      ~ coalesce(.x, get(paste0(cur_column(), "_patch")))
    ),
    soc_code = if_else(is.na(soc_code_patch), soc_code, soc_code_patch)
  ) %>%
  select(-ends_with("_patch")) %>%
  filter(!is.na(soc_code))

oes_6_for_loop_msa <- msa_oes_6_adj %>%
  select(
    soc_code,
    soc_title = soc_title_l,
    all_of(stat_vars) # emp_count:a_pct90
  )


# Occ descriptives
role_summary_df <- read_xlsx(file.path(gen_data_path, "soc_2018_definitions.xlsx")) %>%
  clean_names() %>%
  select(-soc_code) %>%
  rename(soc_code = soc_code_num) %>%
  mutate(
    across(everything(), as.character),
    across(everything(), str_squish)
  )

soc_major_groups <- role_summary_df %>%
  filter(soc_group == "Major") %>%
  select(soc_2dig = soc_code, soc_major_title = soc_title) %>%
  mutate(
    soc_2dig = str_sub(soc_2dig, 1, 2),
    soc_major_title = str_remove(soc_major_title, " Occupations$")
  )

soc_detailed_roles <- role_summary_df %>%
  filter(soc_group == "Detailed") %>%
  select(soc_code, description = soc_definition) # rename for clarity

ex_jobtitles <- read_xlsx(file.path(gen_data_path, "soc_example_jobtitles.xlsx")) %>%
  clean_names() %>%
  mutate(
    across(everything(), as.character),
    across(everything(), str_squish)
  ) %>%
  mutate(
    soc_code = str_replace_all(o_net_soc_code, c("-" = "", "\\..*" = "")),
    non_soc = ifelse(str_detect(o_net_soc_code, "\\.00$"), 0, 1)
  )

## ---- Pie chart occupation and employment count ----
metro_onet <- scope_scored_OID %>%
  filter(!is.na(onet)) %>%
  select(soc_code, soc_title, soc_2, soc_2_label, emp_count)

metro_soc_ccap <- scope_scored_OID %>%
  select(soc_code, soc_title, soc_2, soc_2_label, emp_count)

metro_occ_list <- list(
  ONET = metro_onet,
  All = metro_soc_ccap
)

all_metro_occ <- bind_rows(metro_occ_list, .id = "scope")

# total obs per scope (for center text later)
met_totals <- all_metro_occ %>%
  group_by(scope) %>%
  summarise(total = n())

met_sector_totals <- all_metro_occ %>%
  group_by(scope, soc_2_label) %>%
  summarise(
    soc_2_n = n(),
    .groups = "drop"
  ) %>%
  group_by(scope) %>%
  mutate(soc_2_share = soc_2_n / sum(soc_2_n))

met_emp <- all_metro_occ %>%
  group_by(scope) %>%
  summarise(total = sum(emp_count))

met_sector_emp <- all_metro_occ %>%
  group_by(scope, soc_2_label) %>%
  summarise(
    soc_2_emp = sum(emp_count),
    .groups = "drop"
  ) %>%
  group_by(scope) %>%
  mutate(soc_2_share = soc_2_emp / sum(soc_2_emp))

# build donut
p_soc <- ggplot(
  met_sector_totals,
  aes(x = 2, y = soc_2_share, fill = soc_2_label)
) +
  geom_col(width = 1, color = "white") +
  geom_text(
    data = met_totals,
    aes(x = 0.5, y = 0, label = total), # center
    inherit.aes = FALSE,
    size = 4, fontface = "bold"
  ) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = unname(palette36.colors(n_distinct(met_sector_totals$soc_2_label)))) +
  facet_wrap(~scope) + # multiple donuts
  theme_void() +
  labs(
    fill = "Occupation Group", title = "Scoped Occupations",
    subtitle = "Number in center = total SOC codes"
  ) +
  guides(fill = guide_legend(ncol = 2))

p_emp <- ggplot(
  met_sector_emp,
  aes(x = 2, y = soc_2_share, fill = soc_2_label)
) +
  geom_col(width = 1, color = "white") +
  geom_text(
    data = met_emp,
    aes(x = 0.5, y = 0, label = comma(total)), # center
    inherit.aes = FALSE,
    size = 3, fontface = "bold"
  ) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = unname(palette36.colors(n_distinct(met_sector_emp$soc_2_label)))) +
  facet_wrap(~scope) + # multiple donuts
  theme_void() +
  labs(
    fill = "Occupation Group", title = "Occupation Employment",
    subtitle = "Number in center = total employment"
  ) +
  guides(fill = guide_legend(ncol = 2))

final_plot <- p_soc / p_emp +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5)
  )

final_plot

ggsave(file.path(output_path, "fig4_met_occ_donuts.png"), width = 9, height = 8, dpi = 300)

saveRDS(final_plot, file.path(output_path, "fig4.rds"))

# share of occupation in scoped industries (statewide 2023)
scoped_indXocc <- industry_occ_links %>%
  select(indcode, IndTitle, occcode, empcount) %>%
  distinct() %>%
  left_join(naics_proj_crosswalk, by = "indcode") %>%
  left_join(ind_emp_shares, by = "naics_4") %>%
  mutate(occcode = as.character(occcode)) %>%
  filter(occcode %in% metro_soc_ccap$soc_code) %>%
  mutate(occXindust_emp_adj = empcount * scoped_emp_share_in2022) %>%
  select(-naics_4) %>%
  distinct() %>%
  group_by(occcode, scope) %>%
  summarize(
    empcount = sum(empcount),
    .groups = "drop"
  ) %>%
  group_by(occcode) %>%
  mutate(
    occ_emp = sum(empcount),
    scope_share = empcount / occ_emp
  ) %>%
  ungroup() %>%
  left_join(ccap_soc_scope_final %>% select(soc_code, soc_title),
    by = c("occcode" = "soc_code")
  )

tapply(scoped_indXocc$scope_share, scoped_indXocc$scope, summary)

# some occupations are fully employed in scoped industries, several construction occ.
# by avg occ employment in scoped industries is ~1/3rd with is in line with the ind
# emp total / occ emp total
# But the follow occupation data is all occ employment regardless of industries
# bc jobs/skills are important for the work (eg. electricians working in schools
# or hospitals even though schools and hospitals are in industry scope)
# also helpful to see range of jobs because people move in & out of clean/green work
# even in industries so this helps to show people with specific skills regardless
# of what industry they work in  -  but do restrict to onet occ for stronger connection & tractability


## ---- Priority method scatter ----
onet_scored_OID <- scope_scored_OID %>%
  filter(!is.na(onet)) %>%
  mutate(demand_stars_fct = ifelse(is.na(demand_stars), "No stars", demand_stars)) %>%
  mutate(demand_stars_fct = factor(demand_stars,
    levels = c("1", "2", "3", "4", "5", "No stars")
  )) %>%
  mutate(num_of_sectors = str_count(list_ccap_sectors, ",") + 1)

p90 <- quantile(onet_scored_OID$priority_score, 0.9, na.rm = TRUE)

plot <- ggplot(onet_scored_OID, aes(
  x = jvr_formula,
  y = projected_growth_rate,
  size = emp_count,
  color = demand_stars_fct
)) +
  # 1. Top 10% outline layer (drawn first so it sits behind)
  geom_point(
    data = onet_scored_OID %>% filter(priority_score > p90),
    aes(size = emp_count),
    shape = 21, fill = NA, colour = "red", stroke = 2,
    show.legend = FALSE
  ) +
  # 2. All points, filled by demand stars
  geom_point(alpha = 0.7) +
  scale_x_continuous(
    limits = c(0, 15),
    name = "Job Vacancy Rate"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = .1),
    name = "Projected Growth Rate (2022–2032)"
  ) +
  scale_size_continuous(
    labels = comma_format(),
    name = "Employment (Metro)"
  ) +
  # scale_color_manual(
  #   values = c(setNames(viridis(5, option = "C", direction = -1), 1:5),
  #              "No stars" = "grey60"),
  #   name = "Demand Stars"
  # ) +
  labs(
    title = "Occupational Demand Signals (for 7 county Metro data)",
    subtitle = "Occupations with top 10% of priority scores outlined in <span style='color:red;'>red</span>"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.subtitle = element_markdown()
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5)) # bigger color dots
  )

plot

ggsave(file.path(output_path, "fig5_onet_OID_priority_scatter.png"), width = 8, height = 5, dpi = 300)

saveRDS(plot, file.path(output_path, "fig5.rds"))

### demand signals rework

demand_plot <- ggplot(onet_scored_OID, aes(
  x = jvr_formula,
  y = projected_growth_rate,
  size = emp_count,
  color = demand_stars_fct
)) +
  # 1. Top 10% outline layer (drawn first so it sits behind)
  geom_point(
    data = onet_scored_OID %>% filter(priority_score > p90),
    aes(size = emp_count),
    shape = 21, fill = NA, colour = "red", stroke = 2,
    show.legend = FALSE
  ) +
  # 2. All points, filled by demand stars
  geom_point(alpha = 0.7) +
  scale_x_continuous(
    limits = c(0, 15),
    name = "Job Vacancy Rate"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = .1),
    name = "Projected Growth Rate (2022–2032)"
  ) +
  scale_size_continuous(
    labels = comma_format(),
    name = "Employment (Metro)"
  ) +
  # scale_color_manual(
  #   values = c(setNames(viridis(5, option = "C", direction = -1), 1:5),
  #              "No stars" = "grey60"),
  #   name = "Demand Stars"
  # ) +
  labs(
    title = "Occupational Demand Signals (for 7 county Metro data)",
    subtitle = "Occupations with top 10% of priority scores outlined in <span style='color:red;'>red</span>"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.subtitle = element_markdown()
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5)) # bigger color dots
  )

plot

## ---- Priority occupation tables (10 < BA, 5>=BA) ----
prop.table(table(onet_scored_OID$num_of_sectors))

# 4+ stars, multiple sectors, and split education group
onet_scored_OID_occdetails <- onet_scored_OID %>%
  left_join(
    oes_6_for_loop_msa %>%
      select(soc_code, emp_count),
    by = "soc_code", suffix = c("", "_msa")
  ) %>%
  left_join(soc_detailed_roles, by = "soc_code") %>%
  left_join(
    ex_jobtitles %>% filter(non_soc == 0) %>%
      group_by(soc_code) %>%
      summarize(
        list_ex_titles = paste(sort(unique(reported_job_title)),
          collapse = ", "
        ),
        .groups = "drop"
      ),
    by = "soc_code"
  ) %>%
  select(
    soc_code,
    soc_title,
    description,
    emp_count_msa,
    projected_growth_rate,
    job_vacancy_rate,
    priority_score,
    demand_stars,
    education_requirements,
    list_ccap_sectors,
    num_of_sectors,
    list_ex_titles
  ) %>%
  mutate(list_ccap_sectors = str_replace_all(list_ccap_sectors, "NWL", "Ag & Natural Systems"))



priority_underBA <- onet_scored_OID_occdetails %>%
  filter(num_of_sectors > 1 & demand_stars >= 4 &
    !(education_requirements == "Bachelor's degree" |
      education_requirements == "Doctoral or professional degree" |
      education_requirements == "Graduate or professional degree")) %>%
  arrange(desc(priority_score)) %>%
  head(10) %>%
  mutate(
    projected_growth_rate = percent(projected_growth_rate, accuracy = 0.1),
    job_vacancy_rate = comma(job_vacancy_rate, accuracy = 0.01),
    emp_count_msa = comma(emp_count_msa)
  ) %>%
  select(
    soc_code, soc_title, description, emp_count_msa,
    projected_growth_rate, job_vacancy_rate, list_ex_titles, list_ccap_sectors
  ) %>%
  rename(
    `SOC Code` = soc_code,
    `Occupation Title` = soc_title,
    `Description` = description,
    `MSA Employment (2025)` = emp_count_msa,
    `Projected Growth Rate (2022–2032)` = projected_growth_rate,
    `Job Vacancy Rate (2024)` = job_vacancy_rate,
    `Example Job Titles` = list_ex_titles,
    `Sectors Can Contribute to` = list_ccap_sectors
  )

write_xlsx(
  priority_underBA,
  file.path(output_path, "table1_priority_occupations_underBA_table.xlsx")
)

all_occ_underBA <- onet_scored_OID_occdetails %>%
  filter(num_of_sectors > 1 & demand_stars >= 4 &
    !(education_requirements == "Bachelor's degree" |
      education_requirements == "Doctoral or professional degree" |
      education_requirements == "Graduate or professional degree"))


priority_overBA <- onet_scored_OID_occdetails %>%
  filter(num_of_sectors > 1 & demand_stars >= 4 &
    (education_requirements == "Bachelor's degree" |
      education_requirements == "Doctoral or professional degree" |
      education_requirements == "Graduate or professional degree")) %>%
  arrange(desc(priority_score)) %>%
  head(10) %>%
  mutate(
    projected_growth_rate = percent(projected_growth_rate, accuracy = 0.1),
    job_vacancy_rate = comma(job_vacancy_rate, accuracy = 0.01),
    emp_count_msa = comma(emp_count_msa)
  ) %>%
  select(
    soc_code, soc_title, description, emp_count_msa,
    projected_growth_rate, job_vacancy_rate, list_ex_titles, list_ccap_sectors
  ) %>%
  rename(
    `SOC Code` = soc_code,
    `Occupation Title` = soc_title,
    `Description` = description,
    `MSA Employment (2025)` = emp_count_msa,
    `Projected Growth Rate (2022–2032)` = projected_growth_rate,
    `Job Vacancy Rate (2024)` = job_vacancy_rate,
    `Example Job Titles` = list_ex_titles,
    `Sectors Can Contribute to` = list_ccap_sectors
  )

write_xlsx(
  priority_overBA,
  file.path(output_path, "priority_occupations_overBA_table.xlsx")
)

all_occ_overBA <- onet_scored_OID_occdetails %>%
  filter(num_of_sectors > 1 & demand_stars >= 4 &
    (education_requirements == "Bachelor's degree" |
      education_requirements == "Doctoral or professional degree" |
      education_requirements == "Graduate or professional degree"))

## ---- Growth under over BA ----
priority_growth_underBA <- onet_scored_OID_occdetails %>%
  filter(soc_code %in% c(priority_underBA$`SOC Code`)) %>%
  arrange(desc(priority_score)) %>%
  mutate(jobs_added = emp_count_msa * projected_growth_rate) %>%
  select(soc_code, soc_title, priority_score, jobs_added) %>%
  mutate(soc_title_wrapped = str_wrap(soc_title, width = 40)) %>%
  mutate(soc_title_wrapped = fct_reorder(soc_title_wrapped, desc(priority_score)))


ggplot(
  priority_growth_underBA %>%
    arrange(desc(jobs_added)) %>%
    slice_head(n = 10) %>%
    mutate(soc_title_wrapped = str_wrap(soc_title, width = 30)),
  aes(
    x = jobs_added,
    y = reorder(soc_title_wrapped, jobs_added)
  )
) +
  geom_col(fill = "#2E86AB") +
  geom_text(
    aes(label = comma(round(jobs_added, 0))),
    hjust = -0.1,
    size = 3
  ) +
  scale_x_continuous("Jobs added", labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(
    y = NULL,
    title = "Top 10 Occupations by Jobs Added",
    subtitle = "Most new jobs require less than a BA"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 9)
  )

priority_growth_overBA <- onet_scored_OID_occdetails %>%
  filter(soc_code %in% c(priority_overBA$`SOC Code`)) %>%
  arrange(desc(priority_score)) %>%
  mutate(jobs_added = emp_count_msa * projected_growth_rate) %>%
  select(soc_code, soc_title, priority_score, jobs_added) %>%
  mutate(soc_title_wrapped = str_wrap(soc_title, width = 40)) %>%
  mutate(soc_title_wrapped = fct_reorder(soc_title_wrapped, desc(priority_score)))


ggplot(
  priority_growth_overBA %>%
    arrange(desc(jobs_added)) %>%
    slice_head(n = 10) %>%
    mutate(soc_title_wrapped = str_wrap(soc_title, width = 30)),
  aes(
    x = jobs_added,
    y = reorder(soc_title_wrapped, jobs_added)
  )
) +
  geom_col(fill = "#2E86AB") +
  geom_text(
    aes(label = comma(round(jobs_added, 0))),
    hjust = -0.1,
    size = 3
  ) +
  scale_x_continuous("Jobs added", labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(
    y = NULL,
    title = "Top 10 Occupations by Jobs Added",
    subtitle = "Most new jobs require a BA+"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 9)
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
  # p25–p75 segment (wage range)
  geom_linerange(aes(ymin = h_pct25, ymax = h_pct75, colour = "25th–75th Percentile"),
    linewidth = 1.2
  ) +
  # median point
  geom_point(aes(y = h_median, color = "Median"),
    size = 3
  ) +
  # cost-of-living comparison line
  geom_hline(yintercept = cost_of_living_7co, linetype = "dashed", color = "darkgreen") +
  annotate("text",
    x = 0.5, y = cost_of_living_7co, label = "Cost of Living, Metro",
    vjust = -1, hjust = 0, size = 3.5, color = "darkgreen"
  ) +
  scale_y_continuous(
    limits = c(20, 110),
    breaks = seq(20, 110, by = 10),
    labels = scales::dollar_format()
  ) +
  scale_color_manual(
    values = c(
      "25th–75th Percentile" = "steelblue",
      "Median" = "red"
    ),
    name = NULL
  ) +
  labs(
    title = "Occupations with Min. Edu. Required < Bachelors",
    x = NULL,
    y = "Hourly Wage ($)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 10),
    plot.caption = element_text(size = 10)
  )

p_overBA <- ggplot(
  wage_dumbell %>% filter(soc_code %in% priority_overBA$`SOC Code`),
  aes(x = soc_title_wrapped)
) +
  # p25–p75 segment (wage range)
  geom_linerange(aes(ymin = h_pct25, ymax = h_pct75, colour = "25th–75th Percentile"),
    linewidth = 1.2
  ) +
  # median point
  geom_point(aes(y = h_median, color = "Median"),
    size = 3
  ) +
  # cost-of-living comparison line
  geom_hline(yintercept = cost_of_living_7co, linetype = "dashed", color = "darkgreen") +
  annotate("text",
    x = 0.5, y = cost_of_living_7co, label = "Cost of Living, Metro",
    vjust = -1, hjust = 0, size = 3.5, color = "darkgreen"
  ) +
  scale_y_continuous(
    limits = c(20, 110),
    breaks = seq(20, 110, by = 10),
    labels = scales::dollar_format()
  ) +
  scale_color_manual(
    values = c(
      "25th–75th Percentile" = "steelblue",
      "Median" = "red"
    ),
    name = NULL
  ) +
  labs(
    title = "Occupations with Min. Edu. Required >= Bachelors",
    x = NULL,
    y = "Hourly Wage ($)",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 10),
    plot.caption = element_text(size = 10)
  )

final_plot <- p_underBA + p_overBA +
  plot_layout(guides = "collect") +
  plot_annotation(caption = "Source: OEWS") &
  theme(legend.position = "right")

final_plot

ggsave(file.path(output_path, "fig6_metcouncil_wage_dumbbell_jointedu.png"), width = 12, height = 8, dpi = 300)

saveRDS(final_plot, file.path(output_path, "fig6.rds"))


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
