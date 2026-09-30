# 02_compile_eia_wi_natgas_activity.R
# ──────────────────────────────────────────────────────────────────────────────
# Wisconsin statewide residential + commercial natural gas deliveries from
# Form EIA-176 (EIA Natural Gas Query System, report RP1), used ONLY as an
# annual weather index for 03_compile_wi_natgas_activity.R. PSCW anchors set
# levels; this series supplies year-to-year shape between anchors. Statewide
# weather is treated as representative of Pierce/St. Croix at annual scale.
#
# RP1 returns "Total of All Companies" by state (not company-level), combining
# sales + transport volumes by end-use sector, in Mcf. Residential + commercial
# is the weather-sensitive load and is almost entirely utility sales.
#
# Raw response cached to _energy/data-raw/eia_176/ (re-downloaded each run).
#
# Output:
#   WI_eia_natgas_consumption_state.RDS
#     emissions_year, residential_mcf, commercial_mcf, res_com_mcf, res_com_mmcf
# ──────────────────────────────────────────────────────────────────────────────

source("R/_load_pkgs.R")

ngqs_base_url <- "https://www.eia.gov/naturalgas/ngqs"
dir_eia_176 <- here("_energy", "data-raw", "eia_176")
dir.create(dir_eia_176, showWarnings = FALSE, recursive = TRUE)

read_ngqs_json <- function(url, destination) {
  utils::download.file(url, destfile = destination, mode = "wb", quiet = TRUE)
  if (!file.exists(destination) || file.info(destination)$size == 0) {
    stop(sprintf("NGQS download produced an empty file: %s", url))
  }
  jsonlite::fromJSON(destination, simplifyVector = FALSE)
}

# --- available RP1 years ------------------------------------------------------

report_metadata <- read_ngqs_json(
  paste0(ngqs_base_url, "/data/report"),
  file.path(dir_eia_176, "report_metadata.json")
)
rp1_meta <- Filter(function(r) identical(r$code, "RP1"), report_metadata)
if (length(rp1_meta) != 1) stop("Expected exactly one RP1 report in NGQS metadata.")

available_years <- sort(unique(as.integer(vapply(
  rp1_meta[[1]]$availableYears, function(y) y$ayear, numeric(1)
))))
first_year <- max(min(available_years), 2005L)
latest_year <- max(available_years)

# --- RP1 deliveries: Wisconsin, all companies ------------------------------------

rp1_url <- sprintf(
  "%s/data/report/RP1/data/%d/%d/ACI/Name",
  ngqs_base_url, first_year, latest_year
)
rp1 <- read_ngqs_json(
  rp1_url,
  file.path(dir_eia_176, sprintf("RP1_deliveries_%d_%d.json", first_year, latest_year))
)

rp1_raw <- bind_rows(lapply(rp1$data, tibble::as_tibble))
year_columns <- grep("^y[0-9]{4}$", names(rp1_raw), value = TRUE)
if (!all(c("a", "b", "c") %in% names(rp1_raw)) || length(year_columns) == 0) {
  stop("Unexpected RP1 response structure (need columns a, b, c and yYYYY).")
}

wi_res_com <- rp1_raw %>%
  mutate(across(all_of(c("a", "b", "c")), ~ str_trim(as.character(.x)))) %>%
  filter(a == "Wisconsin", b == "Total of All Companies") %>%
  mutate(sector = str_remove(c, " Volume$")) %>%
  filter(sector %in% c("Residential", "Commercial")) %>%
  select(sector, all_of(year_columns)) %>%
  pivot_longer(-sector, names_to = "year_column", values_to = "mcf") %>%
  mutate(
    emissions_year = as.integer(str_remove(year_column, "^y")),
    mcf = as.numeric(mcf)
  )

# Exactly one Residential and one Commercial line expected
sector_lines <- wi_res_com %>% distinct(sector, year_column) %>% count(sector)
if (!setequal(sector_lines$sector, c("Residential", "Commercial")) ||
    anyDuplicated(wi_res_com[c("sector", "emissions_year")]) > 0) {
  stop("RP1 Wisconsin Residential/Commercial lines missing or duplicated; inspect rp1_raw.")
}

wi_eia_natgas_consumption <- wi_res_com %>%
  select(emissions_year, sector, mcf) %>%
  pivot_wider(names_from = sector, values_from = mcf) %>%
  transmute(
    emissions_year,
    residential_mcf = Residential,
    commercial_mcf = Commercial,
    res_com_mcf = residential_mcf + commercial_mcf,
    res_com_mmcf = res_com_mcf / 1000 # name expected by 03 script
  ) %>%
  filter(!is.na(res_com_mcf)) %>%
  arrange(emissions_year)

# Sanity: WI res+com is roughly 200-260 million Mcf/yr
rng <- range(wi_eia_natgas_consumption$res_com_mcf)
if (rng[1] < 150e6 || rng[2] > 500e6) {
  stop(sprintf(
    "WI res+com outside plausible range (%s - %s Mcf); check RP1 units/lines.",
    format(rng[1], big.mark = ","), format(rng[2], big.mark = ",")
  ))
}

print(wi_eia_natgas_consumption, n = Inf)

write_rds(
  wi_eia_natgas_consumption,
  here("_energy", "data", "wi_eia_natgas_consumption_state.RDS")
)
