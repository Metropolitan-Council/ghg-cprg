# general measures of the TBI
source("R/_load_pkgs.R")
library(srvyr, warn.conflicts = FALSE)

cprg_county <- readRDS("_meta/data/cprg_county.RDS")

cprg_tbi_hh_counties <- c("Anoka MN", "Carver MN",
                          "Chisago MN", "Dakota MN", "Hennepin MN",
                          "PIERCE WI", "Ramsey MN",
                          "Scott MN", "Sherburne MN",  "ST. CROIX WI",
                          "Washington MN")


cprg_tbi_counties <- cprg_county %>% 
  mutate(tbi_county = paste0(NAME, " ", STATE_ABB)) %>% 
  magrittr::extract2("tbi_county")

load(url(paste0(
  "https://github.com/Metropolitan-Council/metc.tbi.helper/raw/main/data/",
  "tbi21.rda"
)))

tbi_home_county <- tbi21$household %>% 
  mutate(cprg_hh_county = ifelse(hh_county %in% cprg_tbi_hh_counties,
                                   home_county,
                                   "Outside CPRG area"),
  ) %>% 
  as_survey(id = hh_id,
            weights = hh_weight) %>% 
  group_by(cprg_hh_county) %>% 
  summarize(n = unweighted(n()),
            est_n = survey_total(),
            est_pct = survey_prop(proportion = TRUE)) %>% 
  arrange(-n)


saveRDS(tbi_home_county, "_transportation/data-raw/tbi/tbi_home_county.RDS")
saveRDS(hh21, "_transportation/data-raw/tbi/hh21.RDS")
