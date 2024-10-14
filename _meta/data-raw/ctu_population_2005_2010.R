source("R/_load_pkgs.R")

ctu_estimates <- readxl::read_xls("_meta/data-raw/2009PopulationEstimates.xls") %>% 
  select(
    1:3
  )
ctu_list <- vector(mode = "list", length = 8)

ctu_list[[1]] <- slice(ctu_estimates, 5:26) %>%
  mutate(county = "Anoka")
# see clean_tabula_tables
# is this the best way to do it? maybe not but it's how i'm doing it
# this section on hold until we can hook it up to FRED

# 2011-2019
ctu_estimates_2011 <- readxl::read_xlsx("_meta/data-raw/IntercensalEstimates.xlsx")
# what more do i do. she's already beautiful.
# ik ik i have to figure out the COCTU stuff
# and then let's assign some proportions on a county level
# and fix naming conventions to align with what we had decided
