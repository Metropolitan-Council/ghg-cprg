source("R/_load_pkgs.R")
source("R/remove_caches.R")
source("_meta/data-raw/compile_county_emissions.R")
saveRDS("00", file.path(here::here(), "caption_index.RDS"))

styler::style_dir()

testthat::test_dir("tests")

rstudioapi::terminalExecute("quarto render --cache-refresh --to html")
rstudioapi::terminalExecute("quarto preview")
