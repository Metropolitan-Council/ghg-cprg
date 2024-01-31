source("R/_load_pkgs.R")
source("_meta/data-raw/compile_county_emissions.R")
# if necessary, remove all cache's and stray html files
# source("R/remove_caches.R")
styler::style_dir()

testthat::test_dir("tests")

rstudioapi::terminalExecute("quarto render --cache-refresh --to html")
rstudioapi::terminalExecute("quarto preview")
