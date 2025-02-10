source("R/_load_pkgs.R")
source("R/remove_caches.R")
source("_meta/data-raw/compile_county_emissions.R")
    
styler::style_dir(include_roxygen_examples = FALSE, 
                  exclude_dirs = c("_transportation/data-raw/metc_travel_model",
                                   "renv"))

testthat::test_dir("tests")

rstudioapi::terminalExecute("quarto render --cache-refresh --to html")
rstudioapi::terminalExecute("quarto preview")
