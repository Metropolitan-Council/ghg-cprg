# get data from the 2020 national emissions inventory
source("R/_load_pkgs.R")

if (file.exists("_transportation/data-raw/epa/nei2020/2020nei_onroad_byregion/onroad_5.csv") == FALSE) {
  cli::cli_abort(c(
    "Required datasets unavailable",
    "*" = "Download NEI data from the EPA website",
    "*" = "{.url https://www.epa.gov/air-emissions-inventories/2020-national-emissions-inventory-nei-data}"
  ))
}


cprg_county <- readRDS("R/data/cprg_county.RDS")

# MN and WI are in EPA district 5
nei_onroad <- read.csv("_transportation/data-raw/epa/nei2020/2020nei_onroad_byregion/onroad_5.csv") %>% 
  clean_names() %>% 
  filter(fips_code %in% cprg_county$GEOID,
         pollutant_type_s == "GHG")


# data downloaced from the NEI search tool
# https://enviro.epa.gov/envirofacts/nei/search/results?q=N4Ig7glgJg5gpgFwMIEMA2aQC4Bm6DOcANCPgK4C2FKATgJ4DyADgvgGrralwDGCA9jQDKlajQhx8IEjThNBCCADsYATTi12nLCABMABgPTSo2nQAycAG5w0WzDp78yShHWP4EKBJIBCdACU4GAh%2BJQ4HAG1QNBQAI1suAFllJUl%2BL2MrdDI4Ll0AdhAAXyIY%2BMSdAHUIfCclfGUsnLydAFY2koBdEicXRT86IS8fCOxInu4%2BQXtx8oSHECT%2BOIg0OAACAFoNhiUtgP4UKA2AEQlCNA3zCBgACwQzsjcNtjg7iB51qRJstFyuABGAAsJTKIFiC2SKzWmx2ewORxOSjCW3OklsGwAEhorHQni83h8vpJmv9WnoABxg%2BaVJYw9bbXb7Q7HDYo-boy7XW4PAn4omfb5kgHtUGlWmLZarRnwllIs4XTE4lB4-mvd5C0m-FpA3Q0iEVKUMuEbAByqNZJwAogBHMgQJgUOCuJkMBB3OA0EUUgDMBshdOlsKZFvlbLtDqdLseOwA4ih8Pw0Mo8jryUDOhLDVCdMHZebLQrI47na6dlzEunRSBfUVs4HjTLTeZ%2BE4KBkIDYfiA-jXAQA2ANG6HNplIfhUL08CDoDZJWipjX4S49vsU3QATmHufpY52AEEIDQeDQUDgED6uG0h8VJgMaLMsBMSPIMM8UK58P4AGIoGcpm4YzPpMb7-F4X7%2BAAKhINDAS%2BIBgR%2BkFDLwAhwdo0Q5nSqA0HEYSKvwAAe0CmkgDC6EyAAUcZYnGACUV46OR%2BqvrETTMbQ%2BFKIRJFQHkDYjnmiB3J%2BZFYsC1G0QxTEgEgEnGEw7FKMkIliTudJmhACA0M4%2BC7HxppmroDBSXRjHVhSxkMIpylcFpOl6QZpHdDIcgKMoagaI%2Bcy9rqOgGEYJCNvkhj6Oa1oAJIGuuoWAkUwVCXo%2BjxRF0XZrFAUpaCiW7gYIJpRsP7KHObyPqEPH6hl-nJYCgLGCFWV1YVZWNARVXgplyX6NSuV0gYPUtV6bU8f6d4kOQVBmMwrC%2BV1hDTMIpjiNq2GLEIaGCBsIhTStPb8XU9neBV6BENtIzEBsE79Pin4nFB4hxHOk1iBcGwwLpZBMHAJxxPi1AAFZbUmZAnpsC3ofpf0bEhEEIFR%2BD0QAdDFNUPoCO2vatjUgDBXobIC23LRcxgHTwR2KGEp3nd4l3Xa4t1KPdj3PcTkgbNDeM0ATHP4rDn7w4jSMbFzPPKF8ZAHRs7ZxGQngVWdyhQHLOmzmgZ1hLpxwAPQclrJx3RsFC1DwtixGkTkQzMwtc-ptCbHLP0bMoMNoBkigqBsOkulA%2BAo9VGY6HgAHaUMbM9jjf4hy8mOLqtZMUyd6s0z4Z30y8hsPRAT1XFHayh17GQELzMPJuBAsI8jrkmLtFjWLYUhYKAVves%2BoBdUox1hA1SVml3KkBzWni0z3u7DCPg8Un0DOjzhzgMxsW1Zwk1fo3NNWd5TKl9Ysfdb6jgekBds-rcfk9cNPbgn1w6f4kvj0CZMwf51fbd%2BYfm8VdfOh71-586MPHw38QDjyAf-EAz9ALuB3lwPOUDujFGKEAA
nei_search <- read.csv("_transportation/data-raw/epa/nei2020/nei_search.csv") %>% 
  clean_names() %>% 
  # fix FIPS code
  mutate(county_fips = paste0(state_fips, stringr::str_pad(as.character(county_fips), width = 3,
                                        side = "left", "0"))) %>% 
  filter(county_fips %in% cprg_county$GEOID)

