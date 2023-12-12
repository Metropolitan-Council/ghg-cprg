source("R/_load_pkgs.R")
source("R/_quarto_helpers.R")

if(!reticulate::virtualenv_exists("save-plotly")){
  # if virtual environment with needed python packages does not exist
  # create and use it, then install libraries

  reticulate::virtualenv_create("save-plotly")
  reticulate::use_virtualenv("save-plotly")
  
  reticulate::py_install("kaleido")
  reticulate::py_install("plotly")
}


reticulate::use_virtualenv("save-plotly")
reticulate::py_run_string("import sys")

fs::dir_create("purl")
# purl all quarto docs
list.files(path = ".", pattern = "*.qmd") %>%
  purrr::map(
    function(x) {
      knitr::purl(x,
                  output = paste0(
                    "purl/",
                    stringr::str_replace_all(x, ".qmd", ".R")
                  ),
                  documentation = 2
      )
    }
  )

# evaluate all quarto
list.files("purl/", full.names = TRUE) %>%
  purrr::map(source)
fs::file_delete("purl/")

# find all plotly objects
is_pl <- purrr::map(
  mget(ls()),
  plotly:::is.plotly
)

pls <- rlist::list.filter(is_pl, . == TRUE) %>%
  unlist()


pl_sources <- purrr::map(
  names(pls),
  function(y) {
    # browser()
    sor <- get(y)$x$source
    return(sor)
  }
)

# generate complete figure index
complete_figure_index <-
  tibble(
    object_name = names(pls),
    Figure = paste0("@", unlist(pl_sources))
  ) %>%
  left_join(
    printCap(book = TRUE) %>%
      dplyr::filter(stringr::str_detect(Figure, "fig")) %>%
      unique()
  )

saveRDS(complete_figure_index, "data/_figure_index.RDS")

# export plots -----
cli::cli_alert(paste0("Writing ", length(pls), " plots"))

# export to pdf (highest resolution)
purrr::map(
  names(pls),
  function(x) {
    save_plotly(get(x),
                fmt = "pdf",
                file_title = "source"
    )
  }
)

purrr::map(
  names(pls),
  function(x) {
    save_plotly(get(x),
                fmt = "svg",
                file_title = "source"
    )
  }
)


# export to png (lower resolution, smaller, still readable)
# purrr::map(names(pls),
#            function(x){
#              save_plotly(get(x), fmt = "png",
#                          file_title = "source")
#            })




# export leaflet -----
# 
# l_maps <- list()
# 
# purrr::map(
#   names(l_maps),
#   function(x) {
#     saveWidget(get(x), "temp.html", selfcontained = FALSE)
#     webshot("temp.html",
#             file = paste0("leaflet_png/", x, ".png"),
#             cliprect = "viewport"
#     )
#   }
# )
# 
# fs::file_delete("temp.html")

# additional export -----
# useful for maps

# htmlwidgets::saveWidget(
#   a_map,
#   file = "",
#   title = "A map",
#   selfcontained = TRUE
# )


