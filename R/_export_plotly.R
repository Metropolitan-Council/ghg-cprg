source("R/_load_pkgs.R")
source("R/_quarto_helpers.R")

if (!reticulate::virtualenv_exists("save-plotly")) {
  # if virtual environment with needed python packages does not exist
  # create and use it, then install libraries

  reticulate::virtualenv_create("save-plotly")
  reticulate::use_virtualenv("save-plotly")

  reticulate::py_install("kaleido")
  reticulate::py_install("plotly")
}


reticulate::use_virtualenv("save-plotly")
reticulate::py_run_string("import sys")

fs::dir_create("purl/_energy", recurse = TRUE)
fs::dir_create("purl/_meta")
fs::dir_create("purl/_transportation")
fs::dir_create("purl/_waste")


# purl all quarto docs
list.files(path = ".", pattern = "*.qmd", recursive = TRUE) %>%
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

fs::file_delete("purl/chapter_template.R")
fs::file_delete("purl/data_source_template.R")
fs::file_delete("purl/_meta/data_request_template.R")

# evaluate all quarto
list.files("purl",
  full.names = TRUE,
  pattern = ".R", recursive = TRUE
) %>%
  purrr::map(source)

fs::file_delete("purl/")
fs::file_delete("assets/plots/")

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
      unique(),
    by = "Figure"
  )

saveRDS(complete_figure_index, "assets/_figure_index.RDS")

# export plots -----
cli::cli_alert(paste0("Writing ", length(pls), " plots"))


purrr::map(
  names(pls),
  function(x) {
    save_plotly(get(x),
      fmt = "png",
      file_title = "source",
      width = 900,
      height = 450,
      scale = 2
    )
  }
)



# create plots without titles ----
purrr::map(
  names(pls),
  function(x) {
    this_pl <- get(x)

    # navigate to text
    # and change to empty character
    this_pl$x$layoutAttrs[[1]]$title$text <- ""

    save_plotly(this_pl,
      fmt = "png",
      file_title = "source",
      file_location = "assets/plots/plotly_no_title_png",
      width = 900,
      height = 450,
      scale = 2
    )
  }
)
