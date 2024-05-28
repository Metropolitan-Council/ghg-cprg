if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
  fs::dir_copy("style", "docs/_transportation/style/", overwrite = TRUE)
  fs::dir_copy("style", "docs/_energy/style/", overwrite = TRUE)
  fs::dir_copy("style", "docs/_waste/style/", overwrite = TRUE)
  fs::dir_copy("style", "docs/_meta/style/", overwrite = TRUE)
  fs::dir_copy("style", "docs/_nature/style/", overwrite = TRUE)
  fs::dir_copy("style", "docs/_agriculture/style/", overwrite = TRUE)
  fs::dir_copy("assets/maps", "docs/_meta/assets/maps", overwrite = TRUE)
  quit()
}
