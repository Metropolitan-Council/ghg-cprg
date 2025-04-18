# manually delete caches, outstanding .html, and captions
source("R/_load_pkgs.R")
# manually delete caches
list.files(pattern = "*_cache") %>% fs::dir_delete()

list.files(path = "_agriculture/", pattern = "*_cache", full.names = TRUE) %>%
  fs::dir_delete()
list.files(path = "_industrial/", pattern = "*_cache", full.names = TRUE) %>%
  fs::dir_delete()
list.files(path = "_energy/", pattern = "*_cache", full.names = TRUE) %>%
  fs::dir_delete()

list.files(path = "_transportation//", pattern = "*_cache", full.names = TRUE) %>%
  fs::dir_delete()

list.files(path = "_waste/", pattern = "*_cache", full.names = TRUE) %>%
  fs::dir_delete()

list.files(path = "_meta/", pattern = "*_cache", full.names = TRUE) %>%
  fs::dir_delete()

# delete html
list.files(pattern = "*.html") %>% fs::file_delete()
list.files(path = "_energy/", pattern = "*.html", full.names = TRUE) %>%
  fs::file_delete()
list.files(path = "_agriculture/", pattern = "*.html", full.names = TRUE) %>%
  fs::file_delete()
list.files(path = "_industrial/", pattern = "*.html", full.names = TRUE) %>%
  fs::file_delete()
list.files(path = "_transportation/", pattern = "*.html", full.names = TRUE) %>%
  fs::file_delete()
list.files(path = "_waste/", pattern = "*.html", full.names = TRUE) %>%
  fs::file_delete()
list.files(path = "_meta/", pattern = "*.html", full.names = TRUE) %>%
  fs::file_delete()

# delete all captions
list.files("assets/captions/*", full.names = TRUE) %>%
  fs::file_delete()
