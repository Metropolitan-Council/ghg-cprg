# start caption index
saveRDS("00", file.path(here::here(), "caption_index.RDS"))

if (!fs::dir_exists(file.path(here::here(), "assets/captions"))) {
  fs::dir_create(file.path(here::here(), "assets/captions"))
}
