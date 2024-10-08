#' Download and read in an Excel file or CSV from a remote URL
#'
#' @param url character, file location URL. Must end with either ".xlsx" or ".xls"
#' @param exdir character, directory location where to save downloaded document
#' @param force_download logical, whether to force a fresh download, regardless
#'   of whether the file exists already. Default value is `FALSE`.
#' @param ... Additional arguments passed to readxl::read_excel() or readr::read_csv
#'
#' @return tibble
#'
#' @examples
#'
#' download_read_table("https://www.dot.state.mn.us/traffic/data/reports/Current_CC_StationList.xlsx",
#' "_transportation/data-raw/mndot/",
#' sheet = 1)
#'
#'
download_read_table <- function(url,
                                exdir,
                                force_download = FALSE,
                                ...) {
  # split URL to get file name
  url_split <- strsplit(url, split = "/")
  file_name <- tail(url_split[[1]], n = 1)

  # if the downloaded file does not already OR
  # we are forcing a fresh download
  # download the file and save in exdir
  if (!file.exists(file.path(exdir, file_name)) | force_download == TRUE) {
    download.file(url,
      destfile = file.path(exdir, file_name),
      mode = "wb"
    )
  }

  # read and return file
  if (fs::path_ext(file_name) == "csv") {
    readr::read_csv(
      file = file.path(exdir, file_name),
      ...
    )
  } else {
    readxl::read_excel(path = file.path(exdir, file_name), ...)
  }
}
