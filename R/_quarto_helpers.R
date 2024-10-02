# https://github.com/harrelfe/rscripts
#
#
# Figures -----

table_styling <- c("striped", "hover")

addCap <- function(label = NULL, cap = NULL, short_cap = NULL) {
  get_chunk_opt <- function(tag1, tag2, tag3) {
    r <- knitr::opts_current$get(tag1)
    if (!length(r)) {
      r <- knitr::opts_current$get(tag2)
    }
    if (!length(r)) {
      r <- knitr::opts_current$get(tag3)
    }

    r
  }

  alter_label <- function() {
    lab <- get_chunk_opt("label")
    # if chunk label exists and it does NOT start with "fig"
    if (length(lab) && (!grepl("^fig-", lab) && !grepl("^tbl-", lab))) {
      # assign lab
      lab <- paste0("fig-", lab)
    }
    lab
  }

  # if no label, alter label
  if (!length(label)) {
    label <- alter_label()
  }

  deb <- .Options$debugaddCap
  deb <- length(deb) && deb

  if (deb) {
    cat("label:", label, "\n", file = "/tmp/z", append = TRUE)
  }

  # if there is no label, return invisible list
  if (!length(label)) {
    return(invisible(list(NULL, NULL, NULL)))
  }

  # if label is logical and is false, return invisible list
  if (is.logical(label) && !label) {
    return(invisible(list(NULL, NULL, NULL)))
  }

  # if there is no cap , get cap from chunk
  if (!length(cap)) {
    cap <- get_chunk_opt("fig.cap", "tbl.cap", "cap", "tbl-cap")
  }

  # if there is no short_cap, get short_cap from chunk
  if (!length(short_cap)) {
    short_cap <- get_chunk_opt("fig.short_cap", "short_cap")
  }

  # if there is no cap but there is a short cap, re-assign cap
  if (!length(cap) && length(short_cap)) {
    cap <- short_cap
  }

  # if there is no short_cap but there is a cap, re-assign short_cap
  if (!length(short_cap) && length(cap)) {
    short_cap <- cap
  }

  # if global global_caption_index does NOT exist, create it
  if (!exists("global_caption_index")) {
    global_caption_index <<- NULL
  }

  # create data frame with label, caption, and short caption
  info <- data.frame(label = label, cap = cap, short_cap = short_cap)


  if (deb) {
    prn(info, fi = "/tmp/z")
  }

  # if there is nothing in global_caption_index OR
  # label is NOT in global_caption_index$label
  # if (!length(global_caption_index) || !label %in% global_caption_index$label) {
  # bind new info table to global captions
  global_caption_index <<- rbind(global_caption_index, info)
  # }
  invisible(list(label = label, cap = cap, short_cap = short_cap))
}


saveCap <- function(basename) {
  if (exists("global_caption_index")) {
    # cli::cli_inform(c("saving global index for ", basename))
    saveRDS(global_caption_index, file = paste0("assets/captions/captions-", basename, ".rds"), compress = "xz")
  }
}


printCap <- function(book = FALSE) {
  if (book) {
    files <- list.files(path = "assets/captions/", full.names = TRUE)
    global_caption_index <- NULL
    for (f in files) global_caption_index <- rbind(global_caption_index, readRDS(f))
  }
  cap <- global_caption_index[c("label", "short_cap")]

  # use '@' to cross reference exact figure
  cap$label <- paste0("@", cap$label)
  names(cap) <- c("Figure", "Short Caption")
  if (book) {
    cap
  } # knitr::kable(cap, row.names=FALSE)
  else {
    knitr::kable(cap, row.names = FALSE, format = "html")
  }
}


hookaddcap <- function(loc = NULL) {
  caption_file <- function(before, options, envir) {
    # attach this function to given chunk
    if (!before) {
      return()
    }
    # create/find label and caption ----
    label <- knitr::opts_current$get("label")
    cap <- options$fig.cap


    if (!length(cap)) {
      cap <- options$tbl.cap
    }


    if (!length(cap)) {
      cap <- options$`tbl-cap`
    }

    # if 'cap' is empty, try 'tbl.cap'

    # if  still empty, use 'cap'
    if (!length(cap)) {
      cap <- options$cap
    }

    # if cap is a function, eval
    if (length(cap) && is.call(cap)) {
      cap <- eval(cap)
    }
    ## Chunk produced a figure if label: fig- and fig-cap were
    if (!length(cap) || cap == "" || (!grepl("^fig-", label) && !grepl("^tbl-", label))) {
      return()
    }

    # create/find short caption----
    short_cap <- options$fig.short_cap

    # if 'fig.short_cap' is empty, try looking for 'short_cap'
    if (!length(short_cap)) {
      short_cap <- options$short_cap
    }
    # if short_cap is a function
    if (length(short_cap) && is.call(short_cap)) {
      short_cap <- eval(short_cap)
    }

    # if short_cap is empty, reassign cap
    if (!length(short_cap) || short_cap == "") {
      short_cap <- cap
    }
    ## addCap will ignore an entry if global_caption_index already has an entry
    ## with the same label.  So if use manually put addCap() inside a chunk
    ## it is likely that a second entry will be avoided
    addCap(label, cap, short_cap)
  }
  knitr::knit_hooks$set(addcapfile = caption_file)
  knitr::opts_chunk$set(addcapfile = TRUE)
  if (length(loc)) {
    knitr::opts_chunk$set(fig.cap.location = loc)
  }
}


save_plotly <- function(a_plotly, fmt = c("png", "pdf", "svg"),
                        file_title = c("source", "title"),
                        file_location = paste0("assets/plots/plotly_", fmt),
                        width = 900,
                        height = 450,
                        scale = 1) {
  # browser()
  pl_titles <-
    purrr::map(
      1:length(a_plotly$x$layoutAttrs),
      function(x) {
        a_plotly$x$layoutAttrs[[x]]$title$text
      }
    )

  pl_title <- pl_titles %>%
    rlist::list.clean() %>%
    rlist::list.filter(. != "") %>%
    unlist() %>%
    stringr::str_replace_all("[^[:alnum:]]", " ")

  # browser()
  pl_source <-
    purrr::map(
      1:length(a_plotly$x$layoutAttrs),
      function(x) {
        a_plotly$x$source
      }
    ) %>%
    unique() %>%
    unlist()

  if (!fs::dir_exists(file_location)) {
    fs::dir_create(file_location, recurse = TRUE)
  }

  file_name <- if (file_title == "source") {
    paste0(
      file_location, "/",
      pl_source, ".", fmt
    )
  } else if (file_title == "title") {
    paste0(
      file_location, "/",
      pl_title, ".", fmt
    )
  }

  scope <- kaleido()

  scope$transform(
    p = a_plotly,
    file = file_name,
    width = width,
    height = height,
    scale = scale
  )

  if (fmt == "pdf") {
    Sys.sleep(1)
    scope$transform(
      p = a_plotly,
      file = file_name
    )
  }
}


# thanks to R for the Rest of Us
# https://rfortherestofus.com/2022/10/automating-sentences-with-r
# Example:
# > 1:4 %>% listify()
# "1, 2, 3 and 4"
listify <- function(my_vector) {
  if (length(my_vector) > 1) {
    paste(
      paste(my_vector[1:length(my_vector) - 1],
        collapse = ", "
      ),
      "and",
      my_vector[length(my_vector)]
    )
  } else {
    # if length == 1, we don't want to print "and blah"
    paste(my_vector)
  }
}
