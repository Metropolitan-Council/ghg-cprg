## Checklist

Please complete this checklist as a courtesy to the PR reviewer.  

Code and styling
* [ ] All of the files/scripts I added are in the right place and named appropriately. See the [README](https://github.com/Metropolitan-Council/ghg-cprg/blob/main/README.md) for details.
* [ ] I have not used `setwd()`
* [ ] I have used `file.path(here::here(), "file_name"))` to source any scripts or read in data
* [ ] I have not added any large datasets, unless absolutely necessary (explain)
* [ ] I have commented my code, particularly in hard to understand areas
* [ ] I have added additional package dependencies as necessary to renv with `renv::install()`
* [ ] I have run `styler::style_dir(".", recursive = TRUE, filetype = c("R", "qmd"))`
* [ ] Plots
  - [ ] If plotly, have you "source" argument that matches the chunk name? 
  - [ ] correct formatting using `councilR::plotly_layout()`?
* [ ] chunk formatting
  - [ ] all chunks named
  - [ ] all figure or table chunks have caption
  - [ ] `out.width: "95%"` and `out.height: "60%"`

Document editing
* [ ] I have named my code chunks
* [ ] My plots have succinct and accurate figure captions using `#| fig.cap: "{caption here}"`
* [ ] I have used `out.width` and `out.height` in **percentage** format in figure configuration
* [ ] I have ensured that modified documents knit successfully from [`render_for_publication.R`](https://github.com/Metropolitan-Council/ghg-cprg/blob/main/R/render_for_publication.R)
* [ ] I have ensured the chapter I've modified knits successfully
* [ ] I have fixed any missing citations
* [ ] I have reviewed my contributions for typos and misspellings.

GitHub and project management
* [ ] I have identified and assigned at least one Reviewer (Liz, Sam, Laine) to this PR
* [ ] I have assigned myself to this PR
* [ ] I have updated the status in the [GitHub Project](https://github.com/orgs/Metropolitan-Council/projects/8/views/1)
