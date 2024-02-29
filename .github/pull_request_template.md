## Checklist

Please complete this checklist as a courtesy to the PR reviewer.  

Code and styling
* [ ] All of the files/scripts I added are in the right place and named appropriately. See the [README](https://github.com/Metropolitan-Council/ghg-cprg/blob/main/README.md) for details.
* [ ] I have not used `setwd()`
* [ ] I have used `file.path(here::here(), "file_name"))` to source any scripts or read in data
* [ ] I have not added any large datasets, unless absolutely necessary (explain)
* [ ] I have commented my code, particularly in hard to understand areas
* [ ] I have added additional package dependencies as necessary with `renv::install()`
* [ ] I have run `styler::style_dir(".", recursive = TRUE, filetype = c("R", "qmd"))`
* [ ] Plots
  - [ ] If plotly, use `source = "chunk-name"` in `plot_ly()`
  - [ ] Use formatting with `councilR::plotly_layout()`?
* [ ] Chunk formatting
  - [ ] All chunks named
  - [ ] All figure or table chunks have caption
  - [ ] `out.width: "95%"`. If a specific height is needed, use pixels. and/or `out.height: "500px"`

Document editing
* [ ] I have ensured that modified documents knit successfully from [`render_for_publication.R`](https://github.com/Metropolitan-Council/ghg-cprg/blob/main/R/render_for_publication.R)
* [ ] I have fixed any missing citations, cross references, hyperlinks
* [ ] I have reviewed my contributions for typos and misspellings.

GitHub and project management
* [ ] I have identified and assigned at least one Reviewer (Liz, Sam, Laine) to this PR
* [ ] I have assigned myself to this PR
* [ ] I have updated the status in the [GitHub Project](https://github.com/orgs/Metropolitan-Council/projects/8/views/1)
