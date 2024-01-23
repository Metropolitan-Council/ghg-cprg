
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Greenhouse Gas Inventory - Climate Pollution Reduction Grant (CPRG)

<!-- badges: start -->
<!-- badges: end -->

We are tracking our progress using GitHub Projects. See our project page
[here](https://github.com/orgs/Metropolitan-Council/projects/8/views/1).

This project uses [`{renv}`](https://rstudio.github.io/renv/index.html).
Ensure you have `{renv}` installed on your machine. When you open the
project in RStudio, run `renv::restore()` in your console to
install/restore the package environment.

Additionally, be sure to set your current Council username and password
with `{keyring}` so you can utilize the defaults parameters with
`{councilR}` functions.

``` r
keyring::key_set("councilR.uid") # your username
keyring::key_set("councilR.pwd") # your password
```

## Adding chapters

Use the [chapter_template.qmd](chapter_template.qmd) to start.

If you are writing a data source report, use
[data_source_template.qmd](data_source_template.qmd).

## Building the Quarto book/site

Open this project in RStudio. Navigate to your terminal and run the
following commands

``` sh
quarto render --cache-refresh --to html
quarto preview
```

To run additional cleaning steps, run
[R/render_for_publication.R](R/render_for_publication.R).

### File structure

This is a Quarto book project. Top level Quarto files include

    #> .
    #> ├── _quarto.yml            # Quarto configuration
    #> ├── metcouncil-cprg-ghg    # references auto-imported from Zotero
    #> ├── style/                 # Quarto template, CSS styling, font files
    #> ├── assets/                # chapter numbering, captions, and filtering helpers

Documents are organized by sector and document purpose.

    #> _transportation/
    #> ├── _transportation.qmd     # main sector documentation
    #> ├── data-raw/               # raw data, R code to process and save to data\
    #> ├── data/                   # cleaned, compressed data. RDS files only
    #> ├── data_streetlight.qmd    # data source report for StreetLight
    #> ├── data_transportation.qmd # compiled sector data report
    #> ├── pa_transportation.qmd   # priority actions
    #> ├── qc_transportation.qmd   # quality control document

Several prefixes are used to mark document purpose

- `qc_{sector}` indicates a quality control report
- `pa_{sector}` indicates priority actions for a given sector.
- `data_{sector}` indicates a combined data report for given sector
- `data_{source}` indicates an individual data source report. These are
  added as child documents in the `data_{sector}` document.

When rendered, the output Quarto book/website lives in `docs/`.

### Citation management

Create a [Zotero](https://www.zotero.org/) account and install Zotero on
your machine. Once your account is created, message/email Liz and ask to
be added to our [group
library](https://www.zotero.org/groups/5318360/metcouncil-cprg-ghg/library).
In Zotero on your machine, ensure you have installed the BetterBibTex
and changed your settings to omit the “file” field (Settings \> Better
BibTex \> Export \> Fields \> Fields to omit).

Alternatively, you can use the [Zotero Web
API](https://quarto.org/docs/visual-editor/technical.html#zotero-web-api).

To add a citation to the document you are writing, go to “Addins” and
select “Insert a citation”. Enable the Zotero connection. Ensure the
citation is added to our Zotero group library. Alternatively, you can
set up automatic exports from Zotero to the root directory of this
project.

## Data and project files

We use a [MS
Team](https://teams.microsoft.com/l/channel/19%3a0ea5e55bb4c8449a98334fc1402d4fae%40thread.skype/GHG%2520Inventory?groupId=44f6d2b9-a73a-4969-9267-de486d00b3b8&tenantId=ddbff68b-482a-4573-81e0-fef8156a4fd0).
Contact Joel, Krysten, or Luis to get access.

Any data you download should NOT be tracked on GitHub to preserve our
storage. Generally, only .RDS files should be tracked in git.

If you add any raw data (like shapefiles, CSVs, Excel workbooks, etc.
you downloaded directly from a data source), add it to the `.gitignore`
(you can use `usethis::use_git_ignore("path/to/file/file")`, edit the
`.gitignore` manually, or use your git GUI). Then, upload the data to
the our MS Team, navigating to the appropriate location in
`GHG Inventory - 2024 Update/ghg-cprg/`. The file structure mirrors the
file structure of this repo, so if you add data to `_energy/data-raw/`
in this repo, upload it in the same place in MS Teams.

## Testing

When possible, write a few tests for your data and functions. Save them
in `/tests`. See the existing files in `/tests` for ideas.
