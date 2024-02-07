
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

Open this project in RStudio. Open
[R/render_for_publication.R](R/render_for_publication.R) and run the
last two lines, previewed below.

``` r
rstudioapi::terminalExecute("quarto render --cache-refresh --to html")
rstudioapi::terminalExecute("quarto preview")
```

### File structure

This is a Quarto book project. Top level Quarto files include

    #> .
    #> ├── _quarto.yml            # Quarto configuration
    #> ├── metcouncil-cprg-ghg    # references auto-imported from Zotero
    #> ├── style/                 # Quarto template, CSS styling, font files
    #> ├── assets/                # chapter numbering, captions, and filtering helpers

All scripts in `R/` are ready to run at will.

    #> R/
    #> ├── _export_plotly.R
    #> ├── _leaflet_helpers.R
    #> ├── _load_pkgs.R
    #> ├── _quarto_helpers.R
    #> ├── cprg_colors.R
    #> ├── global_warming_potential.R
    #> ├── plot_county_emissions.R
    #> ├── remove_caches.R
    #> ├── ...

Common datasets, like shapefiles, population, county population
proportions, GHG emission factors, and other data, live in \_meta/data/.
Each usually as associated metadata, noted with
`[dataset_name]_meta.RDS`

    #>_meta/data/
    #> ├── cprg_county.RDS
    #> ├── cprg_county_emissions.CSV
    #> ├── cprg_county_emissions.RDS
    #> ├── cprg_county_emissions_meta.RDS
    #> ├── cprg_county_meta.RDS
    #> ├── cprg_county_proportions.RDS
    #> ├── cprg_county_proportions_meta.RDS
    #> ├── cprg_population.RDS
    #> ├── ...

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

- `qc_{sector}.qmd` indicates a quality control report
- `pa_{sector}.qmd` indicates priority actions for a given sector.
- `data_{sector}.qmd` indicates a combined data report for given sector
- `data_{source}.qmd` indicates an individual data source report. These
  are added as child documents in the `data_{sector}` document.

`data-raw/` is present in each sector and is where you should put any
processing/calculation code for items in `data/`. Generally, only .RDS
data should be saved in `data/`

When rendered, the output Quarto book/website lives in `docs/`.

### Citation management

Create a [Zotero](https://www.zotero.org/) account and install Zotero on
your machine. Once your account is created, message/email Liz and ask to
be added to our [group
library](https://www.zotero.org/groups/5318360/metcouncil-cprg-ghg/library).
In Zotero on your machine, ensure you have installed the
[BetterBibTex](https://retorque.re/zotero-better-bibtex/installation/index.html)
extension. Then, change your settings to omit the “file” field (Settings
\> Better BibTex \> Export \> Fields \> Fields to omit) on export;
otherwise, it will include the file path where the item lives on your
machine.

Alternatively, you can use the [Zotero Web
API](https://quarto.org/docs/visual-editor/technical.html#zotero-web-api).

To add a citation to the document you are writing in RStudio, go to
“Addins” and select “Insert a citation”. Enable the Zotero connection.
Ensure the citation is added to our Zotero group library. Alternatively,
you can set up automatic exports from Zotero to the root directory of
this project. Ask Liz how to set this up.

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
