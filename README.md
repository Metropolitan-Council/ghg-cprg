
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Greenhouse Gas Inventory - Climate Pollution Reduction Grant (CPRG)

*This project has been funded wholly or in part by the United States
Environmental Protection Agency (EPA) under assistance agreement
00E03476 to the Metropolitan Council. The contents of this repository do
not necessarily reflect the views and policies of the EPA, nor does the
EPA endorse trade names or recommend the use of commercial products
mentioned in this repository.*

This repository contains essential data, code, and documentation for the
greenhouse gas (GHG) inventory, as completed for the Twin Cities MSA
Priority Climate Action Plan (PCAP), submitted to the EPA on March 1,
2024. You can find the full PCAP on the [EPA
website](https://www.epa.gov/system/files/documents/2024-03/metropolitan-council-twin-cities-msa-priority-climate-action-plan.pdf).

## Repository details

### File structure

This is a Quarto book project. Top level Quarto files include

    #> .
    #> ├── _quarto.yml                # Quarto configuration
    #> ├── metcouncil-cprg-ghg.bib    # references auto-imported from Zotero
    #> ├── style/                     # Quarto template, CSS styling, font files
    #> ├── assets/                    # chapter numbering, captions, and filtering helpers

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

Common datasets, like spatial geometries, population, county population
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
    #> ├── qc_transportation.qmd   # quality control document

Several prefixes are used to denote document purpose

- `qc_{sector}.qmd` indicates a quality control report
  <!-- - `pa_{sector}.qmd` indicates priority actions for a given sector. -->
- `data_{sector}.qmd` indicates a combined data report for given sector
- `data_{source}.qmd` indicates an individual data source report. These
  are added as child documents in the `data_{sector}` document.

Within each sector folder is a `data-raw/` folder. This is where you
should put any processing/calculation code and raw data. The code in
`data-raw/` will save output datasets in `data/`. Generally, only .RDS
data should be saved in `data/`

### Building the Quarto book/site

Clone or fork the repository and `ghg-cprg.Rproj` in RStudio. RStudio
will prompt you to install packages through
[`{renv}`](https://rstudio.github.io/renv/index.html) with
`renv::restore()`. Once package installation is complete, open
[R/render_for_publication.R](R/render_for_publication.R) and run the
last two lines, previewed below.

``` r
rstudioapi::terminalExecute("quarto render --cache-refresh --to html")
rstudioapi::terminalExecute("quarto preview")
```

`quarto render` renders all the Qmd documents into HTML in /docs.
`quarto preview` launches a live server previewing the book/site in your
web browser.

### Citation management

We created a shared [Zotero](https://www.zotero.org/) group library.
`metcouncil-cprg-ghg.bib` is the BibTex export. If you would like access
through your own Zotero account, please contact @eroten or @pawilfahrt.

### Data and project files

All data required to render the Quarto document and perform essential
calculations are stored in .RDS files within each sector folder.

Where possible, we access data directly from source providers through
APIs and programmatic downloads. However, some datasets were compiled
manually from various sources. All data processing code live within each
sector folder in `/data-raw/`.

To better manage our GitHub data storage, we have not included most of
the raw data in this repository. See more details on specific datasets
below. We can share our raw data downloads upon request.

| Sector                               | Data source                             | Availability                                                                                                                                                                                                                                                                                                       |
|:-------------------------------------|:----------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Energy - Electricity and natural gas | Individual utility reporting documents  | Many of these documents are large PDFs downloaded directly from utility websites.                                                                                                                                                                                                                                  |
| Transportation                       | StreetLight Data                        | StreetLight Data© is a subscription based service. If you have access to the Minnesota DOT Regional Subscription, you can find the exact analyses we used on [StreetLight Insight](https://insight.streetlightdata.com/). Otherwise, the processed data are available in `_transportation/data-raw/analysis_runs/` |
| Transportation                       | MnDOT tables                            | MnDOT county/route system VMT and volume trends with vehicle type distribution reports are available on the [MnDOT website](https://www.dot.state.mn.us/traffic/data/data-products.html)                                                                                                                           |
| Transportation                       | WisDOT tables                           | WisDOT traffic counts and vehicle type distributions are available on the [WisDOT website](https://data-wisdot.opendata.arcgis.com/)                                                                                                                                                                               |
| Waste - solid waste                  | MPCA SCORE                              | MPCA score data were downloaded from their interactive [Tableau](https://public.tableau.com/app/profile/mpca.data.services/viz/SCOREOverview/SCOREOverview)                                                                                                                                                        |
| All                                  | MPCA GHG Inventory                      | Minnesota state emisssions data downloaded from their interactive [Tableau](https://public.tableau.com/app/profile/mpca.data.services/viz/GHGemissioninventory/GHGsummarystory)                                                                                                                                    |
| All                                  | Wisconsin statewide emissions           | Downloaded from Wisconsin DNR [website](https://widnr.widen.net/view/pdf/o9xmpot5x7/AM610.pdf?t.download=true)                                                                                                                                                                                                     |
| All                                  | EPA Local Greenhouse Gas Inventory Tool | Available on the [EPA website](https://www.epa.gov/statelocalenergy/local-greenhouse-gas-inventory-tool).                                                                                                                                                                                                          |
| All                                  | EPA GHG Emissions Factor Hub            | Available on the [EPA website](https://www.epa.gov/climateleadership/ghg-emission-factors-hub)                                                                                                                                                                                                                     |

## Testing

To ensure consistency, there are a set of tests located in `tests/`.
These are run before rendering in `R/render_for_publication.R`.

## Contributing and Code of Conduct

Contributions are welcome. Please review our [contribution
guide](CONTRIBUTING.md) before making a pull-request. Met Council
employees should also review the [internal contribution
guide](CONTRIBUTING_INTERNAL.md).

Please note that the ghg-cprg project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project
you agree to abide by its terms.
