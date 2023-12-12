
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Greenhouse Gas Inventory - Climate Pollution Reduction Grant (CPRG)

<!-- badges: start -->
<!-- badges: end -->

This project uses [`{renv}`](https://rstudio.github.io/renv/index.html).
Ensure you have `{renv}` installed on your machine. When you open the
project in RStudio, run `renv::restore()` in your console to
install/restore the package environment.

## Building the Quarto book/site

Open this project in RStudio. Navigate to your terminal and run the
following commands

``` sh
quarto render --cache-refresh --to html
quarto preview
```

### File structure

This is a Quarto book project. Top level Quarto files include

    #> .
    #> ├── _quarto.yml            # Quarto configuration
    #> ├── style                  # Quarto template, CSS styling, font files
    #> ├── assets                 # chapter numbering and filtering

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
citation will be written to /references.bib and that the item is added
to our Zotero group library.
