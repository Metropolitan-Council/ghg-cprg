# Metropolitan Council contributions

If you are a core member of the Met Council CPRG working group, please follow these guidelines in addition to the [CODE_OF_CONDUCT](CODE_OF_CONDUCT.md) and [CONTRIBUTING](CONTRIBUTING.md). 

We track issue completion and progress using GitHub Projects. See our project page [here](https://github.com/orgs/Metropolitan-Council/projects/8/views/1).


## Adding chapters

Use the [chapter_template.qmd](chapter_template.qmd) to start.

If you are writing a data source report, use [data_source_template.qmd](data_source_template.qmd).

Then, add your new Qmd to the list of chapters in [_quarto.yml](_quarto.yml)  

## Citation management

Create a [Zotero](https://www.zotero.org/) account and install Zotero on your machine. Once your account is created, message/email Liz and ask to be added to our [group library](https://www.zotero.org/groups/5318360/metcouncil-cprg-ghg/library). In Zotero on your machine, ensure you have installed the [BetterBibTex](https://retorque.re/zotero-better-bibtex/installation/index.html) extension. Then, change your settings to omit the "file" field (Settings > Better BibTex > Export > Fields > Fields to omit) on export; otherwise, it will include the full file path where the item lives on your machine. 

Alternatively, you can use the [Zotero Web API](https://quarto.org/docs/visual-editor/technical.html#zotero-web-api). 

To add a citation to the document you are writing in RStudio, go to "Addins" and select "Insert a citation". Enable the Zotero connection. Ensure the citation is added to our Zotero group library. Alternatively, you can set up automatic exports from Zotero to the root directory of this project. Ask Liz how to set this up.

## Data and project files

We use a Microsoft Team to share large files. Contact Joel, Krysten, or Luis to get access.

Any large data you download should NOT be tracked on GitHub to preserve our storage. Generally, only .RDS files should be tracked in git.

If you add any raw data (like shapefiles, CSVs, Excel workbooks, etc. you downloaded directly from a data source), add it to the `.gitignore` (you can use `usethis::use_git_ignore("path/to/file/file")`, edit the `.gitignore` manually, or use your git GUI to ignore the file). Then, upload the data to the our MS Team, navigating to the appropriate location in `GHG Inventory - 2024 Update/ghg-cprg/`. The file structure mirrors the file structure of this repo, so if you add data to `_energy/data-raw/` in this repo, upload it in the same place in MS Teams. 

## Testing

When possible, write a few tests for your data and functions. Save them in `/tests`. See the existing files in `/tests` for ideas. 
