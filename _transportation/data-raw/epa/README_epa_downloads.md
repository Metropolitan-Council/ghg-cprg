
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EPA download guide

This document details how the \_transportation/data-raw/epa/ folder is
organized and lists the associated processing scripts for each dataset.

Altogether, this folder comes to nearly 200 GB of files. If you plan to
replicate these downloads on your machine, account for space
accordingly.

All download links and essential description information is documented
in [epa_downloads.xlsx](_transportation/data-raw/epa/epa_downloads.xlsx)

Keep in mind that URLs are relatively fragile and may change at any
time.

## Folder structure

<PRE class="fansi fansi-output"><CODE>#&gt; <span style='color: #0000BB; font-weight: bold;'>/Users/rotenle/Documents/MetC_Locals/Interdivisional/ghg-cprg/_transportation/data-raw/epa/nei/</span>
#&gt; ├── <span style='color: #0000BB; font-weight: bold;'>2008NEI</span>
#&gt; │   ├── <span style='color: #0000BB; font-weight: bold;'>2008neiv3_nonroad_byregions</span>
#&gt; │   ├── <span style='color: #0000BB; font-weight: bold;'>2008neiv3_onroad_byregions</span>
#&gt; │   ├── <span style='color: #0000BB; font-weight: bold;'>references</span>
#&gt; │   └── <span style='color: #0000BB; font-weight: bold;'>section4-mobile</span>
#&gt; ├── <span style='color: #0000BB; font-weight: bold;'>2011NEI</span>
#&gt; │   ├── <span style='color: #0000BB; font-weight: bold;'>2011neiv2_nonroad_byregions</span>
#&gt; │   └── <span style='color: #0000BB; font-weight: bold;'>2011neiv2_onroad_byregions</span>
#&gt; ├── <span style='color: #0000BB; font-weight: bold;'>2014NEI</span>
#&gt; │   ├── <span style='color: #0000BB; font-weight: bold;'>2014neiv2_nonroad_byregions</span>
#&gt; │   ├── <span style='color: #0000BB; font-weight: bold;'>2014neiv2_onroad_byregions</span>
#&gt; │   └── <span style='color: #0000BB; font-weight: bold;'>2014v2_onroad_activity_final</span>
#&gt; ├── <span style='color: #0000BB; font-weight: bold;'>2017NEI</span>
#&gt; │   ├── <span style='color: #0000BB; font-weight: bold;'>2017NEI_onroad_activity_final</span>
#&gt; │   ├── <span style='color: #0000BB; font-weight: bold;'>2017neiApr_nonroad_byregions</span>
#&gt; │   └── <span style='color: #0000BB; font-weight: bold;'>2017neiApr_onroad_byregions</span>
#&gt; └── <span style='color: #0000BB; font-weight: bold;'>2020NEI</span>
#&gt;     ├── <span style='color: #0000BB; font-weight: bold;'>2020NEI_onroad</span>
#&gt;     ├── <span style='color: #0000BB; font-weight: bold;'>2020NEI_spdist</span>
#&gt;     ├── <span style='color: #0000BB; font-weight: bold;'>2020nei_nonroad_byregion</span>
#&gt;     └── <span style='color: #0000BB; font-weight: bold;'>2020nei_onroad_byregion</span>
</CODE></PRE>

### nei

The NEI is available in multiple places at different aggregation levels

NEI should only be compared directly for years 2011 onward. Between 2008
and 2011, there were significant changes in the underlying source
classification codes (SCCs).

### air_emissions_modeling

EPA teams continually work on air emissions modeling. These emissions
modeling platforms support the development of emissions inputs for air
quality modeling supporting efforts across government agencies.
Platforms include everything from model-ready datasets for models like
CMAQ, MOVES, and SMOKE, scripts, output datasets, shapefiles, tables,
and documentation.

The organizing body for air emissions modeling is [Community Modeling
and Analysis System (CMAS)](https://www.cmascenter.org/). CMAS offers a
[discussion forum](https://forum.cmascenter.org/) for folks to
collaborate and ask questions. Staff scientists from the EPA, air
quality monitoring agencies, and other organizations answer questions
and respond to bug reports.

County-level data are only available at the continental US (CONUS)
level; these datasets include all counties in the US and are very large.

#### Air emissions modeling platforms

The data available for each platform year varies. Data is much more
consistently available and standardized from 2014 onward.

Downloads for thsee

#### EQUATES

> EPA scientists have developed a set of modeled meteorology, emissions,
> air quality and pollutant deposition spanning the years 2002 through
> 2019. Modeled datasets cover the Conterminous U.S. (CONUS) at a 12km
> horizontal grid spacing and the Northern Hemisphere at a 108km grid
> spacing using the Weather Research and Forecasting (WRF) model version
> 4.1.1 for simulating weather conditions and EPA’s Community Multiscale
> Air Quality (CMAQ) model version 5.3.2 for air quality modeling. New
> hemispheric and North American emissions inventories were developed
> using, to the extent possible, consistent input data and methods
> across all years, including emissions from motor vehicles, fires, and
> oil and gas sources. - [EPA website](https://www.epa.gov/cmaq/equates)