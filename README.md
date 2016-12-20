
# Comparing TB mortality sources: IHME vs. WHO

## Data sources

There are 4 direct data sources used for this project:

1. IHME data: 
    - `data/IHME DATASET 24112016.csv`
      -Can be downloaded from http://ghdx.healthdata.org/gbd-results-tool
      -Select the specific variables of interest. 
      -The download corresponds to 24-11-2016

2. WHO data: 
    - `data/File a excel WHO_pg.xlsx`
    - `data/File b excel WHO_pg.xlsx`
    - `data/File c excel WHO_pg.xlsx`
    - `data/File d excel WHO_pg.xlsx`
  - All of the above were sent by email to Alberto Garcia-Basteiro by Philippe Glaziou (WHO) on 23-11-2016 (Mario Raviglione, Frank Cobelens, Babis Sismanidis (WHO officers) copied in the email.
    - `data/TB_notifications_2016-12-20.csv`
    - `data/MDR_RR_TB_burden_estimates_2016-12-20.csv` 
  - The above were downloaded from  http://www.who.int/tb/country/data/download/en/ on December 20, 2016

3. Global burden of disease data:
    - `data/Global burden public excel WHO.xlsx`
  - From http://www.who.int/tb/country/data/download/en/
  - Downloaded on the 24-11-2016

4. Population data:
    - `data/Population Estimates IHME_2015.xlsx`
  - From http://ghdx.healthdata.org/record/global-burden-disease-study-2015-gbd-2015-population-estimates-1970-2015
  -Downloaded on the 24-11-2016

Additionally, for visualizations, an ESRI-format global administrative boundaries shapefile is used:
    - `shp/world/ne_110m_admin_0_countries.shp`

This wasretrieved from the "Natural Earth" database, administrative level 0, country boundaries: http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip

## Combining data

To combine the different direct sources into one "wide" dataset, run `make_wide.R`. This generated `combined_data.csv`.

## Questions

For questions on this project, email joe@economicsofmalaria.com or alberto@basteiro.com.