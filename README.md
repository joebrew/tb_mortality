
# Comparing TB mortality sources: IHME vs. WHO

## Data sources

There are 4 direct data sources used for this project:

1. IHME data: 
    - `data/IHME DATASET 24112016.csv`

2. WHO data: 
    - `data/File a excel WHO_pg.xlsx`
    - `data/File b excel WHO_pg.xlsx`
    - `data/File c excel WHO_pg.xlsx`
    - `data/File d excel WHO_pg.xlsx`

3. Global burden of disease data:
    - `data/Global burden public excel WHO.xlsx`

4. Population data:
    - `data/Population Estimates IHME_2015.xlsx`

Additionally, for visualizations, an ESRI-format global administrative boundaries shapefile is used:
    - `shp/world/ne_110m_admin_0_countries.shp`

This wasretrieved from the "Natural Earth" database, administrative level 0, country boundaries: http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip

## Combining data

To combine the different direct sources into one "wide" dataset, run `make_wide.R`. This generated `combined_data.csv`.

## Questions

For questions on this project, email joe@economicsofmalaria.com or alberto@basteiro.com.