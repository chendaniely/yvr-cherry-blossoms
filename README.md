# yvr-cherry-blossoms

Analysis of Japanese flowering cherry trees across Vancouver's street network, using the City of Vancouver Open Data portal.

**Data sources**
- [Public Trees](https://opendata.vancouver.ca/explore/dataset/public-trees/information/) — tree inventory with species, location, and dimensions
- [Public Streets](https://opendata.vancouver.ca/explore/dataset/public-streets/information/) — city-maintained street segments
- [Non-City Streets](https://opendata.vancouver.ca/explore/dataset/non-city-streets/information/) — privately-maintained streets
- [Lanes](https://opendata.vancouver.ca/explore/dataset/lanes/information/) — lane segments
- [Local Area Boundary](https://opendata.vancouver.ca/explore/dataset/local-area-boundary/information/) — neighbourhood polygons

## Installation

Install R packages from CRAN:

```r
install.packages(c(
  "sf",
  "dplyr",
  "tidyr",
  "mapgl",
  "DT",
  "here",
  "httr",
  "xml2",
  "sfnetworks",
  "igraph",
  "tidygraph"
))
```

Install packages from R-universe:

```r
install.packages(
  "spopt",
  repos = c("https://walkerke.r-universe.dev", "https://cloud.r-project.org")
)
```

Install [Quarto](https://quarto.org/docs/get-started/) to render the results pages.

## Usage

Run the full pipeline with:

```bash
make
```

Or run individual stages:

```bash
make data      # clean raw data → data/final/ and data/processed/
make results   # render HTML results → results/*/README.html
make routes    # generate GPX files → data/routes/
```

To rebuild everything from scratch:

```bash
make clean && make
```

## Project structure

```
data/
  original/   raw data from Vancouver Open Data (not modified)
  final/      cleaned, analysis-ready files
  processed/  intermediate files produced during analysis
  routes/     GPX route files for running and cycling

code/
  01-clean/           data cleaning scripts
  02-street_tree/     snapping trees to streets and street-level analysis
  03-routes/          GPX export

results/
  neighbourhood_choropleth/   cherry blossoms by neighbourhood
  street_results/             top streets by count and density
  tsp_route/                  optimized viewing route via road network
  google_route/               Google Maps links and route tables
```
