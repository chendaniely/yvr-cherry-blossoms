.PHONY: all data results routes clean

# Run the full pipeline
all: data results routes

# ── 1. Data pipeline ──────────────────────────────────────────────────────────

data: \
	data/final/streets.geojson \
	data/final/cherry_blossoms.geojson \
	data/final/snapped-tree-street.geojson \
	data/processed/streets_counted.geojson \
	data/processed/top30_streets.geojson

data/final/streets.geojson: \
		code/01-clean/streets.R \
		data/original/public-streets.geojson \
		data/original/non-city-streets.geojson \
		data/original/lanes.geojson
	Rscript code/01-clean/streets.R

data/final/cherry_blossoms.geojson: \
		code/01-clean/cherry_blossoms.R \
		data/original/public-trees.geojson
	Rscript code/01-clean/cherry_blossoms.R

data/final/snapped-tree-street.geojson \
data/processed/nearest_idx.rds \
data/processed/snapped.geojson: \
		code/02-street_tree/snap_trees_to_streets.R \
		data/final/streets.geojson \
		data/final/cherry_blossoms.geojson
	Rscript code/02-street_tree/snap_trees_to_streets.R

data/processed/streets_counted.geojson \
data/processed/street_summary.csv: \
		code/02-street_tree/street_analysis.R \
		data/final/snapped-tree-street.geojson \
		data/final/streets.geojson
	Rscript code/02-street_tree/street_analysis.R

# street_results render also writes top30_streets.geojson as a side effect
data/processed/top30_streets.geojson: results/street_results/README.html

# ── 2. Results ────────────────────────────────────────────────────────────────

results: \
	results/neighbourhood_choropleth/README.html \
	results/street_results/README.html \
	results/tsp_route/README.html \
	results/google_route/README.html

results/neighbourhood_choropleth/README.html: \
		results/neighbourhood_choropleth/README.qmd \
		data/final/cherry_blossoms.geojson \
		data/original/local-area-boundary.geojson
	quarto render results/neighbourhood_choropleth/README.qmd

results/street_results/README.html: \
		results/street_results/README.qmd \
		data/processed/streets_counted.geojson \
		data/processed/street_summary.csv
	quarto render results/street_results/README.qmd

results/tsp_route/README.html: \
		results/tsp_route/README.qmd \
		data/processed/top30_streets.geojson \
		data/final/streets.geojson
	quarto render results/tsp_route/README.qmd

results/google_route/README.html: \
		results/google_route/README.qmd \
		data/processed/top30_streets.geojson
	quarto render results/google_route/README.qmd

# ── 3. GPX routes ─────────────────────────────────────────────────────────────

routes: \
	data/routes/cherry-blossom-top10-run.gpx \
	data/routes/cherry-blossom-top10-cycling.gpx \
	data/routes/cherry-blossom-run.gpx \
	data/routes/cherry-blossom-bike.gpx

data/routes/cherry-blossom-top10-run.gpx \
data/routes/cherry-blossom-top10-cycling.gpx \
data/routes/cherry-blossom-run.gpx \
data/routes/cherry-blossom-bike.gpx: \
		code/03-routes/gpx_export.R \
		data/processed/top30_streets.geojson
	Rscript code/03-routes/gpx_export.R

# ── Helpers ───────────────────────────────────────────────────────────────────

# Remove all generated files (keeps data/original untouched)
clean:
	rm -f data/final/*.geojson
	rm -f data/processed/*.geojson data/processed/*.csv data/processed/*.rds
	rm -f data/routes/*.gpx
	rm -f results/neighbourhood_choropleth/README.html
	rm -f results/street_results/README.html
	rm -f results/tsp_route/README.html
	rm -f results/google_route/README.html
