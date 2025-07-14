# CROP_ANALYSIS — Crop-Area, Production & Revenue Workflow

This stand-alone R script turns every `luc.res` scenario file into a rich
per-polygon crop dataset (area → production → revenue) and a companion
visual report.  
Outputs are written to `./CROP_ANALYSIS/`.

---

## What the script does

| Step | Action |
|------|--------|
| 1 | **Read inputs**<br>• `luc.res` objects (`*.RData`) from `results_v04/`<br>• EU polygon grid with `SimUID` (`SimU_area_dissolved_CR_LUID.shp`)<br>• GLOBIOM price / yield / MCI table (`GLOBIOM_PRICE_YIELD_ACCREU.Rdata`) |
| 2 | **Centroid extraction** – converts every SimUID polygon to `(lat, lon)` |
| 3 | **luc.res processing**<br>• collapse `value` by LU class, year & SimUID<br>• derive land-use fraction → crop area (ha) |
| 4 | **Price / yield merge**<br>• attach crop-group map & production potential (`YLD × (1+MCI)`)<br>• calculate **total production** (t) & **revenue** (USD) |
| 5 | **Export per-scenario CSV** → `CROP_ANALYSIS/<basename>_crop_analysis.csv` |
| 6 | **Make a 3-panel PDF**<br>1. Heat-map of revenue by crop & year<br>2. Productivity bubble chart (area × production × revenue)<br>3. EU map of average crop area |
| 7 | **Combine all results** into `combined_crop_analysis_results.csv` |

Four snapshot years are analysed: 2020, 2030, 2040, 2050.

---

## Folder structure (output)

```

CROP\_ANALYSIS/
├── csv\_analysis.csv   # Per-polygon × crop metrics
├── plot\_analysis.pdf   # Visual summary (3 panels)
└── .\crop_analysis.R    # R script for processing 

````

---

## Required R packages

```r
install.packages(c(
  "dplyr","readr","ggplot2","patchwork","scales","tidyr",
  "sf","lwgeom","purrr","stringr",
  "rnaturalearth","rnaturalearthdata"
))
````

---

## Running the script

Simply `source()` the file; it auto-discovers every `*.RData` in
`./` and processes them sequentially.

```r
source("R_scripts/crop_analysis.R")   # adjust path if needed
```

Set‐and-forget parameters are defined at the top of the script:

| Variable      | Default                                                          | Meaning                              |
| ------------- | ---------------------------------------------------------------- | ------------------------------------ |
| `dir_results` | `H:/ACCREU/flood/results_v04/`                                 | Location of `*.RData` scenario files |
| `dir_price`   | `P:/bnr/.../GLOBIOM_PRICE_YIELD_ACCREU.Rdata`                    | Price / yield / MCI lookup           |
| `luc_id_shp`  | `H:/ACCREU/flood/SimU_CR_LUID/SimU_area_dissolved_CR_LUID.shp` | Polygon grid with `SimUID`           |
| `years`       | `c(2020,2030,2040,2050)`                                         | Snapshot years to keep               |

Change these paths if your directory layout differs.

---

## Output columns (CSV)

| Column               | Unit | Description                                     |
| -------------------- | ---- | ----------------------------------------------- |
| `SimUID`             | –    | Simulation unit ID                              |
| `lat`, `long`        | °    | Polygon centroid (WGS 84)                       |
| `year`               | –    | Snapshot year                                   |
| `scenario`, `model`  | text | Extracted from filename (`<model>_RCP...`)      |
| `crop_group`         | text | Harmonised crop code (e.g. `Corn`, `Whea_HI`)   |
| `total_area_ha`      | ha   | Crop area within polygon                        |
| `total_production_t` | t    | Harvestable production (`YLD × (1+MCI) × area`) |
| `total_revenue_usd`  | USD  | Production × price                              |

---

## Visual report

Each PDF merges three high-level views:

1. **Revenue heat-map** – crop × year (colour = M USD)
2. **Productivity scatter** – avg. area vs. production (bubble = revenue)
3. **EU distribution map** – point size = mean area, colour = crop group

These panels help to spot:

* Hot-spots of revenue risk
* Area–yield trade-offs between crop categories
* Spatial clusters of high-value or vulnerable crops

---

## Reproducibility notes

| Item           | Detail                                                                |
| -------------- | --------------------------------------------------------------------- |
| CRS            | All geometries re-projected internally to **EPSG 4326** for centroids |
| Crop mapping   | See `crop_map` tibble inside the script                               |
| Economic units | Prices in **constant 2020 USD**                                       |
| NA handling    | All missing numeric entries treated as 0                              |

---

## Citation

If this crop-analysis workflow aids your study, please cite:

> **Nakhavali M.A., Greber G., ARBELAEZ-GAVIRIA J., Palazzo A., Wogerer M. (2025)**  
> *EU Crop-Area, Production & Revenue Dataset*.  
>  DOI: 10.5281/zenodo.15878951

```
```
