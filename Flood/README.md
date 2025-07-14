# Flood-Exposure & Land-Use Analysis

This repository holds the complete, reproducible workflow used to 

1. Quantify flooded area per land-use class for multiple models, RCPs, and adaptation / mitigation strategies (snapshot years 2020 – 2050)  
2. Derive economic and production losses from flooding for crops & grassland  
3. Aggregate, visualise and export** all intermediate and final products (CSV, GeoTIFF, PDF)

> Key features
> • loss_analysis pipeline – converts flooded area to production / revenue losses  
> • grass_data_analysis pipeline – links flooding to grass-feed gaps  
> • Extract mean flooded fraction for every simulation unit 
> • Re‑weight by land‑use fractions to obtain flooded area per LU class  
> • Save tabular outputs → build raster stacks → generate bar‑plots  
---

## Folder structure

```text
.
├── Flooded_area
│   ├── csv                 # Tabular outputs created by the scripts
│   │   ├── flooded_detailed.csv     # Per-polygon × LU × year metrics
│   │   └── flooded_aggregated.csv   # Per-polygon × LU-group × year summaries
│   ├── raster              # GeoTIFF stacks of flooded-LU fractions
│   │   └── allYears_floodedLuFrac.tif
│   ├── R_scripts           # Processing scripts
│   │   ├── 01_batch_process_luc.R
│   │   ├── 02_build_raster_stacks.R
│   │   ├── 03_generate_plots.R
│   │   ├── 04_loss_analysis.R
│   │   └── 05_grass_data_analysis.R
│   └── plot                # Publication-ready PDFs
│       ├── <model>_<RCP>_plot.pdf     # Faceted by adaptation/mitigation
│       └── <model>_ALL_RCPs_plot.pdf  # Grid of all RCPs × strategies
├── loss_analysis           # Economic-loss workflow outputs
│   ├── detailed_losses_*.csv
│   ├── aggregated_losses_*.csv
│   └── flood_impact_*.pdf
├── grass_data_analysis     # Grassland-gap workflow outputs
│   ├── csv
│   │   └── <label>_results.csv
│   ├── plots
│   │   └── <label>_analysis.pdf
│   └── all_scenarios_combined.csv
└── README.md               # You are here

````

---

## 1 Land-use flood processor `01_batch_process_luc.R`

**Inputs**

* `luc.res` objects (`*.RData`)  
* 10 km flood cube (`global_fracflooded_all_scenarios_years_res10000m_buffer10000m_watermask.tif`)  
* `SimU_area_dissolved_CR_LUID.shp` polygons with unique **SimUID**

**Steps**

1. Strip 2015 bands → keep years 2020 – 2100  
2. Pre-compute polygon and cell areas  
3. Loop through every scenario file × four snapshot years (2020/30/40/50)  
4. For each polygon  
   * mean flooded fraction  
   * flooded area (km² & ha)  
   * flooded area by LU class (via `luc.res` fractions)  
5. Write two CSVs per scenario file  
   * `*_flooded_detailed.csv` (every LU class)  
   * `*_flooded_aggregated.csv` (user-defined LU groups)

Outputs land in **csv/**.

---

## 2 Raster-stack builder `02_build_raster_stacks.R`

Transforms aggregated CSVs into multi-band GeoTIFFs.

1. Rasterise **SimUID** polygons once (EPSG: 3035, 10 km)  
2. For each aggregated CSV, write flooded-fraction layers (LU-group × year)  
3. Stack → `raster/<basename>_allYears_floodedLuFrac.tif`

---

## 3 Plot generator `03_generate_plots.R`

Creates publication-ready bar plots of flooded area (km²).

* Facetted PDF per model & RCP  
* Grid PDF of all RCPs × strategies per model  
* Saves to **plot/**

---

## 4 Loss-analysis pipeline `04_loss_analysis.R`

**Purpose** – convert flooded area into **production- and revenue-loss** indicators for **all crops & forest categories**.

| Step | Action |
|------|--------|
| 1 | Load every `_flooded_detailed/aggregated.csv` |
| 2 | Merge with price / yield / MCI data (`GLOBIOM_PRICE_YIELD_ACCREU.Rdata`) |
| 3 | Compute per-polygon: <br>• flooded ha • production loss (t) • revenue loss (USD) <br>• baseline production & revenue (no SLR) |
| 4 | Export **detailed & aggregated** loss CSVs to **loss_analysis/** |
| 5 | Auto-generate 4-panel PDFs: flooded area, production loss, revenue loss, flooded-fraction |

**Outputs**

* `loss_analysis/detailed_losses_*.csv`  
* `loss_analysis/aggregated_losses_*.csv`  
* `loss_analysis/flood_impact_*.pdf`

---

## 5 Grass-data pipeline `05_grass_data_analysis.R`

Quantifies grass-feed production gaps caused by flooding.

| Step | Action |
|------|--------|
| 1 | Read each `_flooded_detailed.csv` (filter GrsLnd) |
| 2 | Join with grass-requirement / yield dataset (`GLOBIOM_GRASS_REQ_PROD_ACCREU.Rdata`) |
| 3 | Compute per-polygon, per-year metrics: <br>• total grass production <br>• lost production (SLR) <br>• production gap (= requirement – available) <br>• risk categories |
| 4 | Save one CSV & one PDF per scenario, plus a master combined CSV |

**Outputs**

* `grass_data_analysis/csv/*_results.csv`  
* `grass_data_analysis/plots/(_analysis.pdf`  
* `grass_data_analysis/all_scenarios_combined.csv`

---

## Running the full workflow

```r
# 0. (One-time) install packages
install.packages(c(
  "terra","sf","exactextractr","dplyr","tidyr","readr","data.table",
  "ggplot2","forcats","viridis","scales","patchwork","purrr","stringr"
))

# 1-3. Core flooded-area pipeline
source("R_scripts/01_batch_process_luc.R")
source("R_scripts/02_build_raster_stacks.R")
source("R_scripts/03_generate_plots.R")

# 4. Production & revenue-loss analysis
source("R_scripts/04_loss_analysis.R")

# 5. Grass-feed gap analysis
source("R_scripts/05_grass_data_analysis.R")
````

> **Paths:** each script defines its own `dir_*` and `*_path` variables at the top.
> Edit these if your directory layout differs.

---

## Reproducibility notes

| Item             | Detail                            |
| ---------------- | --------------------------------- |
| CRS              | **EPSG: 3035** (ETRS-LAEA)        |
| Grid resolution  | 10 km × 10 km (100 km²)           |
| Snapshot years   | 2020, 2030, 2040, 2050            |
| Scenario tags    | Mitigation scenario – Model – RCP |
| Memory footprint | EU 10 km raster ≈ 1–2 GB RAM      |
| Economic units   | Prices in constant **2020 USD**   |

---
## Acknowledgement

This work received funding from the European Commission under the scope of the Assessing Climate Change Risk in Europe (ACCREU) project (grant agreement No: 101081358).

## Citation

If this workflow or its outputs help your research, please cite:

> **Nakhavali M.A., Greber G., ARBELAEZ-GAVIRIA J., Palazzo A., Wogerer N. (2025)**  
> *Flood-Exposure & Land-Use Analysis Workflow*.  
>  DOI: 10.5281/zenodo.15878951


```
```
