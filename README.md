# ACCREU Project

A comprehensive, reproducible suite of workflows for assessing flood exposure, land-use impacts, and crop-area, production, and revenue outcomes across multiple models, RCPs, and adaptation or mitigation strategies (snapshot years 2020 – 2050).

This repository comprises two independent but complementary modules. Each module has its own detailed README; see the directories below for in-depth descriptions, input requirements, and output formats.

## Repository Structure

```
ACCREU/
├── Flood/                  # Flood-Exposure & Land-Use Analysis (see Flood/README.md)
│   ├── Flooded_area/       # Tabular and raster outputs of flooded fractions
│   ├── loss_analysis/      # Production & revenue loss calculations and reports
│   ├── grass_data_analysis/# Grass-feed gap assessment outputs
│   └── R_scripts/    ACCREU Project

A comprehensive, reproducible suite of workflows for assessing flood exposure, land-use impacts, and crop-area, production, and revenue outcomes across multiple models, RCPs, and adaptation or mitigation strategies (snapshot years 2020 – 2050).

This repository comprises two independent but complementary modules. Each module has its own detailed README; see the directories below for in-depth descriptions, input requirements, and output formats.

Repository Structure

ACCREU/
├── Flood/                  # Flood-Exposure & Land-Use Analysis (see Flood/README.md)
│   ├── Flooded_area/       # Tabular and raster outputs of flooded fractions
│   ├── loss_analysis/      # Production & revenue loss calculations and reports
│   ├── grass_data_analysis/# Grass-feed gap assessment outputs
│   └── R_scripts/          # R scripts driving the flood & loss pipelines
│
├── CROP_ANALYSIS/          # Crop-Area, Production & Revenue Workflow (see CROP_ANALYSIS/README.md)
│   ├── crop_analysis.R     # R script to generate per-polygon crop metrics
│   ├── combined_crop_analysis_results.csv
│   ├── csv_analysis.csv    # Detailed per-scenario, per-polygon CSVs
│   └── plot_analysis.pdf   # Composite visual report (3 panels)
│
├── .gitignore              # Git ignore rules
└── README.md               # This overview file

Flood/: Quantifies flooded area by land-use class, computes economic and production losses for crops and grassland, and produces intermediate and final outputs (CSV, GeoTIFF, PDF).

CROP_ANALYSIS/: Processes land‐use scenario outputs into crop-area, production, and revenue metrics; exports per-scenario CSVs and a summary visual report.

Each module defines its own input paths and parameters at the top of its R scripts—adjust these to suit your directory layout. For detailed instructions on running the workflows and interpreting outputs, please consult the README.md in each top-level module directory.      # R scripts driving the flood & loss pipelines
│
├── CROP_ANALYSIS/          # Crop-Area, Production & Revenue Workflow (see CROP_ANALYSIS/README.md)
│   ├── crop_analysis.R     # R script to generate per-polygon crop metrics
│   ├── combined_crop_analysis_results.csv
│   ├── csv_analysis.csv    # Detailed per-scenario, per-polygon CSVs
│   └── plot_analysis.pdf   # Composite visual report (3 panels)
│
├── .gitignore              # Git ignore rules
└── README.md               # This overview file
```

- **Flood/**: Quantifies flooded area by land-use class, computes economic and production losses for crops and grassland, and produces intermediate and final outputs (CSV, GeoTIFF, PDF).
- **CROP\_ANALYSIS/**: Processes land‐use scenario outputs into crop-area, production, and revenue metrics; exports per-scenario CSVs and a summary visual report.

Each module defines its own input paths and parameters at the top of its R scripts—adjust these to suit your directory layout. For detailed instructions on running the workflows and interpreting outputs, please consult the `README.md` in each top-level module directory.


## Acknowledgement

This work received funding from the European Commission under the scope of the Assessing Climate Change Risk in Europe (ACCREU) project (grant agreement No: 101081358).

## Citation

If this workflow or its outputs help your research, please cite:

> **Nakhavali M.A., Greber G., ARBELAEZ-GAVIRIA J., Palazzo A., Wogerer N. (2025)**  
> *ACCREU Integrated Flood Exposure, Land-Use Impact, Crop & Grassland data*.  
> DOI: *tbd*



