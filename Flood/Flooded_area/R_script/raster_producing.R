# ───────────────────────────────────────────────────────────────────────────────
# Batch‐build multi‐year “flooded‐LU fraction” stacks from each aggregated CSV.
# Each output GeoTIFF (<basename>_allYears_floodedLuFrac.tif) contains one band
# ───────────────────────────────────────────────────────────────────────────────

# 0) Install required packages if not already:
# install.packages("data.table")
# install.packages("terra")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("sf")

library(data.table)   # for fread
library(terra)        # for raster operations
library(dplyr)        # for data manipulation
library(tidyr)        # for pivot_wider
library(sf)           # for vector operations

# 1) Define paths and constants
csv_dir         <- "H:/ACCRELEU/flood/results_v04"
flooded_tif     <- "H:/ACCRELEU/flood/global_fracflooded_all_scenarios_years_res10000m_buffer10000m_watermask.tif"
luc_id_shp      <- "H:/ACCRELEU/flood/SimU_CR_LUID/SimU_area_dissolved_CR_LUID.shp"

# 2) Load flooded‐fraction raster (all scenarios/years) as SpatRaster
flooded_rast <- terra::rast(flooded_tif)

# 3) Drop all “_2015” bands so that we keep only 2020–2100
layer_names  <- names(flooded_rast)
keep_idx     <- which(!grepl("_2015$", layer_names))
flood_no2015 <- flooded_rast[[keep_idx]]

# 4) Use the first layer of flood_no2015 as template for extent/CRS/res
template_candidate <- flood_no2015[[1]]
if (!inherits(template_candidate, "SpatRaster")) {
  template_rast <- rast(template_candidate)
} else {
  template_rast <- template_candidate
}

# 5) Read SimUID shapefile; if missing CRS, assume WGS84 (EPSG:4326)
luc_id <- sf::st_read(luc_id_shp, quiet = TRUE)
if (is.na(sf::st_crs(luc_id))) {
  sf::st_crs(luc_id) <- 4326
}

# 6) Transform SimUID polygons to match raster CRS (EPSG:3035)
luc_id_3035    <- sf::st_transform(luc_id, 3035)
luc_id_3035$SimUID <- as.integer(luc_id_3035$SimUID)

# 7) Rasterize SimUID polygons onto the template grid
simuid_rast <- terra::rasterize(
  x     = terra::vect(luc_id_3035),
  y     = template_rast,
  field = "SimUID",
  fun   = "sum"
)

# 8) List all aggregated CSVs (ending with "_flooded_aggregated.csv")
agg_files <- list.files(
  path = csv_dir,
  pattern = "_flooded_aggregated\\.csv$",
  full.names = TRUE
)

# 9) Loop over each aggregated CSV to build one combined multi‐band stack
for (csv_path in agg_files) {
  base_name <- tools::file_path_sans_ext(basename(csv_path))
  message("Processing ", base_name)
  
  # 9.1) Read the aggregated CSV quickly with fread
  df_agg <- fread(csv_path)
  
  # 9.2) Identify LU columns (all except SimUID and year)
  lu_cols <- setdiff(names(df_agg), c("SimUID", "year"))
  
  # 9.3) Unique years present in this CSV
  years <- sort(unique(df_agg$year))
  
  # 9.4) Prepare a list to hold every single raster layer
  all_layers <- list()
  
  # 9.5) Loop over each year & each LU to classify simuid_rast
  for (yr in years) {
    df_year <- df_agg[year == yr]
    if (nrow(df_year) == 0) next
    
    for (lu_name in lu_cols) {
      # 9.5.1) Build two‐column matrix (SimUID, flooded_frac_group)
      mat <- as.matrix(df_year[, .(SimUID, get(lu_name))])
      colnames(mat) <- c("SimUID", "frac")
      
      # 9.5.2) Classify simuid_rast → raster of flooded fraction for this LU/year
      r_lu <- terra::classify(simuid_rast, rcl = mat, others = 0)
      
      # 9.5.3) Name the layer "<year>_<LUname>"
      names(r_lu) <- paste0(yr, "_", lu_name)
      
      # 9.5.4) Append to list
      all_layers[[ length(all_layers) + 1 ]] <- r_lu
    }
  }
  
  # 10) Combine all individual layers into one SpatRaster (multi‐band)
  combined_stack <- terra::rast(all_layers)
  
  # 11) Write a single GeoTIFF with all bands
  out_tif <- file.path(csv_dir, paste0(base_name, "_allYears_floodedLuFrac.tif"))
  if (!file.exists(out_tif)) {
    terra::writeRaster(
      combined_stack,
      out_tif,
      overwrite = FALSE
    )
    message("Wrote combined stack: ", basename(out_tif))
  } else {
    message("Skipping existing: ", basename(out_tif))
  }
}

# ───────────────────────────────────────────────────────────────────────────────
# End of script.
# Outputs per aggregated CSV:
#   • "<basename>_allYears_floodedLuFrac.tif" containing one band per <year>_<LU>.
# ───────────────────────────────────────────────────────────────────────────────
