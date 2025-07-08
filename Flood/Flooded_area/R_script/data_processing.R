# ───────────────────────────────────────────────────────────────────────────────
# Batch‐process multiple RData files containing luc.res for different scenarios.
# For each RData file, this script:
#   • Runs the flooded‐area extraction per Land Use types 
#   • Produces two CSV outputs named:
#       <original_basename>_flooded_detailed.csv
#       <original_basename>_flooded_aggregated.csv
# ───────────────────────────────────────────────────────────────────────────────

# 1) Load required packages 
# install.packages("terra")
# install.packages("sf")
# install.packages("exactextractr")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("readr")

library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(tidyr)
library(readr)

# ───────────────────────────────────────────────────────────────────────────────
# 2) Define constants: paths to data and common inputs
# ───────────────────────────────────────────────────────────────────────────────

# Directory containing the RData files:
rdata_dir <- "H:/ACCRELEU/flood/results_v04"

# Directory to write the CSV outputs 
output_dir <- "H:/ACCRELEU/flood/results_v04"

# Path to the flooded‐fraction raster 
flooded_rast_path <- "H:/ACCRELEU/flood/global_fracflooded_all_scenarios_years_res10000m_buffer10000m_watermask.tif"

# Path to the SimUID shapefile 
luc_id_shp <- "H:/ACCRELEU/flood/SimU_CR_LUID/SimU_area_dissolved_CR_LUID.shp"

# The years of interest and scenario string 
years    <- c(2020, 2030, 2040, 2050)
scenario <- "rcp126"

# ───────────────────────────────────────────────────────────────────────────────
# 3) Load common inputs that do not change per scenario
# ───────────────────────────────────────────────────────────────────────────────

# 3.1) Flooded‐fraction raster (contains all RCPs / years)
flooded_rast <- terra::rast(flooded_rast_path)

# 3.2) Read and reproject SimUID polygons to raster CRS (EPSG:3035)
luc_id <- sf::st_read(luc_id_shp, quiet = TRUE)
luc_id_3035 <- sf::st_transform(luc_id, crs(flooded_rast))
luc_id_3035$SimUID <- as.integer(luc_id_3035$SimUID)

# Compute polygon area (m² → km²) once, store in luc_id_3035
luc_id_3035 <- luc_id_3035 %>%
  mutate(
    poly_area_m2  = as.numeric(st_area(luc_id_3035)),
    poly_area_km2 = poly_area_m2 / 1e6
  )

# 3.3) Drop all “_2015” layers from flooded_rast; keep 2020–2100
all_layer_names <- names(flooded_rast)
idx_2015        <- grep("_2015$", all_layer_names)
flood_no2015    <- flooded_rast[[-idx_2015]]
# Now flood_no2015 has layers "rcp126_2020", "rcp126_2030", …, "rcp370_2100"

# 3.4) Compute raster cell area in m² and km² (constant across layers)
resolution_xy <- terra::res(flood_no2015)       # e.g. ~10000 × ~10000 (m)
cell_area_m2  <- abs(resolution_xy[1] * resolution_xy[2])  # ~1e8 m²
cell_area_km2 <- cell_area_m2 / 1e6                       # ~100 km²

# 3.5) Define LU‐to‐group mapping for aggregation (as per user specification)
lu_map <- tribble(
  ~lu.to,       ~lu_group,
  "NatLnd",     "NatLnd",
  "OagLnd",     "OagLnd",
  "GrsLnd",     "GrsLnd",
  "WetLnd",     "WetLnd",
  "PltFor",     "PltFor",
  "NotRel",     "NotRel",
  "prot.other", "prot.other",
  "Forest",     "Forest",
  "BeaD_HI",    "HI",      "SugC_HI",    "HI",
  "Whea_HI",    "HI",      "Pota_HI",    "HI",
  "Cott_HI",    "HI",      "Soya_HI",    "HI",
  "Rice_HI",    "HI",      "Gnut_HI",    "HI",
  "Srgh_HI",    "HI",      "SwPo_HI",    "HI",
  "Corn_HI",    "HI",      "Sunf_HI",    "HI",
  "ChkP_HI",    "HI",      "Barl_HI",    "HI",
  "Rape_HI",    "HI",
  "Corn_IR",    "IR",      "Pota_IR",    "IR",
  "Whea_IR",    "IR",      "Rice_IR",    "IR",
  "Sunf_IR",    "IR",      "Cott_IR",    "IR",
  "Soya_IR",    "IR",      "Srgh_IR",    "IR",
  "Rape_IR",    "IR",      "Barl_IR",    "IR",
  "ChkP_IR",    "IR",      "SwPo_IR",    "IR",
  "Whea_LI",    "LI",      "ChkP_LI",    "LI",
  "Srgh_LI",    "LI",      "Pota_LI",    "LI",
  "SugC_LI",    "LI",      "BeaD_LI",    "LI",
  "Cott_LI",    "LI",      "Soya_LI",    "LI",
  "Rice_LI",    "LI",      "Barl_LI",    "LI",
  "Corn_LI",    "LI",      "Sunf_LI",    "LI",
  "Mill_LI",    "LI",      "Gnut_LI",    "LI",
  "Rape_LI",    "LI",      "SwPo_LI",    "LI",
  "Corn_SS",    "SS",      "Rice_SS",    "SS",
  "Whea_SS",    "SS",      "Gnut_SS",    "SS",
  "Pota_SS",    "SS",      "Barl_SS",    "SS",
  "Srgh_SS",    "SS",      "BeaD_SS",    "SS",
  "Rape_SS",    "SS",      "Sunf_SS",    "SS",
  "ChkP_SS",    "SS"
)

# ───────────────────────────────────────────────────────────────────────────────
# 4) List all RData files and loop over them
# ───────────────────────────────────────────────────────────────────────────────

# 4.1) Get full paths to all .RData files in rdata_dir
rdata_files <- list.files(
  path = rdata_dir,
  pattern = "\\.RData$",
  full.names = TRUE
)

# 4.2) Loop over each RData file
for (rdata_path in rdata_files) {
  # 4.2.1) Derive a base name (without directory or ".RData")
  base_name <- tools::file_path_sans_ext(basename(rdata_path))
  
  # 4.2.2) Load the RData file: this should bring luc.res into the workspace
  load(rdata_path)  
  # (Assumes luc.res is now available in the global environment.)
  
  # 4.2.3) Build the “test” table of land‐use fractions for this scenario
  test <- luc.res %>%
    group_by(lu.to, ns, times) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    group_by(ns, times) %>%
    mutate(frac = value / sum(value)) %>%
    ungroup() %>%
    rename(SimUID = ns) %>%
    mutate(
      SimUID     = as.integer(SimUID),
      times      = as.integer(times),
      frac       = pmin(pmax(frac, 0), 1)  # clamp frac into [0,1]
    )
  
  # 4.2.4) Prepare a fresh copy of luc_id_3035 for this iteration 
  #        (so we can add new flooded columns without polluting next iteration)
  luc_id_iter <- luc_id_3035
  
  # 4.2.5) Initialize a list to store detailed per‐year results
  results_list <- list()
  
  # 4.2.6) Loop over each year and extract flooded metrics
  for (year_of_interest in years) {
    # Identify the flood raster layer name for this year (e.g., "rcp126_2020")
    pattern       <- paste0(scenario, "_", year_of_interest, "$")
    matched_layer <- grep(pattern, names(flood_no2015), value = TRUE)
    if (length(matched_layer) != 1) {
      warning("File ", base_name, ": skipping year ", year_of_interest, 
              " (layer ", pattern, " not found).")
      next
    }
    
    # Extract the single‐band flood raster for this year
    flood_r <- flood_no2015[[matched_layer]]
    
    # Compute area‐weighted mean flooded fraction per polygon
    flood_frac_mean_vec <- exact_extract(flood_r, luc_id_iter, "mean")
    flood_frac_mean_vec <- pmin(pmax(flood_frac_mean_vec, 0), 1)  # clamp [0,1]
    col_frac_name <- paste0("flooded_frac_", year_of_interest)
    luc_id_iter[[col_frac_name]] <- flood_frac_mean_vec
    
    # Compute flooded area (km²) per polygon
    flood_area_sum   <- exact_extract(flood_r, luc_id_iter, "sum")
    flood_area_sum   <- pmax(flood_area_sum, 0)  # clamp ≥ 0
    flooded_area_m2  <- flood_area_sum * cell_area_m2
    flooded_area_km2 <- flooded_area_m2 / 1e6
    col_area_name    <- paste0("flooded_area_km2_", year_of_interest)
    luc_id_iter[[col_area_name]] <- flooded_area_km2
    
    # Subset `test` to this year
    test_year <- test %>%
      filter(times == year_of_interest) %>%
      mutate(frac = pmin(pmax(frac, 0), 1))
    
    # Join polygon‐level metrics and polygon area to per‐LU table
    joined_df <- test_year %>%
      left_join(
        luc_id_iter %>%
          st_drop_geometry() %>%
          select(
            SimUID,
            poly_area_km2,
            !!col_frac_name,
            !!col_area_name
          ),
        by = "SimUID"
      ) %>%
      # Replace NA with zero
      mutate(
        !!col_frac_name := ifelse(is.na(.data[[col_frac_name]]), 0, .data[[col_frac_name]]),
        !!col_area_name := ifelse(is.na(.data[[col_area_name]]), 0, .data[[col_area_name]])
      )
    
    # Compute per‐land‐use metrics
    result_year <- joined_df %>%
      mutate(
        flooded_frac_by_lu       = .data[[col_frac_name]] * frac,
        flooded_frac_by_lu       = pmin(pmax(flooded_frac_by_lu, 0), 1),
        flooded_area_km2_by_lu   = .data[[col_area_name]] * frac,
        flooded_area_km2_by_lu   = pmax(flooded_area_km2_by_lu, 0),
        lu_area_km2_polygon      = frac * poly_area_km2,
        lu_value                 = value,
        frac_value               = frac,
        scenario                 = scenario
      ) %>%
      select(
        SimUID,
        times,
        scenario,
        lu.to,
        lu_value,
        frac_value,
        poly_area_km2,
        !!col_frac_name,
        !!col_area_name,
        flooded_frac_by_lu,
        flooded_area_km2_by_lu,
        lu_area_km2_polygon
      ) %>%
      rename_with(
        ~ gsub(pattern = paste0("_", year_of_interest), replacement = "", x = .x),
        matches(paste0(col_frac_name, "|", col_area_name))
      )
    # Now columns “flooded_frac” and “flooded_area_km2” refer to that year
    
    # Store this year’s detailed results
    results_list[[as.character(year_of_interest)]] <- result_year
  }
  
  # 4.2.7) Combine all years into one detailed data frame
  detailed_results <- bind_rows(results_list, .id = "year") %>%
    mutate(year = as.integer(year))
  
  # 4.2.8) Create aggregated results per SimUID and LU group
  aggregated_results <- detailed_results %>%
    left_join(lu_map, by = "lu.to") %>%
    mutate(lu_group = ifelse(is.na(lu_group), "OTHER", lu_group)) %>%
    group_by(SimUID, year, scenario, lu_group) %>%
    summarise(
      total_lu_value            = sum(lu_value, na.rm = TRUE),
      total_frac_value          = sum(frac_value, na.rm = TRUE),
      total_flooded_area_km2    = sum(flooded_area_km2_by_lu, na.rm = TRUE),
      total_lu_area_km2_polygon = sum(lu_area_km2_polygon, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      flooded_frac_group = ifelse(
        total_lu_area_km2_polygon > 0,
        total_flooded_area_km2 / total_lu_area_km2_polygon,
        0
      )
    ) %>%
    select(
      SimUID,
      year,
      scenario,
      lu_group,
      total_lu_value,
      total_frac_value,
      flooded_frac_group,
      total_flooded_area_km2,
      total_lu_area_km2_polygon
    ) %>%
    rename(
      lu_value            = total_lu_value,
      frac_value          = total_frac_value,
      flooded_area_km2    = total_flooded_area_km2,
      lu_area_km2_polygon = total_lu_area_km2_polygon
    )
  
  # 4.2.9) Write the detailed and aggregated CSVs with matching base names
  detailed_out_path   <- file.path(output_dir, paste0(base_name, "_flooded_detailed.csv"))
  aggregated_out_path <- file.path(output_dir, paste0(base_name, "_flooded_aggregated.csv"))
  
  write_csv(detailed_results, detailed_out_path)
  write_csv(aggregated_results, aggregated_out_path)
  
  message("Processed ", basename(rdata_path), 
          " → Detailed: ", basename(detailed_out_path),
          " | Aggregated: ", basename(aggregated_out_path))
}

# ───────────────────────────────────────────────────────────────────────────────
# End of batch script.
# For each RData file in results_v04, two CSVs are created:
#   <basename>_flooded_detailed.csv
#   <basename>_flooded_aggregated.csv
# ───────────────────────────────────────────────────────────────────────────────
