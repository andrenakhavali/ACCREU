# 
# CROP ANALYSIS 
# 
# Load required packages
library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)
library(scales)
library(tidyr)
library(sf)
library(lwgeom)
library(purrr)
library(stringr)
library(rnaturalearth)
library(rnaturalearthdata)

# Set paths and constants
dir_results <- "H:/ACCRELEU/flood/results_v04/"
dir_price <- "P:/bnr/03_Projects/ACCREU/WP2/GLOBIOM/GLOBIOM_PRICE_YIELD_ACCREU.Rdata"
dir_out <- file.path(dir_results, "CROP_ANALYSIS")
luc_id_shp <- "H:/ACCRELEU/flood/SimU_CR_LUID/SimU_area_dissolved_CR_LUID.shp"
years <- c(2020, 2030, 2040, 2050)


# Define complete crop grouping 
crop_map <- tibble::tribble(
  ~DSName,       ~crop_group,
  "NatLnd",     "NatLnd",
  "OagLnd",     "OagLnd",
  "GrsLnd",     "GrsLnd",
  "WetLnd",     "WetLnd",
  "PltFor",     "PltFor",
  "NotRel",     "NotRel",
  "prot.other", "prot.other",
  "Forest",     "Forest",
  "BeaD_HI",    "BeaD", "BeaD_LI", "BeaD", "BeaD_SS", "BeaD",
  "SugC_HI",    "SugC", "SugC_LI", "SugC",
  "Whea_HI",    "Whea", "Whea_LI", "Whea", "Whea_SS", "Whea", "Whea_IR", "Whea",
  "Pota_HI",    "Pota", "Pota_LI", "Pota", "Pota_SS", "Pota", "Pota_IR", "Pota",
  "Cott_HI",    "Cott", "Cott_LI", "Cott", "Cott_IR", "Cott",
  "Soya_HI",    "Soya", "Soya_LI", "Soya", "Soya_IR", "Soya",
  "Rice_HI",    "Rice", "Rice_LI", "Rice", "Rice_SS", "Rice", "Rice_IR", "Rice",
  "Gnut_HI",    "Gnut", "Gnut_LI", "Gnut", "Gnut_SS", "Gnut",
  "Srgh_HI",    "Srgh", "Srgh_LI", "Srgh", "Srgh_SS", "Srgh", "Srgh_IR", "Srgh",
  "SwPo_HI",    "SwPo", "SwPo_LI", "SwPo", "SwPo_IR", "SwPo",
  "Corn_HI",    "Corn", "Corn_LI", "Corn", "Corn_SS", "Corn", "Corn_IR", "Corn",
  "Sunf_HI",    "Sunf", "Sunf_LI", "Sunf", "Sunf_SS", "Sunf", "Sunf_IR", "Sunf",
  "ChkP_HI",    "ChkP", "ChkP_LI", "ChkP", "ChkP_SS", "ChkP", "ChkP_IR", "ChkP",
  "Barl_HI",    "Barl", "Barl_LI", "Barl", "Barl_SS", "Barl", "Barl_IR", "Barl",
  "Rape_HI",    "Rape", "Rape_LI", "Rape", "Rape_SS", "Rape", "Rape_IR", "Rape",
  "Mill_LI",    "Mill"
) %>% 
  distinct(DSName, .keep_all = TRUE) %>%
  mutate(crop_group = ifelse(is.na(crop_group), "Other", crop_group))

# Extract EXACT original filename components
extract_metadata <- function(filename) {
  basename <- tools::file_path_sans_ext(basename(filename))
  
  list(
    full_name = basename,
    model = str_extract(basename, "(?<=_)[^_]+(?=_RCP)"),
    scenario = str_extract(basename, "RCP[0-9]p[0-9]")
  )
}

# Load and process SimUID shapefile
process_shapefile <- function(shp_path) {
  shp <- st_read(shp_path, quiet = TRUE) %>%
    st_transform(4326) %>%
    st_make_valid() %>%
    select(SimUID) %>%
    mutate(
      SimUID = as.integer(SimUID),
      centroid = tryCatch(
        st_centroid(geometry),
        error = function(e) {
          message("Error for SimUID: ", SimUID)
          st_point(c(NA, NA))
        }
      )
    ) %>%
    filter(!st_is_empty(centroid)) %>%
    mutate(
      coords = st_transform(st_sfc(centroid), 4326),
      long = st_coordinates(coords)[,1],
      lat = st_coordinates(coords)[,2],
      long = ifelse(between(long, -180, 180), long, NA),
      lat = ifelse(between(lat, -90, 90), lat, NA)
    ) %>%
    filter(!is.na(long), !is.na(lat)) %>%
    st_drop_geometry() %>%
    select(SimUID, lat, long)
  
  return(shp)
}

simu_shp <- process_shapefile(luc_id_shp)

# Load price/yield data 
price_data <- local({
  load(dir_price)
  
  PRICE_YLD_MCI %>%
    rename(scenario = Scen3, simuid = simuid) %>%
    mutate(
      simuid = as.integer(as.character(simuid)),
      scenario = tolower(scenario),
      scenario = case_when(
        scenario == "rcp2p6" ~ "rcp26",
        scenario == "rcp4p5" ~ "rcp45",
        scenario == "rcp6p0" ~ "rcp60",
        scenario == "rcp8p5" ~ "rcp85",
        TRUE ~ scenario
      ),
      across(c(YLD, MCI, PRICE), ~ifelse(is.na(.), 0, .))
    ) %>%
    left_join(crop_map, by = "DSName") %>%
    mutate(
      production_t_ha = YLD * (1 + MCI),
      crop_group = ifelse(is.na(crop_group), "Other", crop_group)
    )
})

# Process RData file 
process_rdata_file <- function(rdata_path, simu_data, price_data, shp_path) {
  meta <- extract_metadata(rdata_path)
  
  message("\nProcessing: ", meta$full_name)
  message("Model: ", meta$model)
  message("Scenario: ", meta$scenario)
  
  env <- new.env()
  load(rdata_path, envir = env)
  
  if (!exists("luc.res", envir = env)) {
    message("Error: 'luc.res' not found")
    return(NULL)
  }
  
  # Convert to data frame with NA handling
  luc.res <- as.data.frame(env$luc.res) %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))
  
  # Ensure required columns exist
  required_cols <- c("lu.to", "ns", "times", "value")
  missing_cols <- setdiff(required_cols, names(luc.res))
  if (length(missing_cols) > 0) {
    message("Missing columns: ", paste(missing_cols, collapse = ", "))
    return(NULL)
  }
  
  # Processing pipeline
  crop_data <- luc.res %>%
    group_by(lu.to, ns, times) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    group_by(ns, times) %>%
    mutate(frac = value / sum(value)) %>%
    ungroup() %>%
    rename(SimUID = ns) %>%
    mutate(
      SimUID = as.integer(SimUID),
      times = as.integer(times),
      frac = pmin(pmax(frac, 0), 1)
    ) %>%
    left_join(
      st_read(shp_path, quiet = TRUE) %>%
        st_make_valid() %>%
        mutate(
          SimUID = as.integer(SimUID),
          poly_area_km2 = as.numeric(st_area(geometry)) / 1e6
        ) %>%
        st_drop_geometry() %>%
        select(SimUID, poly_area_km2),
      by = "SimUID"
    ) %>%
    mutate(
      poly_area_km2 = ifelse(is.na(poly_area_km2), 0, poly_area_km2),
      crop_area_ha = frac * poly_area_km2 * 100
    ) %>%
    left_join(simu_data, by = "SimUID") %>%
    left_join(
      price_data,
      by = c("lu.to" = "DSName", "SimUID" = "simuid", "times" = "YEAR")
    ) %>%
    mutate(
      across(c(YLD, MCI, PRICE, production_t_ha), ~ifelse(is.na(.), 0, .)),
      total_production_t = crop_area_ha * production_t_ha,
      total_revenue_usd = total_production_t * PRICE,
      scenario = meta$scenario,
      model = meta$model,
      crop_group = ifelse(is.na(crop_group), "Other", crop_group)
    ) %>%
    filter(times %in% years) %>%
    group_by(SimUID, lat, long, times, scenario, model, crop_group) %>%
    summarise(
      total_area_ha = sum(crop_area_ha, na.rm = TRUE),
      total_production_t = sum(total_production_t, na.rm = TRUE),
      total_revenue_usd = sum(total_revenue_usd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(year = times)
  
  # Save with original naming
  output_name <- paste0(meta$full_name, "_crop_analysis.csv")
  write_csv(crop_data, file.path(dir_out, output_name))
  
  # Generate visualizations
  create_visualizations(crop_data, meta$full_name)
  
  return(crop_data)
}

# VISUALIZATION FUNCTION 

create_visualizations <- function(crop_data, original_name) {
  # 1. DATA PREPARATION ----
  # Ensure we have data to plot
  if (nrow(crop_data) == 0) {
    message("No data available for visualizations")
    return(NULL)
  }
  
  # Prepare plot data with proper aggregation
  plot_data <- crop_data %>%
    filter(!is.na(crop_group)) %>%
    group_by(year, crop_group, lat, long) %>%
    summarise(
      area = sum(total_area_ha, na.rm = TRUE),
      production = sum(total_production_t, na.rm = TRUE),
      revenue = sum(total_revenue_usd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      revenue_millions = revenue / 1e6,
      crop_group = factor(crop_group),
      # Create broader categories for analysis
      crop_category = case_when(
        str_detect(crop_group, "_HI$") ~ "High Intensity",
        str_detect(crop_group, "_LI$") ~ "Low Intensity",
        str_detect(crop_group, "_IR$") ~ "Irrigated",
        str_detect(crop_group, "_SS$") ~ "Subsistence",
        crop_group %in% c("NatLnd", "OagLnd", "GrsLnd", "WetLnd", "PltFor", "Forest") ~ "Natural",
        TRUE ~ "Other"
      )
    )
  
  # Get top 10 crops by average area
  top_crops <- plot_data %>%
    group_by(crop_group) %>%
    summarise(avg_area = mean(area)) %>%
    arrange(desc(avg_area)) %>%
    slice_head(n = 10) %>%
    pull(crop_group)
  
  # Create color palettes
  category_colors <- c(
    "High Intensity" = "#e41a1c",
    "Low Intensity" = "#377eb8",
    "Irrigated" = "#4daf4a",
    "Subsistence" = "#984ea3",
    "Natural" = "#ff7f00",
    "Other" = "#a65628"
  )
  
  crop_colors <- scales::hue_pal()(length(unique(plot_data$crop_group)))
  names(crop_colors) <- unique(plot_data$crop_group)
  # 4. PLOT 3: REVENUE HEATMAP ----
  p3 <- plot_data %>%
    group_by(year, crop_group) %>%
    summarise(revenue = sum(revenue_millions), .groups = "drop") %>%
    ggplot(aes(x = year, y = crop_group, fill = revenue)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "plasma") +
    labs(
      title = "Revenue by Crop (Millions USD)",
      x = "Year",
      y = "Crop Group",
      fill = "Revenue\n(Millions)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  # 5. PLOT 4: PRODUCTIVITY SCATTER ----
  p4 <- plot_data %>%
    group_by(crop_group) %>%
    summarise(
      avg_area = mean(area),
      avg_production = mean(production),
      avg_revenue = mean(revenue_millions),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = avg_area, y = avg_production, size = avg_revenue, color = crop_group)) +
    geom_point(alpha = 0.8) +
    scale_size(range = c(3, 10)) +
    scale_color_manual(values = crop_colors) +
    labs(
      title = "Productivity Analysis",
      subtitle = "Bubble size = Average Revenue (Millions USD)",
      x = "Average Area (ha)",
      y = "Average Production (tons)",
      color = "Crop Group",
      size = "Revenue\n(Millions)"
    ) +
    theme_minimal()
  
  # 6. PLOT 5: EU-FOCUSED MAP ----
  # Get EU countries geometry
  eu_countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(continent == "Europe") %>%
    st_crop(xmin = -25, xmax = 40, ymin = 35, ymax = 70)  # Rough EU extent
  
  # Prepare map data
  map_data <- plot_data %>%
    group_by(lat, long, crop_group) %>%
    summarise(area = mean(area), .groups = "drop")
  
  p5 <- ggplot() +
    geom_sf(data = eu_countries, fill = "gray95", color = "gray60") +
    geom_point(
      data = map_data,
      aes(x = long, y = lat, size = area, color = crop_group),
      alpha = 0.7
    ) +
    scale_size(range = c(1, 8)) +
    scale_color_manual(values = crop_colors) +
    coord_sf(
      xlim = c(-10, 30),  
      ylim = c(35, 60)    
    ) +
    labs(
      title = "EU Crop Distribution",
      subtitle = "Point size = Average Area (ha)",
      color = "Crop Group",
      size = "Area (ha)"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank()
    )
  
  # 7. COMBINE PLOTS ----
  combined_plot <- (p3 + p4)  / p5 +
    plot_layout(heights = c(1, 1, 1.5)) +
    plot_annotation(
      title = paste("Comprehensive Crop Analysis -", original_name),
      theme = theme(plot.title = element_text(size = 16, face = "bold"))
    )
  
  # 8. SAVE OUTPUTS ----
  # Save combined PDF
  plot_name <- paste0(original_name, "_crop_analysis.pdf")
  ggsave(
    file.path(dir_out, plot_name),
    combined_plot,
    width = 16,
    height = 20,
    device = "pdf"
  )
  
  
  message("Visualizations saved for: ", original_name)
}
# Main processing loop
rdata_files <- list.files(
  path = dir_results,
  pattern = "\\.RData$",
  full.names = TRUE
)

# Process all files
results <- map(rdata_files, ~{
  tryCatch({
    process_rdata_file(.x, simu_shp, price_data, luc_id_shp)
  }, error = function(e) {
    message("Error processing file: ", basename(.x), "\n", e$message)
    return(NULL)
  })
})

# Combine and save final results
all_results <- bind_rows(compact(results)) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

if (nrow(all_results) > 0) {
  write_csv(all_results, file.path(dir_out, "combined_crop_analysis_results.csv"))
  message("\nProcessing complete. Results saved to: ", dir_out)
} else {
  message("\nNo files were successfully processed.")
}

# Main processing loop
rdata_files <- list.files(
  path = dir_results,
  pattern = "\\.RData$",
  full.names = TRUE
)

# Process all files
results <- map(rdata_files, ~{
  tryCatch({
    process_rdata_file(.x, simu_shp, price_data, luc_id_shp)
  }, error = function(e) {
    message("Error processing file: ", basename(.x), "\n", e$message)
    return(NULL)
  })
})

# Combine and save final results
all_results <- bind_rows(compact(results)) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

if (nrow(all_results) > 0) {
  write_csv(all_results, file.path(dir_out, "combined_crop_analysis_results.csv"))
  message("\nProcessing complete. Results saved to: ", dir_out)
} else {
  message("\nNo files were successfully processed.")
}