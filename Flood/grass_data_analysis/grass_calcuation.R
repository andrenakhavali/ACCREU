# Load required libraries
library(tidyverse)
library(data.table)
library(patchwork)

# 1. SETUP AND CONFIGURATION ----------------------------------------------
grass_data_path <- "P:/bnr/03_Projects/ACCREU/WP2/GLOBIOM/GLOBIOM_GRASS_REQ_PROD_ACCREU.Rdata"
dir_results <- "H:/ACCRELEU/flood/results_v04/"
dir_out <- "H:/ACCRELEU/flood/results_v04/grass_data_analysis"

# Create output directories
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(dir_out, "csv"), showWarnings = FALSE)
dir.create(file.path(dir_out, "plots"), showWarnings = FALSE)

# 2. LOAD BASE DATA ------------------------------------------------------
load(grass_data_path) 
detailed_files <- list.files(dir_results, pattern = "_flooded_detailed\\.csv$", full.names = TRUE)

# 3. PROCESSING FUNCTION -------------------------------------------------
process_scenario <- function(dfile) {
  label <- sub("_flooded_detailed\\.csv$", "", basename(dfile))
  parts <- strsplit(label, "_")[[1]]
  scen1 <- paste(parts[2:3], collapse = "_")
  scen2 <- parts[4]
  scen3 <- parts[5]
  
  cat("\nProcessing:", label, "\n")
  
  # Load and prepare flood data (GRASS ONLY)
  flood_data <- fread(dfile) %>% 
    rename(simuid = SimUID) %>%
    mutate(
      simuid = as.integer(simuid),
      year = as.integer(year),
      grass_class = ifelse(lu.to == "GrsLnd", "GrsLnd", NA_character_)
    ) %>%
    filter(!is.na(grass_class)) %>%
    select(-lu.to)
  
  # Filter and prepare grass data
  grass_data <- GRAS_YLD_PROD_REQ_OUTPUT %>%
    filter(
      Scen1 == scen1,
      Scen2 == scen2,
      Scen3 == scen3,
      IND %in% c("GRAS_REQ", "GRAS_YLD"),
      !is.na(simuid)
    ) %>%
    mutate(
      simuid = as.integer(as.character(simuid)),
      year = as.integer(YEAR),
      grass_class = "GrsLnd"
    ) %>%
    select(simuid, year, grass_class, IND, VALUE) %>%
    pivot_wider(names_from = IND, values_from = VALUE) %>%
    rename(
      grass_requirement = GRAS_REQ,
      grass_yield = GRAS_YLD
    )
  
  # Merge and calculate metrics 
  final_data <- flood_data %>%
    inner_join(grass_data, by = c("simuid", "year", "grass_class")) %>%
    mutate(
      total_grass_prod = (lu_area_km2_polygon * (grass_yield / 0.01)) / 1000,  
      lost_prod_slr = (flooded_area_km2_by_lu * (grass_yield / 0.01)) / 1000,
      production_gap = grass_requirement - (total_grass_prod - lost_prod_slr),
      
      # Add scenario identifiers
      scenario = scen1,
      model = scen2,
      rcp = scen3,
      
      # Additional helpful metrics
      prod_efficiency = total_grass_prod / grass_requirement,
      risk_category = case_when(
        flooded_area_km2_by_lu > 0 & production_gap > 0 ~ "High Risk",
        flooded_area_km2_by_lu > 0 ~ "Moderate Risk",
        production_gap > 0 ~ "Supply Deficit",
        TRUE ~ "Stable"
      )
    ) %>%
    arrange(simuid, year)
  
  # 4. GENERATE PLOTS ----------------------------------------------------
  plot_data <- final_data %>% filter(!is.na(production_gap))
  
  p_yield <- ggplot(plot_data, aes(x = grass_yield, y = grass_requirement)) +
    geom_point(aes(size = lu_area_km2_polygon, color = production_gap), alpha = 0.7) +
    scale_size_continuous(range = c(1, 10)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    labs(title = paste("Scenario:", scen1, scen2, scen3),
         x = "Yield (t/ha)", y = "Requirement (1000 t dm)") +
    theme_minimal()
  
  p_gap <- ggplot(plot_data, aes(x = production_gap)) +
    geom_histogram(fill = "steelblue", bins = 30) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    labs(x = "Production Gap (1000 t dm)", y = "Count") +
    theme_minimal()
  
  combined_plot <- p_yield / p_gap + plot_layout(heights = c(2, 1))
  
  # 5. SAVE OUTPUTS -----------------------------------------------------
  # CSV file with ALL calculated metrics
  csv_path <- file.path(dir_out, "csv", paste0(label, "_results.csv"))
  fwrite(final_data, csv_path)
  
  # PDF plot
  pdf_path <- file.path(dir_out, "plots", paste0(label, "_analysis.pdf"))
  ggsave(pdf_path, combined_plot, width = 10, height = 8, device = "pdf")
  
  return(final_data)
}

# 6. PROCESS ALL FILES ---------------------------------------------------
all_results <- map(detailed_files, safely(process_scenario))

# Combine successful results
successful_results <- map(all_results, "result") %>% compact()
combined_data <- rbindlist(successful_results, fill = TRUE)

# Save combined data with ALL metrics
fwrite(combined_data, file.path(dir_out, "all_scenarios_combined.csv"))

# 7. FINAL REPORT --------------------------------------------------------
cat("\nPROCESSING COMPLETE\n")
cat("Processed files:", length(detailed_files), "\n")
cat("Successful outputs:", length(successful_results), "\n")
cat("Failed files:", length(detailed_files) - length(successful_results), "\n")
cat("Output location:", normalizePath(dir_out), "\n")
cat("Production gap range in final data:", 
    round(min(combined_data$production_gap, na.rm = TRUE), 2), "to",
    round(max(combined_data$production_gap, na.rm = TRUE), 2), "\n")
