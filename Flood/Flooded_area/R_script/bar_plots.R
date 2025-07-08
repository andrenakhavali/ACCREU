# Load required libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(forcats)
library(viridis)
library(scales)
library(purrr)
library(stringr)

# Set working directory
setwd("H:/ACCRELEU/flood/results_v04/")

### 1. File Processing ========================================================
all_files <- list.files(pattern = "flooded_aggregated.csv$")

extract_metadata <- function(filename) {
  clean_name <- gsub("ACCREU2025_|ACCREU_|_flooded_aggregated.csv", "", filename)
  parts <- unlist(strsplit(clean_name, "_"))
  
  if (parts[1] == "Ref" & parts[2] == "Ref") {
    mitigation <- "Ref_Ref"  # Will be filtered out later
    model <- parts[3]
    rcp <- parts[4]
  } else {
    mitigation <- parts[1]
    model <- parts[3]
    rcp <- parts[4]
  }
  data.frame(mitigation, model, rcp)
}

all_data <- rbindlist(lapply(all_files, function(f) {
  dt <- fread(f)
  meta <- extract_metadata(f)
  cbind(dt, meta)
}))

### 2. Data Preparation ======================================================
mitigation_levels <- c("Ref", "HighAdapt", "LowAdapt", 
                       "Mit2p6", "Mit4p5", "Mit7p0")

plot_data <- all_data %>%
  filter(year %in% c(2020, 2030, 2040, 2050)) %>%
  # Filter out Ref_Ref cases
  filter(mitigation != "Ref_Ref") %>%
  group_by(year, lu_group, mitigation, model, rcp) %>%
  summarise(
    total_flooded = sum(flooded_area_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mitigation = factor(mitigation, 
                        levels = mitigation_levels,
                        labels = c("Reference",  # Renamed from "Reference (Policy)"
                                   "High Adaptation", "Low Adaptation",
                                   "2.6°C Mitigation", "4.5°C Mitigation", 
                                   "7.0°C Mitigation")),
    rcp_label = paste0("RCP ", str_replace(rcp, "RCP|p", "")),
    year = factor(year),
    lu_group = fct_reorder(lu_group, total_flooded, .fun = max)
  )

### 3. Plotting Function =====================================================
generate_plot <- function(data, model_name, rcp_name) {
  p <- ggplot(data, aes(x = year, y = total_flooded, fill = lu_group)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
    facet_wrap(~mitigation, ncol = 3, 
               labeller = labeller(mitigation = label_wrap_gen(20))) +
    scale_fill_viridis(
      discrete = TRUE,
      option = "plasma",
      direction = -1,
      name = "Land Use",
      guide = guide_legend(reverse = TRUE)
    ) +
    labs(
      title = paste0("Flooded Area by Land Use (", model_name, " | ", rcp_name, ")"),
      x = "Year",
      y = "Flooded Area (km²)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 9),
      panel.spacing = unit(1, "lines"),
      legend.position = "bottom"
    ) +
    scale_y_continuous(labels = comma)
  
  filename <- paste0(model_name, "_", rcp_name, "_plot.pdf")
  ggsave(filename, plot = p, width = 12, height = 8, device = "pdf")
}

### 4. Generate All Plots ====================================================
models <- unique(plot_data$model)
rcps <- unique(plot_data$rcp)

walk(models, function(m) {
  walk(rcps, function(r) {
    subset_data <- plot_data %>% 
      filter(model == m & rcp == r)
    
    if (nrow(subset_data) > 0) {
      rcp_clean <- paste0("RCP ", str_replace(r, "RCP|p", ""))
      generate_plot(subset_data, m, rcp_clean)
      message(paste("Generated:", m, rcp_clean))
    }
  })
})

### 5. Combined Plots ==============================================
walk(models, function(m) {
  subset_data <- plot_data %>% filter(model == m)
  
  if (nrow(subset_data) > 0) {
    p <- ggplot(subset_data, aes(x = year, y = total_flooded, fill = lu_group)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      facet_grid(rcp_label ~ mitigation, scales = "free_y") +
      scale_fill_viridis(discrete = TRUE, option = "plasma", direction = -1) +
      labs(title = paste0("Flooded Area (", m, " - All RCPs)")) +
      theme(strip.text.y = element_text(angle = 0))
    
    ggsave(paste0(m, "_ALL_RCPs_plot.pdf"), width = 16, height = 10)
  }
})