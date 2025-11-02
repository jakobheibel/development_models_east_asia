# ==============================================================================
# EAST ASIA CLUSTER CHARACTERISTICS BOXPLOTS
# Zeigt charakteristische Merkmale der vier Entwicklungsmodelle
# ==============================================================================

# Clear environment
rm(list = ls())
set.seed(123)

# Load required packages
library(here)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggplot2)
library(RColorBrewer)
library(purrr)
library(ggpubr)

# Source utility functions
source(here("R/stylized facts/Figure_7_boxplot_utils.R"))

# Import data
load(here("data/macro_world.RData"))

# ==============================================================================
# 1. DATA PREPARATION
# ==============================================================================

# Define cluster assignment
cluster_assignment <- tribble(
  ~Country, ~Cluster,
  # Periphery (1)
  "Vietnam", "Periphery",
  "Mongolia", "Periphery",
  "Myanmar", "Periphery",
  "Cambodia", "Periphery",
  "Laos", "Periphery",
  "Indonesia", "Periphery",
  # Emerging (2)
  "China", "Emerging",
  "Malaysia", "Emerging",
  "Philippines", "Emerging",
  "Thailand", "Emerging",
  # Finance (3)
  "Hong Kong", "Finance",
  "Singapore", "Finance",
  # Developmental (4)
  "Taiwan", "Developmental",
  "Japan", "Developmental",
  "South Korea", "Developmental"
)

# Prepare base dataset
macro_data <- macro_world %>%
  left_join(cluster_assignment, by = "Country") %>%
  filter(Year >= 2000, Year <= 2019, !is.na(Cluster))

# ==============================================================================
# 2. CONFIGURE GROUP COMPARISONS
# ==============================================================================

group_configs <- list(
  # Developmental States: Fokus auf niedrige Ungleichheit, Demokratie, moderate FDI
  Developmental = tribble(
    ~var_name,          ~title,                    ~y_label,                    ~reference_groups,           ~special_treatment,
    "FDInetinflow",     "FDI Net Inflows",         "% of GDP",                  list("Others"),              "exclude_mng_2016",
    "GiniMkt",          "Market Income",             "Gini Coefficient",          list("Others"),              "japan_separate",
    "LibDem",           "Liberal Democracy",       "V-Dem Index",               list("Others"),              "none"
  ),
  
  # Emerging Economies: Fokus auf Ungleichheit, hohe FDI, Rohstoffsektor
  Emerging = tribble(
    ~var_name,              ~title,                        ~y_label,                    ~reference_groups,           ~special_treatment,
    "GiniMkt",              "Market Income",                 "Gini Coefficient",          list("Dev_noJapan"),         "none",
    "FDInetinflow",         "FDI Net Inflows",             "% of GDP",                  list("Developmental"),       "exclude_mng_2016",
    "Mining_Agri_Share",    "Mining + Agriculture",        "% of Value Added",          list("Developmental"),       "combined"
  ),
  
  # Finance: Fokus auf Finanzsektor, hohe FDI, hohe Exporte
  Finance = tribble(
    ~var_name,          ~title,                    ~y_label,                    ~reference_groups,           ~special_treatment,
    "FinanceShareVA",   "Financial Sector",        "% of Value Added",          list("Developmental"),       "none",
    "FDInetinflow",     "FDI Net Inflows",         "% of GDP",                  list("Developmental"),       "exclude_mng_2016",
    "XinPercGDP",       "Exports",                 "% of GDP",                  list("Developmental"),       "none"
  ),
  
  # Periphery: Fokus auf Rohstoffsektor, hohe FDI, Leistungsbilanz
  Periphery = tribble(
    ~var_name,              ~title,                        ~y_label,                    ~reference_groups,           ~special_treatment,
    "Mining_Agri_Share",    "Mining + Agriculture",        "% of Value Added",          list("Others"),                "combined",
    "FDInetinflow",         "FDI Net Inflows",             "% of GDP",                  list("Others_noFinance"),      "exclude_mng_2016",
    "CAinPercGDP",          "Current Account",             "% of GDP",                  list("Others"),                "none"
  )
)

# ==============================================================================
# 3. PREPARE COMBINED VARIABLES
# ==============================================================================

# Create combined Mining + Agriculture variable
macro_data <- macro_data %>%
  mutate(Mining_Agri_Share = MiningShareVA + AgricultureShareVA)

# ==============================================================================
# 4. CREATE REFERENCE GROUPS
# ==============================================================================

plot_data_grouped <- data.frame()

for (group in names(group_configs)) {
  config <- group_configs[[group]]
  
  for (i in 1:nrow(config)) {
    var_name <- config$var_name[i]
    ref_groups <- config$reference_groups[[i]]
    special <- config$special_treatment[i]
    
    # Filter data for current variable
    filtered_data <- macro_data %>%
      select(Country, ISO, Year, Cluster, all_of(var_name)) %>%
      rename(value = all_of(var_name)) %>%
      drop_na(value)
    
    # Apply Mongolia 2016 outlier exclusion if needed
    if (special == "exclude_mng_2016" && var_name == "FDInetinflow") {
      filtered_data <- filtered_data %>%
        mutate(value = if_else(Year == 2016 & ISO == "MNG", NA_real_, value)) %>%
        drop_na(value)
    }
    
    # Create comparison groups based on reference
    processed_data <- create_reference_groups_ea(
      data = filtered_data,
      focus_group = group,
      ref_groups = ref_groups,
      special_treatment = special
    ) %>%
      mutate(
        original_focus_group = group,
        variable_name = var_name,
        title = config$title[i],
        y_label = config$y_label[i]
      )
    
    plot_data_grouped <- bind_rows(plot_data_grouped, processed_data)
  }
}

# ==============================================================================
# 5. CREATE PLOTS
# ==============================================================================

# Define color palette
set2_colors <- brewer.pal(n = 8, name = "Set2")

group_colors <- c(
  "Developmental" = "#42B540",
  "Emerging" = "#ED0000",
  "Finance" = "#0099B4",
  "Periphery" = "#925E9F",
  "Developmental \n(excl. Japan)" = "#78C475",
  "Japan" = "#2D7A2D",
  "Others" = "grey70",
  "Others \n(excl. Finance)" = "grey60"
)

# Create plots for each group
group_plots <- map2(group_configs, names(group_configs), function(config, group_name) {
  plots <- map(1:nrow(config), function(i) {
    var_data <- plot_data_grouped %>%
      filter(original_focus_group == group_name, 
             variable_name == config$var_name[i])
    
    # Determine factor levels for comparison groups
    unique_groups <- unique(var_data$comparison_group)
    focus_in_data <- group_name %in% unique_groups
    
    if (focus_in_data) {
      level_order <- c(group_name, setdiff(unique_groups, group_name))
    } else {
      # For special cases like "Dev_noJapan"
      level_order <- c("Developmental \n(excl. Japan)", "Japan", "Others")
    }
    
    var_data <- var_data %>%
      mutate(comparison_group = factor(comparison_group, levels = level_order))
    
    # Create boxplot
    p <- ggplot(var_data, aes(x = comparison_group, y = value, fill = comparison_group)) +
      geom_boxplot(width = 0.7, alpha = 0.9, outliers = FALSE) +
      geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "black") +
      stat_boxplot(geom = "errorbar", width = 0.3) +
      scale_fill_manual(values = group_colors) +
      theme_bw() +
      ggtitle(config$title[i]) +
      ylab(config$y_label[i]) +
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7.8),
        axis.title.y = element_text(size = 9),
        panel.border = element_blank(),
        axis.line = element_line()
      )
    
    return(p)
  })
  
  # Combine plots horizontally
  wrap_plots(plots, nrow = 1) +
    plot_layout(guides = "collect") &
    theme(plot.margin = margin(5, 5, 5, 5))
})


# ==============================================================================
# 6. EXPORT INDIVIDUAL FIGURES
# ==============================================================================

# Export Developmental
ggsave(file = here("output/cluster_characteristics_developmental.pdf"),
       plot = group_plots[[1]],
       width = 7, height = 3)

ggsave(file = here("output/cluster_characteristics_developmental.png"),
       plot = group_plots[[1]],
       width = 7, height = 3, dpi = 300, bg = "white")


# Export Emerging
ggsave(file = here("output/cluster_characteristics_emerging.pdf"),
       plot = group_plots[[2]],
       width = 7, height = 3)

ggsave(file = here("output/cluster_characteristics_emerging.png"),
       plot = group_plots[[2]],
       width = 7, height = 3, dpi = 300, bg = "white")


# Export Finance
ggsave(file = here("output/cluster_characteristics_finance.pdf"),
       plot = group_plots[[3]],
       width = 7, height = 3)

ggsave(file = here("output/cluster_characteristics_finance.png"),
       plot = group_plots[[3]],
       width = 7, height = 3, dpi = 300, bg = "white")


# Export Periphery
ggsave(file = here("output/cluster_characteristics_periphery.pdf"),
       plot = group_plots[[4]],
       width = 7, height = 3)

ggsave(file = here("output/cluster_characteristics_periphery.png"),
       plot = group_plots[[4]],
       width = 7, height = 3, dpi = 300, bg = "white")


# ==============================================================================
# CALCULATE BOXPLOT STATISTICS FOR TEXT
# ==============================================================================

# Function to calculate comprehensive boxplot statistics
calculate_boxplot_stats <- function(data) {
  data %>%
    group_by(original_focus_group, variable_name, title, y_label, comparison_group) %>%
    summarise(
      n = n(),
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      q25 = quantile(value, 0.25, na.rm = TRUE),
      q75 = quantile(value, 0.75, na.rm = TRUE),
      iqr = IQR(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      range = paste0("[", round(min, 1), " to ", round(max, 1), "]"),
      iqr_range = paste0("[", round(q25, 1), " to ", round(q75, 1), "]")
    )
}

# Calculate statistics for all groups
boxplot_stats <- calculate_boxplot_stats(plot_data_grouped)

# View statistics by focus group
cat("\n", strrep("=", 80), "\n")
cat("BOXPLOT STATISTICS BY DEVELOPMENT MODEL\n")
cat(strrep("=", 80), "\n")

for (group in names(group_configs)) {
  cat("\n\n### ", group, " ###\n")
  
  stats_group <- boxplot_stats %>%
    filter(original_focus_group == group) %>%
    select(title, comparison_group, n, median, mean, q25, q75, min, max)
  
  print(stats_group, n = 100)
}

# Export to CSV for reference
write.csv(boxplot_stats, 
          here("output/boxplot_statistics.csv"), 
          row.names = FALSE)

# Create a summary table for the paper
summary_for_paper <- boxplot_stats %>%
  mutate(
    stat_description = case_when(
      # For focus groups: show median and IQR
      comparison_group %in% c("Developmental", "Emerging", "Finance", "Periphery") ~ 
        paste0("Median: ", round(median, 1), " (IQR: ", round(q25, 1), "-", round(q75, 1), ")"),
      # For comparison groups: show median and range
      TRUE ~ 
        paste0("Median: ", round(median, 1), " (Range: ", round(min, 1), "-", round(max, 1), ")")
    )
  ) %>%
  select(original_focus_group, title, comparison_group, stat_description, n)

# Print formatted summary
cat("\n\n")
cat(strrep("=", 80), "\n")
cat("FORMATTED STATISTICS FOR PAPER\n")
cat(strrep("=", 80), "\n\n")

for (group in names(group_configs)) {
  cat("\n### ", group, " ###\n\n")
  
  summary_group <- summary_for_paper %>%
    filter(original_focus_group == group)
  
  print(summary_group, n = 100)
}

# Export formatted summary
write.csv(summary_for_paper, 
          here("output/boxplot_summary_for_paper.csv"), 
          row.names = FALSE)

cat("\n\nStatistics exported to:\n")
cat("  - output/boxplot_statistics.csv (detailed)\n")
cat("  - output/boxplot_summary_for_paper.csv (formatted)\n")

