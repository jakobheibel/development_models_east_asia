# ==============================================================================
# UTILITY FUNCTIONS FOR EAST ASIA CLUSTER ANALYSIS
# Helper functions for creating reference groups and processing data
# ==============================================================================

#' Create Reference Groups for East Asia Clusters
#'
#' @description
#' Creates comparison groups based on cluster focus and reference requirements.
#' Handles special cases like Japan separation and combined groups.
#'
#' @param data Input data frame with Cluster column
#' @param focus_group The cluster being analyzed (e.g., "Developmental")
#' @param ref_groups List of reference group specifications
#' @param special_treatment Special handling ("japan_separate", "combined", "none")
#'
#' @return Data frame with comparison_group column added
#' @export
create_reference_groups_ea <- function(data, 
                                       focus_group, 
                                       ref_groups,
                                       special_treatment = "none") {
  
  # Get all unique clusters
  all_clusters <- c("Developmental", "Emerging", "Finance", "Periphery")
  
  # Process based on reference group specification
  if (length(ref_groups) == 1) {
    ref_spec <- ref_groups[[1]]
    
    # Handle different reference group types
    if (ref_spec == "Others") {
      # All other clusters
      comparison_clusters <- setdiff(all_clusters, focus_group)
      
      return_data <- data %>%
        mutate(comparison_group = case_when(
          Cluster == focus_group ~ focus_group,
          Cluster %in% comparison_clusters ~ "Others"
        ))
      
    } else if (ref_spec == "Others_noFinance") {
      # All except focus and Finance
      comparison_clusters <- setdiff(all_clusters, c(focus_group, "Finance"))
      
      return_data <- data %>%
        filter(Cluster != "Finance") %>%
        mutate(comparison_group = case_when(
          Cluster == focus_group ~ focus_group,
          Cluster %in% comparison_clusters ~ "Others \n(excl. Finance)"
        ))
      
    } else if (ref_spec == "Developmental") {
      # Compare to Developmental cluster
      return_data <- data %>%
        filter(Cluster %in% c(focus_group, "Developmental")) %>%
        mutate(comparison_group = Cluster)
      
    } else if (ref_spec == "Dev_noJapan") {
      # Compare to Developmental without Japan
      return_data <- data %>%
        filter(Cluster %in% c(focus_group, "Developmental")) %>%
        mutate(comparison_group = case_when(
          Cluster == focus_group ~ focus_group,
          Country == "Japan" ~ NA_character_,  # Exclude Japan
          Cluster == "Developmental" ~ "Developmental \n(excl. Japan)"
        )) %>%
        drop_na(comparison_group)
      
    } else {
      # Default: use cluster as-is
      return_data <- data %>%
        mutate(comparison_group = Cluster)
    }
    
  } else {
    # Multiple reference groups specified
    return_data <- data %>%
      mutate(comparison_group = Cluster)
  }
  
  # Apply special treatments
  if (special_treatment == "japan_separate") {
    # Separate Japan from other Developmental countries
    return_data <- return_data %>%
      mutate(comparison_group = case_when(
        Country == "Japan" ~ "Japan",
        Cluster == "Developmental" ~ "Developmental \n(excl. Japan)",
        TRUE ~ comparison_group
      ))
  }
  
  return(return_data)
}

#' Calculate Summary Statistics for Boxplots
#'
#' @description
#' Calculates quartiles, median, and outliers for boxplot creation
#'
#' @param data Input data frame
#' @param var_name Variable to summarize
#' @param group_var Grouping variable
#'
#' @return Data frame with summary statistics
#' @export
calculate_boxplot_stats <- function(data, var_name, group_var = "comparison_group") {
  data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      n = n(),
      min = min(!!sym(var_name), na.rm = TRUE),
      q25 = quantile(!!sym(var_name), 0.25, na.rm = TRUE),
      median = median(!!sym(var_name), na.rm = TRUE),
      q75 = quantile(!!sym(var_name), 0.75, na.rm = TRUE),
      max = max(!!sym(var_name), na.rm = TRUE),
      mean = mean(!!sym(var_name), na.rm = TRUE),
      sd = sd(!!sym(var_name), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      iqr = q75 - q25,
      lower_fence = q25 - 1.5 * iqr,
      upper_fence = q75 + 1.5 * iqr
    )
}

#' Identify Outliers for Labeling
#'
#' @description
#' Identifies outliers beyond 1.5*IQR for potential labeling
#'
#' @param data Input data frame
#' @param var_name Variable name
#' @param group_var Grouping variable
#'
#' @return Data frame of outliers
#' @export
identify_outliers <- function(data, var_name, group_var = "comparison_group") {
  # Calculate IQR bounds per group
  bounds <- data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      q25 = quantile(!!sym(var_name), 0.25, na.rm = TRUE),
      q75 = quantile(!!sym(var_name), 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      iqr = q75 - q25,
      lower = q25 - 1.5 * iqr,
      upper = q75 + 1.5 * iqr
    )
  
  # Join bounds and identify outliers
  data %>%
    left_join(bounds, by = group_var) %>%
    mutate(is_outlier = (!!sym(var_name) < lower | !!sym(var_name) > upper)) %>%
    filter(is_outlier) %>%
    select(Country, !!sym(group_var), !!sym(var_name), Year)
}

#' Format Variable Labels
#'
#' @description
#' Creates formatted labels for variables with units
#'
#' @param var_name Variable name
#' @param unit Unit of measurement
#'
#' @return Formatted label string
#' @export
format_var_label <- function(var_name, unit = NULL) {
  # Dictionary of common variable names
  label_dict <- c(
    "FDInetinflow" = "FDI Net Inflows",
    "GiniMkt" = "Market Gini Coefficient",
    "LibDem" = "Liberal Democracy Index",
    "FinanceShareVA" = "Financial Sector Share",
    "XinPercGDP" = "Exports",
    "CAinPercGDP" = "Current Account Balance",
    "Mining_Agri_Share" = "Mining + Agriculture Sectors",
    "ManufacturingShareVA" = "Manufacturing Share",
    "AgricultureShareVA" = "Agriculture Share",
    "MiningShareVA" = "Mining Share"
  )
  
  base_label <- if (var_name %in% names(label_dict)) {
    label_dict[var_name]
  } else {
    var_name
  }
  
  if (!is.null(unit)) {
    paste0(base_label, " (", unit, ")")
  } else {
    base_label
  }
}

#' Validate Cluster Data
#'
#' @description
#' Checks if data has required columns and valid cluster assignments
#'
#' @param data Input data frame
#' @param required_vars Required variable names
#'
#' @return Logical indicating if data is valid
#' @export
validate_cluster_data <- function(data, required_vars = NULL) {
  # Check for required columns
  required_cols <- c("Country", "Year", "Cluster")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    warning("Missing required columns: ", paste(missing_cols, collapse = ", "))
    return(FALSE)
  }
  
  # Check for valid cluster values
  valid_clusters <- c("Developmental", "Emerging", "Finance", "Periphery")
  invalid_clusters <- setdiff(unique(data$Cluster), valid_clusters)
  
  if (length(invalid_clusters) > 0) {
    warning("Invalid cluster values: ", paste(invalid_clusters, collapse = ", "))
    return(FALSE)
  }
  
  # Check for required variables if specified
  if (!is.null(required_vars)) {
    missing_vars <- setdiff(required_vars, names(data))
    if (length(missing_vars) > 0) {
      warning("Missing required variables: ", paste(missing_vars, collapse = ", "))
      return(FALSE)
    }
  }
  
  return(TRUE)
}

#' Print Cluster Summary
#'
#' @description
#' Prints summary information about clusters in the data
#'
#' @param data Input data frame
#'
#' @return NULL (prints to console)
#' @export
print_cluster_summary <- function(data) {
  cat("\n=== Cluster Summary ===\n")
  
  summary <- data %>%
    group_by(Cluster) %>%
    summarise(
      n_countries = n_distinct(Country),
      countries = paste(unique(Country), collapse = ", "),
      n_obs = n(),
      .groups = "drop"
    )
  
  print(as.data.frame(summary))
  
  cat("\n=== Time Period ===\n")
  cat("Years:", min(data$Year, na.rm = TRUE), "to", max(data$Year, na.rm = TRUE), "\n")
  
  invisible(NULL)
}