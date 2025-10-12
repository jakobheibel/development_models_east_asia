library(here)
library(tidyverse)
library(sandwich)
library(plm)
library(cluster)
library(factoextra)
library(RColorBrewer)
library(ggsci)

# sources: based on R scripts in folder:
# "EconomicPolarizationEU2025-main/R/clustering/utils/~.R" 
# from: 
# (https://github.com/dominyj/EconomicPolarizationEU2025) 

# Function for Scale fixed effects and adjust standard errors ------------------

scale_with_se <- function(data_with_se) {
  # Identify pairs of est and se columns based on the pattern type_varname_est/se
  est_cols <- grep("_est$", names(data_with_se), value = TRUE)
  base_names <- sub("_est$", "", est_cols)
  
  scaled_data <- data_with_se
  for(base in base_names) {
    est_col <- paste0(base, "_est")
    se_col <- paste0(base, "_se")
    
    est_mean <- mean(data_with_se[[est_col]], na.rm = TRUE)
    est_sd <- sd(data_with_se[[est_col]], na.rm = TRUE)
    
    # Calculate scaled values
    scaled_data[[est_col]] <- (data_with_se[[est_col]] - est_mean) / est_sd
    scaled_data[[se_col]] <- data_with_se[[se_col]] / est_sd
  }
  scaled_data
}

# Function for Calculate SE-weighted distances ---------------------------------

dist_weighted_by_se <- function(scaled_data,
                                method = c("mean", "rms"),
                                na_handling = c("omit", "infinite_se")) {
  method <- match.arg(method)
  na_handling <- match.arg(na_handling)
  
  # Identify pairs of est and se columns
  est_cols <- grep("_est$", names(scaled_data), value = TRUE)
  base_names <- sub("_est$", "", est_cols)
  se_cols <- paste0(base_names, "_se")
  
  if (length(est_cols) != length(se_cols)) {
    stop("Number of estimate columns must match number of SE columns")
  }
  
  n_countries <- nrow(scaled_data)
  distances <- matrix(0, nrow = n_countries, ncol = n_countries)
  rownames(distances) <- rownames(scaled_data)
  colnames(distances) <- rownames(scaled_data)
  
  for(i in 1:n_countries) {
    for(j in 1:n_countries) {
      if(i < j) {
        t_stats <- numeric(length(base_names))
        for(v in seq_along(base_names)) {
          est_col <- paste0(base_names[v], "_est")
          se_col <- paste0(base_names[v], "_se")
          
          # Get values
          est_i <- scaled_data[[est_col]][i]
          est_j <- scaled_data[[est_col]][j]
          se_i <- scaled_data[[se_col]][i]
          se_j <- scaled_data[[se_col]][j]
          
          # Handle missing values
          if(na_handling == "infinite_se" &&
             (is.na(est_i) || is.na(est_j) || is.na(se_i) || is.na(se_j))) {
            t_stats[v] <- 0  # Missing value leads to zero contribution
          } else {
            diff <- abs(est_i - est_j)
            combined_se <- sqrt(se_i^2 + se_j^2)
            t_stats[v] <- diff / combined_se
          }
        }
        
        # Calculate distance based on chosen method
        distances[i,j] <- if(method == "mean") {
          if(na_handling == "omit") {
            mean(t_stats, na.rm = TRUE)
          } else {
            mean(t_stats)  # All values are defined (0 or actual t-stat)
          }
        } else {  # method == "rms"
          if(na_handling == "omit") {
            sqrt(mean(t_stats^2, na.rm = TRUE))
          } else {
            sqrt(mean(t_stats^2))
          }
        }
        distances[j,i] <- distances[i,j]
      }
    }
  }
  as.dist(distances)
}

# Function for Weights assigned to variables -----------------------------------

calculate_variable_weights <- function(scaled_data, dist_method = "se_weighted") {
  if (dist_method == "standard") {
    var_info <- data.frame(
      variable <- gsub("^fe_|_est$", "", colnames(scaled_data)),
      type = sub("^([^_]+).*", "\\1", colnames(scaled_data))
    )
    weights <- rep(1, ncol(scaled_data))
  } else {
    # Select SE columns
    se_cols <- grep("_se$", names(scaled_data), value = TRUE)
    base_names <- sub("_se$", "", se_cols)
    
    # Extract variable names and types
    var_info <- data.frame(
      full_name = base_names,
      type = sub("^([^_]+)_.*", "\\1", base_names),
      variable = sub("^[^_]+_(.*)", "\\1", base_names)
    )
    
    # Calculate weights for each variable
    weights <- sapply(seq_along(se_cols), function(i) {
      se <- scaled_data[[se_cols[i]]]
      # Calculate combined SE for all pairs
      pairs <- combn(length(se), 2)
      combined_ses <- sqrt(se[pairs[1,]]^2 + se[pairs[2,]]^2)
      # Calculate average weight
      mean(1/combined_ses, na.rm = TRUE)
    })
    
  }
  # Create output tibble with type information
  tibble(
    variable = var_info$variable,
    type = var_info$type,
    avg_weight = weights / sum(weights)
  ) %>%
    arrange(desc(avg_weight))
  
}

# Function for creating period clusters ----------------------------------------

period_cluster <- function(macro_EA, var_names,
                           start_year = 1990, end_year = 1995,
                           k = 5, method = "ward",
                           palette = palette) {
  
  # Set year range for period
  
  macro_EA <- macro_EA %>% 
    filter(Year >= start_year & Year <= end_year)
  
  # Panel estimation
  
  results_lvl <- tibble()
  
  for (var in var_names) {
    
    # Filter the data for the current variable
    macro_EA_var <- macro_EA %>% 
      filter(variable_name == var)
    
    # Estimate the model
    model <- lm(value ~ factor(Country) + factor(Year) - 1, data = macro_EA_var)
    #print(paste0("Calculation complete for Variable: ", var))
    # -1 removes the intercept, meaning that each country has its own baseline level
    # rather than being compared to a reference category
    pdata <- pdata.frame(macro_EA_var, index = c("Country", "Year"))
    vcov_matrix <- vcovHC(model, type = "HC1", cluster = "group", pdata = pdata)
    #vcov_matrix <- vcov(model)
    #vcov_matrix <- sandwich::vcovHC(model)
    
    # Extract coefficients and standard errors
    coefs <- coef(model)
    ses <- sqrt(diag(vcov_matrix))
    
    # Prepare the results for the current variable
    results_var <- tibble(
      variable = var,
      coefficient = names(coefs),
      estimate = coefs,
      std_error = ses
    ) %>% 
      filter(
        str_detect(coefficient, "^factor\\(Country\\)")) %>% # Keep country FXs
      mutate( # remove the factor(Country) prefix
        coefficient = str_remove(coefficient, "^factor\\(Country\\)")) 
    
    # Append the results for the current variable to the overall results
    results_lvl <- bind_rows(results_lvl, results_var)
  }
  
  # Data preparation for standardized distances
  
  results_lvl_clean <- results_lvl %>% 
    pivot_wider(id_cols = all_of("coefficient"),
                names_from = variable,
                values_from = c(estimate, std_error),
                names_glue = "{variable}_{ifelse(.value == 'estimate', 'est', 'se')}"
    ) %>% 
    column_to_rownames("coefficient")
  
  # Scale fixed effects and adjust standard errors
  
  results_lvl_scaled <- scale_with_se(results_lvl_clean)
  
  # Create table for weights assigned to variables
  
  variable_weights_lvl <- calculate_variable_weights(results_lvl_scaled)
  
  # Calculate SE-weighted distances
  
  weighted_distances_lvl <- dist_weighted_by_se(results_lvl_scaled,
                                                method = "mean",
                                                na_handling = "infinite_se")
  # Create hierarchical clusters
  
  agnes_results <- agnes(weighted_distances_lvl, method = "ward")
  
  # Create hierarchical clusters based on standard approach for distance calc
  
  standardized_results <- scale(select(results_lvl_clean, matches("^.*_est$")))
  standard_dist_matrix <- dist(standardized_results)
  
  agnes_results_standard <- agnes(standard_dist_matrix, method = "ward")
  
  # Extract groupings for Sankey diagram and factor map
  
  groupings <- cutree(as.hclust(agnes_results), k = k)
  
  groupings_standard <- cutree(as.hclust(agnes_results_standard), k = k)

  # Make dendogram plot
  
  dendo <- fviz_dend(as.hclust(agnes_results),
                     main = paste0("Agglomerative Hierarchical Clustering of FE estimates (",
                                   start_year, "-", end_year, ")"),
                     k = k,
                     xlab = "Countries", ylab = "Height",
                     palette = palette,
                     cex = 0.75, # label size
                     rect = TRUE, # Add rectangle around groups
                     rect_fill = TRUE,
                     horiz = TRUE)
  
  dendo_standard <- fviz_dend(as.hclust(agnes_results_standard),
                              main = paste0("Agglomerative Hierarchical Clustering of FE estimates (",
                                   start_year, "-", end_year, ")"),
                              k = k,
                              xlab = "Countries", ylab = "Height",
                              palette = palette,
                              cex = 0.75, # label size
                              rect = TRUE, # Add rectangle around groups
                              rect_fill = TRUE,
                              horiz = TRUE)
  
  # Return the results directly: 
  # Weighted distances, Variable Weights, Country groupings, 
  # dendograms, country groupings,
  
  list(
    panel_estimates = results_lvl_clean,
    weighted_distances = weighted_distances_lvl,
    variable_weights = variable_weights_lvl,
    agnes_results = agnes_results,
    agnes_results_standard = agnes_results_standard,
    dendo = dendo,
    dendo_standard = dendo_standard,
    groupings = groupings,
    groupings_standard = groupings_standard
  )
}

# Function for creating Sankey (Period Clusters) -------------------------------
# takes the country_groupings list from 6_a_period_clusters_display.R
# takes the vector with the time_periods defined in 6_a_period_clusters_display.R

create_sankey <- function(country_groupings, time_periods,
cluster_colors) {
  
  # Transform country_groupings list into data frame
  
  country_groupings <- do.call(cbind, country_groupings)
  country_groupings <- as.data.frame(country_groupings)
  country_groupings$Country <- rownames(country_groupings)
  
  # Melt the data frame to long format for Sankey diagram
  
  country_groupings_long <- country_groupings %>% 
    reshape2::melt(id.vars = "Country") %>% 
    mutate(value = as.factor(value))
  
  # Create the limit names for the scale_x_discrete
  
  limit_names <- c()
  
  for (i in seq(1, length(time_periods), by = 2)) {
    limit_names <- c(limit_names, 
                     paste0(time_periods[i], "-", time_periods[i + 1]))
  }
  
  # Create Sankey showing country groupings over the time periods
  
  ggplot(country_groupings_long,
         aes(x = variable,
             stratum = value,
             alluvium = Country,
             fill = value,
             label = value)) +
    geom_flow(stat = "alluvium",
              lode.guidance = "zigzag",
              color = "darkgrey") +
    geom_stratum() +
    # Label each alluvium (i.e., each country flow)
    geom_text(stat = "alluvium", 
              aes(label = Country),
              size = 3,
              min.y = 1) +
    #scale_fill_lancet(name = "Clusters") +
    scale_fill_manual(values = cluster_colors, name = "Clusters") +
    #scale_fill_brewer(palette = "Spectral", name = "Clusters") +
    scale_x_discrete(limits = limit_names) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),  # Removes major grid lines
      panel.grid.minor = element_blank(),  # Removes minor grid lines
      axis.text.y = element_blank(),       # Removes y-axis text
      axis.ticks.y = element_blank(),      # Removes y-axis ticks
      axis.title.y = element_blank(),      # Removes y-axis title
      axis.title.x = element_blank(),
      axis.line.y = element_blank()        # Removes y-axis line
    )
}

# Function for creating clusters with different variables ----------------------

variable_cluster <- function(macro_EA, var_names, var_set,
                             start_year = 2000, end_year = 2019,
                             k = 5, method = "ward",
                             palette = palette) {
  
  # Set year range for period
  
  macro_EA <- macro_EA %>% 
    filter(Year >= start_year & Year <= end_year)
  
  # Panel estimation
  
  results_lvl <- tibble()
  
  for (var in var_set) {
    
    # Filter the data for the current variable
    macro_EA_var <- macro_EA %>% 
      filter(variable_name == var)
    
    # Estimate the model
    model <- lm(value ~ factor(Country) + factor(Year) - 1, data = macro_EA_var)
    #print(paste0("Calculation complte for Variable: ", var))
    # -1 removes the intercept, meaning that each country has its own baseline level
    # rather than being compared to a reference category
    pdata <- pdata.frame(macro_EA_var, index = c("Country", "Year"))
    vcov_matrix <- vcovHC(model, type = "HC1", cluster = "group", pdata = pdata)
    #vcov_matrix <- vcov(model)
    
    # Extract coefficients and standard errors
    coefs <- coef(model)
    ses <- sqrt(diag(vcov_matrix))
    
    # Prepare the results for the current variable
    results_var <- tibble(
      variable = var,
      coefficient = names(coefs),
      estimate = coefs,
      std_error = ses
    ) %>% 
      filter(
        str_detect(coefficient, "^factor\\(Country\\)")) %>% # Keep country FXs
      mutate( # remove the factor(Country) prefix
        coefficient = str_remove(coefficient, "^factor\\(Country\\)")) 
    
    # Append the results for the current variable to the overall results
    results_lvl <- bind_rows(results_lvl, results_var)
  }
  
  # Data preparation for standardized distances
  
  results_lvl_clean <- results_lvl %>% 
    pivot_wider(id_cols = all_of("coefficient"),
                names_from = variable,
                values_from = c(estimate, std_error),
                names_glue = "{variable}_{ifelse(.value == 'estimate', 'est', 'se')}"
    ) %>% 
    column_to_rownames("coefficient")
  
  # Scale fixed effects and adjust standard errors
  
  results_lvl_scaled <- scale_with_se(results_lvl_clean)
  
  # Create table for weights assigned to variables
  
  variable_weights_lvl <- calculate_variable_weights(results_lvl_scaled)
  
  # Calculate SE-weighted distances
  
  weighted_distances_lvl <- dist_weighted_by_se(results_lvl_scaled,
                                                method = "mean",
                                                na_handling = "infinite_se")
  # Create hierarchical clusters
  
  agnes_results <- agnes(weighted_distances_lvl, method = "ward")
  
  # Extract groupings for Sankey diagram and factor map
  
  groupings <- cutree(as.hclust(agnes_results), k = k)
  
  # Make dendogram plot
  
  dendo <- fviz_dend(as.hclust(agnes_results),
                     main = paste0("Clustering of FE estimates (",
                                   var_set_name, ")"),
                     xlab = "Countries", ylab = "Height",
                     k = k,
                     palette = palette,
                     cex = 0.75, # label size
                     rect = TRUE, # Add rectangle around groups
                     rect_fill = TRUE,
                     horiz = TRUE)
  
  # Return the results directly: 
  # Weighted distances, Variable Weights, Country groupings, 
  # dendograms, country groupings,
  
  list(
    weighted_distances = weighted_distances_lvl,
    variable_weights = variable_weights_lvl,
    agnes_results = agnes_results,
    dendo = dendo,
    groupings = groupings
  )
}

# Function for creating Sankey (Variable Selection Clusters) -------------------
# takes the country_groupings list from 6_c_variable_clusters_display.R
# takes the vector with the time_periods defined in 6_c_variable_clusters_display.R

create_sankey_selection <- function(country_groupings, variable_list,
                                    cluster_colors) {
  
  # Transform country_groupings list into data frame
  
  country_groupings <- do.call(cbind, country_groupings)
  country_groupings <- as.data.frame(country_groupings)
  country_groupings$Country <- rownames(country_groupings)
  
  # Melt the data frame to long format for Sankey diagram
  
  country_groupings_long <- country_groupings %>% 
    reshape2::melt(id.vars = "Country") %>% 
    mutate(value = as.factor(value))
  
  # Create the limit names for the scale_x_discrete
  
  limit_names <- c()
  
  for (i in seq_along(variable_list)) {
    
    var_set_name <- names(variable_list[i])
    
    limit_names <- c(limit_names, 
                     paste0(var_set_name))
  }
  
  # Create Sankey showing country groupings over the time periods
  
  ggplot(country_groupings_long,
         aes(x = variable,
             stratum = value,
             alluvium = Country,
             fill = value,
             label = value)) +
    geom_flow(stat = "alluvium",
              lode.guidance = "zigzag",
              color = "darkgrey") +
    geom_stratum() +
    # Label each alluvium (i.e., each country flow)
    geom_text(stat = "alluvium", 
              aes(label = Country),
              size = 3,
              min.y = 1) +
    #scale_fill_lancet(name = "Clusters") +
    scale_fill_manual(values = cluster_colors, name = "Clusters") +
    #scale_fill_brewer(palette = "Spectral", name = "Clusters") +
    scale_x_discrete(limits = limit_names) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),  # Removes major grid lines
      panel.grid.minor = element_blank(),  # Removes minor grid lines
      axis.text.y = element_blank(),       # Removes y-axis text
      axis.ticks.y = element_blank(),      # Removes y-axis ticks
      axis.title.y = element_blank(),      # Removes y-axis title
      axis.title.x = element_blank(),
      axis.line.y = element_blank()        # Removes y-axis line
    )
}