# ==============================================================================
# Correlation Matrix of Country Fixed Effects
# ==============================================================================
# Creates correlation matrix visualization with hierarchical clustering
# Exports to PNG, PDF, and SVG formats

library(here)
library(tidyverse)
library(corrplot)

# Load cluster analysis results -----------------------------------------------

load(here("output/cluster_results/FE_Clust_EA_2000_2019.Rdata"))
panel_estimates <- results$panel_estimates

# Extract fixed effect estimates ----------------------------------------------

fe_estimates <- panel_estimates %>%
  select(ends_with("_est"))

colnames(fe_estimates) <- gsub("_est$", "", colnames(fe_estimates))

# Rename variables for clarity ------------------------------------------------

variable_labels <- c(
  "Unemp" = "Unemployment rate",
  "XinPercGDP" = "Exports (% of GDP)",
  "GDPpcPPPDivFromMean" = "GDP p.c. (dev. from mean)",
  "CAinPercGDP" = "Current account (% of GDP)",
  "DebtPercGDP" = "Public debt (% of GDP)",
  "FinanceShareVA" = "Finance share of GVA",
  "ManufacturingShareVA" = "Manufacturing share of GVA",
  "AgricultureShareVA" = "Agriculture share of GVA",
  "MiningShareVA" = "Mining share of GVA",
  "GiniMkt" = "Gini (market income)",
  "FDInetinflow" = "FDI net inflows (% of GDP)",
  "ECI" = "Economic Complexity Index"
)

colnames(fe_estimates) <- variable_labels[colnames(fe_estimates)]

# Calculate correlation matrix ------------------------------------------------

cor_fe <- cor(fe_estimates, use = "pairwise.complete.obs")

# Hierarchical clustering -----------------------------------------------------
# Distance: 1 - |correlation| to group variables measuring same dimension

var_dist <- as.dist(1 - abs(cor_fe))
var_hclust <- hclust(var_dist, method = "ward.D2")
cor_fe_reordered <- cor_fe[var_hclust$order, var_hclust$order]

# Define clusters -------------------------------------------------------------

var_clusters <- cutree(var_hclust, k = 3)
cluster_labels_ordered <- var_clusters[var_hclust$order]
n_vars <- length(cluster_labels_ordered)

# Function to create plot -----------------------------------------------------

create_corr_plot <- function() {
  par(mar = c(0, 0, 2, 0))
  
  corrplot(cor_fe_reordered, 
           method = "color",
           type = "full",  # Symmetric matrix
           tl.col = "black", 
           tl.srt = 45,
           tl.cex = 0.75,
           addCoef.col = "black",
           number.cex = 0.65,
           col = colorRampPalette(c("#053061", "#2166AC", "#4393C3", 
                                    "#92C5DE", "#D1E5F0", "#FFFFFF",
                                    "#FDDBC7", "#F4A582", "#D6604D", 
                                    "#B2182B", "#67001F"))(200),
           title = "Correlation Matrix of Country Fixed Effects (2000-2019)",
           mar = c(0, 0, 2, 0))
  
  # Add cluster rectangles (squared boxes for symmetric matrix)
  for(clust in 1:3) {
    idx_current <- which(cluster_labels_ordered == clust)
    
    if(length(idx_current) > 0) {
      start_idx <- min(idx_current)
      end_idx <- max(idx_current)
      
      # Draw rectangle around cluster
      rect(start_idx - 0.5, n_vars - end_idx + 0.5,
           end_idx + 0.5, n_vars - start_idx + 1.5,
           border = "black", lwd = 3)
    }
  }
}

# Export PNG ------------------------------------------------------------------

png(here("output/fe_correlation_matrix_clustered.png"),
    width = 11, height = 9, units = "in", res = 300)
create_corr_plot()
dev.off()

# Export PDF ------------------------------------------------------------------

pdf(here("output/fe_correlation_matrix_clustered.pdf"),
    width = 11, height = 9)
create_corr_plot()
dev.off()

# Export SVG ------------------------------------------------------------------

svg(here("output/fe_correlation_matrix_clustered.svg"),
    width = 11, height = 9)
create_corr_plot()
dev.off()

