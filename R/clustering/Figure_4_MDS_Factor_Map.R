rm(list = ls())
set.seed(4)

# Load required packages
library(here)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(RColorBrewer)
library(stringr)
library(isotone)
library(xtable)
library(patchwork)

# =============================================================================
# DATA LOADING
# =============================================================================

load(here("output/cluster_results/FE_Clust_EA_2000_2019.Rdata"))
cluster_obj <- results$agnes_results
dist_matrix <- results$weighted_distances
panel_estimates <- results$panel_estimates
variable_weights <- results$variable_weights

# =============================================================================
# EXTRACT VARIABLES AND PREPARE DATA
# =============================================================================

# Extract variable names from "_est" columns
cluster_vars <- colnames(panel_estimates)[grepl("_est$", colnames(panel_estimates))]
cluster_vars_clean <- gsub("_est$", "", cluster_vars)

# =============================================================================
# MULTIDIMENSIONAL SCALING (MDS)
# =============================================================================

# Compute MDS coordinates for four dimensions
n_dims <- 2
mds_coords_full <- cmdscale(dist_matrix, k = n_dims, eig = TRUE)
mds_coords <- mds_coords_full$points
colnames(mds_coords) <- paste0("MDS_", 1:n_dims)

# =============================================================================
# STRESS CALCULATION
# =============================================================================

# Distanzen berechnen
mds_dist <- as.matrix(dist(mds_coords))
orig_dist <- as.matrix(dist_matrix)

# Obere Dreiecksmatrix extrahieren (ohne Diagonale)
mds_vec <- mds_dist[upper.tri(mds_dist)]
orig_vec <- orig_dist[upper.tri(orig_dist)]

# 1. METRIC STRESS (misst absolute Distanz-Abweichungen)
metric_stress <- sqrt(sum((mds_vec - orig_vec)^2) / sum(orig_vec^2))

# 2. KRUSKAL'S NONMETRIC STRESS (misst Monotonie-Abweichungen)
# MONOTONE REGRESSION (Pool Adjacent Violators Algorithm)
ord <- order(orig_vec)
d_hat <- gpava(z = orig_vec[ord], y = mds_vec[ord])$x

# Zurück in ursprüngliche Reihenfolge bringen
d_hat_unsorted <- numeric(length(ord))
d_hat_unsorted[ord] <- d_hat

# STRESS nach Kruskal (1964)
stress <- sqrt(sum((mds_vec - d_hat_unsorted)^2) / sum(mds_vec^2))


# =============================================================================
# CORRELATION ANALYSIS
# =============================================================================

# Combine MDS coordinates with panel estimates
correlation_data <- data.frame(
  country_id = rownames(panel_estimates),
  panel_estimates,
  mds_coords[match(rownames(panel_estimates), rownames(mds_coords)), ]
)

# Calculate correlations between MDS dimensions and estimates
numeric_cols <- correlation_data %>%
  dplyr::select(starts_with("MDS_"), ends_with("_est")) %>%
  dplyr::select_if(is.numeric)

cor_matrix <- cor(numeric_cols, use = "complete.obs")
mds_var_correlations <- cor_matrix[1:n_dims, 5:ncol(cor_matrix)]




# =============================================================================
# CLUSTER ASSIGNMENT
# =============================================================================

n_clusters <- 4
cluster_factor <- factor(cutree(cluster_obj, k = n_clusters))

# Define cluster colors and shapes
cluster_colors <- c(
  "1" = "#925E9F",
  "2" = "#ED0000", 
  "3" = "#0099B4",
  "4" = "#42B540"
)

cluster_shapes <- c(
  "1" = 16,  # circle
  "2" = 17,  # triangle
  "3" = 15,  # square
  "4" = 18   # diamond
)

# =============================================================================
# PREPARE VISUALIZATION DATA
# =============================================================================

# MDS data for plotting
mds_data <- data.frame(
  x = mds_coords[, 1],
  y = mds_coords[, 2],
  country = rownames(mds_coords),
  cluster = cluster_factor
)

# Calculate convex hulls for each cluster
hulls <- do.call(rbind, lapply(split(mds_data, mds_data$cluster), function(cluster_data) {
  if(nrow(cluster_data) < 3) {
    return(cluster_data)
  }
  hull_indices <- chull(cluster_data$x, cluster_data$y)
  cluster_data[hull_indices, ]
}))

# Prepare variable loadings for biplot
variable_labels <- c(
  "Unemp_est" = "Unemployment",
  "XinPercGDP_est" = "Exports",
  "GDPpcPPPDivFromMean_est" = "GDP p.c. (dev.)",
  "CAinPercGDP_est" = "Current Account",
  "DebtPercGDP_est" = "Public Debt",
  "FinanceShareVA_est" = "Finance Share",
  "ManufacturingShareVA_est" = "Manufacturing",
  "AgricultureShareVA_est" = "Agriculture",
  "MiningShareVA_est" = "Mining",
  "GiniMkt_est" = "Inequality (Gini)",
  "FDInetinflow_est" = "FDI Inflows",
  "ECI_est" = "ECI"
)

loadings_data <- data.frame(
  variable = colnames(mds_var_correlations),
  MDS1 = mds_var_correlations[1, ],
  MDS2 = mds_var_correlations[2, ],
  variable_clean = variable_labels[colnames(mds_var_correlations)]
)

# Scale factor for variable vectors
scale_factor <- 4

# Filter variables based on correlation strength (optional)
loadings_filtered <- loadings_data %>%
  mutate(variable = str_remove(variable, "_est$")) %>% 
  mutate(correlation_strength = sqrt(MDS1^2 + MDS2^2)) %>%
  filter(correlation_strength > 0) %>% 
  left_join(variable_weights, by = "variable")


# =============================================================================
# PLOT: MDS FACTOR MAP WITH INTEGRATED LOADING VECTORS
# =============================================================================

# Berechne symmetrische Achsenlimits unabhängig für x und y
max_abs_x <- max(abs(mds_data$x))
max_abs_y <- max(abs(mds_data$y))
xlim <- c(-max_abs_x, max_abs_x)
ylim <- c(-max_abs_y, max_abs_y)

mds_plot_alternative <- ggplot() +
  # Convex hulls (filled areas)
  geom_polygon(data = hulls,
               aes(x = x, y = y, fill = cluster),
               alpha = 0.12,
               color = NA) +
  # Convex hulls (outlines)
  geom_polygon(data = hulls,
               aes(x = x, y = y, color = cluster),
               alpha = 0,
               size = 0.6,
               linetype = "solid",
               fill = NA) +
  # Nulllinien
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4, linetype = "solid") +
  geom_vline(xintercept = 0, color = "grey60", linewidth = 0.4, linetype = "solid") +
  # Variable vectors - um Faktor 4 skaliert, gleiches Layout wie im loadings_plot
  geom_segment(data = loadings_filtered,
               aes(x = 0, y = 0,
                   xend = MDS1 * scale_factor,
                   yend = MDS2 * scale_factor,
                   linewidth = avg_weight),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed", angle = 20),
               color = "grey30",
               linetype = "solid",
               lineend = "butt",
               linejoin = "mitre") +
  scale_linewidth(range = c(0.4, 1.6), name = "Scaling Factor") +
  # Variable labels
  geom_text_repel(data = loadings_filtered,
                  aes(x = MDS1 * scale_factor,
                      y = MDS2 * scale_factor,
                      label = variable_clean),
                  size = 3,
                  color = "grey20",
                  fontface = "bold",
                  bg.color = "white",
                  bg.r = 0.15,
                  force = 2.5,
                  max.overlaps = Inf) +
  # Country points
  geom_point(data = mds_data,
             aes(x = x, y = y, color = cluster, shape = cluster),
             size = 3.5, alpha = 0.9) +
  # Country labels
  geom_text_repel(data = mds_data,
                  aes(x = x, y = y, label = country, color = cluster),
                  size = 3,
                  max.overlaps = Inf,
                  force = 2,
                  segment.alpha = 0.5,
                  segment.size = 0.3) +
  # Stress annotation
  annotate("text", 
           x = Inf, y = Inf, 
           label = if(!is.na(stress)) paste0("Kruskal's Stress = ", round(stress, 3)) else "Stress = N/A",
           hjust = 1.1, vjust = 1.5,
           size = 3.2,
           color = "grey40",
           fontface = "italic") +
  # Styling
  scale_color_manual(values = cluster_colors,
                     labels = c("1" = "Periphery", 
                                "2" = "Emerging Economies", 
                                "3" = "Finance Hubs", 
                                "4" = "Developmental States"),
                     name = "Cluster") +
  scale_fill_manual(values = cluster_colors,
                    labels = c("1" = "Periphery", 
                               "2" = "Emerging Economies", 
                               "3" = "Finance Hubs", 
                               "4" = "Developmental States"),
                    guide = "none") +
  scale_shape_manual(values = cluster_shapes,
                     labels = c("1" = "Periphery", 
                                "2" = "Emerging Economies", 
                                "3" = "Finance Hubs", 
                                "4" = "Developmental States"),
                     name = "Cluster") +
  coord_fixed(ratio = 1, 
              xlim = xlim,
              ylim = ylim, 
              clip = "off") +
  theme_bw() +
  labs(
    x = "MDS Dimension 1",
    y = "MDS Dimension 2",
    title = "Multidimensional Scaling Analysis of East Asian Economies"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.spacing = unit(0.1, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.2)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        size = 3.5, 
        alpha = 1,
        shape = c(16, 17, 15, 18),
        label = ""
      ),
      title = "Cluster",
      order = 1
    ),
    shape = "none",
    linewidth = guide_legend(
      title = "Scaling Factor",
      order = 2
    )
  )

# =============================================================================
# DISPLAY AND SAVE PLOT
# =============================================================================

print(mds_plot_alternative)

# Save plot
ggsave(here("output/mds_factor_map.pdf"),
       plot = mds_plot_alternative,
       width = 10.5, height = 7,
       device = cairo_pdf,
       dpi = 300)

