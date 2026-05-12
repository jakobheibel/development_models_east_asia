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

load(here("output/cluster_results/cluster_results_inst/FE_Clust_EA_inst_2000_2019.RData"))
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
mds_var_correlations <- cor_matrix[1:n_dims, grepl("_est$", colnames(cor_matrix)), drop = FALSE]

# Comparing importance of each dimension

eig <- mds_coords_full$eig
eig_pos <- eig[eig > 0] # keep only positive eigenvalues
explained_share_dim1 <- eig_pos[1] / sum(eig_pos)
explained_share_dim2 <- eig_pos[2] / sum(eig_pos)

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
  y = -mds_coords[, 2],
  country = rownames(mds_coords),
  cluster = cluster_factor
)

country_labels <- mds_data %>%
  mutate(
    label_x = ifelse(country == "Malaysia", x + 0.15, x),
    label_y = ifelse(country == "Malaysia", y + 0.5, y)
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
  "ECI_est" = "ECI",
  "cc_est" = "Control of Corruption",
  "ge_est" = "Government Effectiveness",
  "pv_est" = "Political Stability",
  "rq_est" = "Regulatory Quality",
  "rl_est" = "Rule of Law",
  "va_est" = "Voice and Accountability"#,
  #"LibDem_est" = "Lib. Democracy",
  #"human_capital_index_est" = "HCI",
  #"sjr_per_million_est" = "Journal Publications",
  #"patent_applications_per_million_est" = "Patent Applications"
)

loadings_data <- data.frame(
  variable = colnames(mds_var_correlations),
  MDS1 = mds_var_correlations[1, ],
  MDS2 = -mds_var_correlations[2, ],
  variable_clean = variable_labels[colnames(mds_var_correlations)]
)

# Scale factor for variable vectors
scale_factor <- 4

# Select which loading vectors to show in the factor map.
# Edit this vector to control the arrows that are plotted.
selected_vector_variables <- c(
  #"Unemp",
  #"XinPercGDP",
  #"GDPpcPPPDivFromMean", 
  #"CAinPercGDP",
  #"DebtPercGDP", 
  #"FinanceShareVA",
  #"ManufacturingShareVA",
  #"AgricultureShareVA",
  #"MiningShareVA",
  #"GiniMkt",
  #"FDInetinflow", 
  #"ECI",
  # World Governance Indicators (WGI) and other social capability variables
  "cc", #"Control of Corruption",
  "ge", #"Government Effectiveness",
  "pv", #"Political Stability and Absence of Violence/Terrorism",
  "rq", #"Regulatory Quality",
  "rl", #"Rule of Law",
  "va"#,  #"Voice and Accountability"
  #"human_capital_index",
  # Technological capability variables
  #"sjr_per_million",
  #"patent_applications_per_million"
)

# Keep all vectors for arrows; mark selected variables for highlighting/labeling.
loadings_all <- loadings_data %>%
  mutate(variable = str_remove(variable, "_est$")) %>%
  left_join(variable_weights, by = "variable") %>%
  mutate(
    avg_weight = ifelse(is.na(avg_weight), 0, avg_weight),
    is_selected = variable %in% selected_vector_variables
  )

loadings_segments_all <- loadings_all
loadings_segments_selected <- loadings_all %>% filter(is_selected)
loadings_labels <- loadings_all %>%
  mutate(
    label_text = variable_clean,
    #label_text = ifelse(is_selected, variable_clean, variable),
    label_color = ifelse(is_selected, "grey20", "grey75")
  )


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
    # Variable vectors (all in grey)
    geom_segment(data = loadings_segments_all,
               aes(x = 0, y = 0,
                   xend = MDS1 * scale_factor,
                   yend = MDS2 * scale_factor,
                   linewidth = avg_weight),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed", angle = 20),
           color = "grey75",
           alpha = 0.9,
               linetype = "solid",
               lineend = "butt",
               linejoin = "mitre") +
    # Highlight selected variable vectors
    geom_segment(data = loadings_segments_selected,
           aes(x = 0, y = 0,
             xend = MDS1 * scale_factor,
             yend = MDS2 * scale_factor,
             linewidth = avg_weight),
           arrow = arrow(length = unit(0.15, "cm"), type = "closed", angle = 20),
           color = "grey30",
           alpha = 1,
           linetype = "solid",
           lineend = "butt",
           linejoin = "mitre") +
  scale_linewidth(range = c(0.4, 1.6), name = "Scaling Factor") +
  # Variable labels
  geom_text_repel(data = loadings_labels,
                  aes(x = MDS1 * scale_factor,
                      y = MDS2 * scale_factor,
            label = label_text,
            color = I(label_color)),
        size = 2.8,
                  bg.color = "white",
                  bg.r = 0.15,
        force = 4,
                  max.overlaps = Inf) +
  # Country points
  geom_point(data = mds_data,
             aes(x = x, y = y, color = cluster, shape = cluster),
             size = 3.5, alpha = 0.9) +
  # Country labels
  #geom_text_repel(data = country_labels,
                  #aes(x = x, y = y, label = country, color = cluster),
  geom_text_repel(data = country_labels,
                  aes(x = label_x, y = label_y, label = country, color = cluster),
                  size = 3,
                  max.overlaps = Inf,
                  force = 2,
                  segment.alpha = 0.5,
                  segment.size = 0.3) +
  # Stress annotation
  annotate("text", 
           x = -Inf, y = Inf, 
           label = if(!is.na(stress)) paste0("Kruskal's Stress = ", round(stress, 3)) else "Stress = N/A",
           hjust = -0.1, vjust = 1.5,
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

# Choose output dimensions based on data aspect ratio.
# This keeps the MDS geometry (coord_fixed = 1) but avoids excess top/bottom whitespace.
export_width <- 10.5
data_aspect_ratio <- diff(ylim) / diff(xlim)
panel_height <- export_width * data_aspect_ratio
export_height <- max(5.0, panel_height + 1.8)

# Save plot
ggsave(here("output/mds_factor_map_inst.pdf"),
       plot = mds_plot_alternative,
  width = export_width, height = export_height,
       device = cairo_pdf,
       dpi = 300)

ggsave(here("output/mds_factor_map_inst.svg"),
       plot = mds_plot_alternative,
  width = export_width, height = export_height,
       #device = cairo_pdf,
       dpi = 300)