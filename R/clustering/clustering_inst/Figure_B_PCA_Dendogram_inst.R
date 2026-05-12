#------------------------------------------------------------------------------#
# PCA-based clustering for the 18-variable institutional specification
# (2000-2019). Textbook robustness check addressing the double-counting
# concern that arises with several correlated WGI indicators inside the
# variable block: orthogonalise the country fixed-effect estimates via PCA,
# then apply Ward hierarchical clustering on Euclidean distances of the
# principal-component scores. Components are retained by the Kaiser criterion
# (eigenvalues >= 1).
#
# Outputs (in output/):
#   FE_Clust_inst_PCA_Dendo_Kaiser.pdf
#   FE_Clust_inst_PCA_Loadings.csv
#------------------------------------------------------------------------------#

rm(list = ls())

library(here)
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)

source(here("R/clustering/utils/helper_functions.R"))

# Load the SE-weighted cluster result; we only re-use the fixed-effect
# estimates (results$panel_estimates) for the PCA.
load(here("output/cluster_results/cluster_results_inst/FE_Clust_EA_inst_2000_2019.RData"))

k <- 4  # number of country groupings

cluster_colors <- c(
  "1" = "#925E9F",
  "2" = "#ED0000",
  "3" = "#0099B4",
  "4" = "#42B540"
)

#------------------------------------------------------------------------------#
# Build FE matrix (countries x variables) --------------------------------------
#------------------------------------------------------------------------------#

panel_estimates <- results$panel_estimates

est_cols <- grep("_est$", names(panel_estimates), value = TRUE)
fe_matrix <- as.matrix(panel_estimates[, est_cols, drop = FALSE])
rownames(fe_matrix) <- rownames(panel_estimates)
colnames(fe_matrix) <- sub("_est$", "", est_cols)

# Check that no NAs exist
any(is.na(fe_matrix))

#------------------------------------------------------------------------------#
# PCA --------------------------------------------------------------------------
#------------------------------------------------------------------------------#

pca_obj <- prcomp(fe_matrix, center = TRUE, scale. = TRUE)

eig            <- pca_obj$sdev^2
var_explained  <- eig / sum(eig)
cum_var        <- cumsum(var_explained)
n_pcs_kaiser   <- max(1L, sum(eig >= 1))

scores <- pca_obj$x

#------------------------------------------------------------------------------#
# Loadings table ---------------------------------------------------------------
#------------------------------------------------------------------------------#

loadings_mat <- pca_obj$rotation
extra_rows   <- rbind(variance_share      = round(var_explained, 4),
                      cumulative_variance = round(cum_var, 4))
colnames(extra_rows) <- colnames(loadings_mat)

loadings_out <- rbind(loadings_mat, extra_rows)
loadings_df  <- tibble::rownames_to_column(as.data.frame(loadings_out),
                                           var = "variable")

write.csv(loadings_df,
          file = here("output/FE_Clust_inst_PCA_Loadings.csv"),
          row.names = FALSE)

#------------------------------------------------------------------------------#
# Dendrogram (Kaiser cut) ------------------------------------------------------
#------------------------------------------------------------------------------#

scores_kaiser <- scores[, seq_len(n_pcs_kaiser), drop = FALSE]
d_kaiser      <- dist(scores_kaiser)
ag_kaiser     <- agnes(d_kaiser, method = "ward")

dend <- fviz_dend(
  ag_kaiser,
  k = k,
  cex = 0.7,
  main = sprintf(
    "PCA-then-Cluster, 18-var inst. spec. (Kaiser: %d PCs, %.0f%% var.)",
    n_pcs_kaiser, 100 * cum_var[n_pcs_kaiser]
  ),
  ylab = "Height",
  rect = FALSE,
  color_labels_by_k = TRUE,
  horiz = TRUE,
  palette = cluster_colors,
  ggtheme = theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank()
    )
) +
  custom_rect_dendrogram(
    stats::as.dendrogram(ag_kaiser),
    k = k,
    k_colors = cluster_colors,
    rect_fill = TRUE,
    rect_lty = 2,
    rect_width_offset = 0.8
  ) +
  coord_flip() +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.spacing = unit(0, "lines")
  )

dend$layers[[1]]$mapping$linewidth <- NULL
dend$layers[[1]]$mapping$lwd <- NULL
dend$layers[[1]]$aes_params$linewidth <- 0.5
dend$layers[[1]]$aes_params$lwd <- 0.5
dend <- dend + guides(linewidth = "none", lwd = "none")

ggsave(file = here("output/FE_Clust_inst_PCA_Dendo_Kaiser.pdf"),
       plot = dend,
       width = 9, height = 6,
       bg = "white",
       device = cairo_pdf,
       dpi = 300)
