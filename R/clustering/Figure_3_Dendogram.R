#------------------------------------------------------------------------------#
# Setup -----------------------------------------------------------------------
#------------------------------------------------------------------------------#
rm(list = ls())

# Load required packages
library(here)
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)
#library(gridExtra)
#library(ggrepel)
#library(ggpubr)
#library(devEMF)
#library(grid)

# Source helper functions
source(here("R/clustering/utils/helper_functions.R"))

# Load results
load(here("output/cluster_results/FE_Clust_EA_2000_2019.Rdata"))

#------------------------------------------------------------------------------#
# Prepare visualization ----------------------------------------------------
#------------------------------------------------------------------------------#

cluster_colors <- c( # to ensure that that the colors of the dendogram match
  # those of the Sankey diagram and factor plot
  #"1" = "#00468B", 
  "1" = "#925E9F",
  "2" = "#ED0000", 
  "4" = "#42B540", 
  "3" = "#0099B4"#, 
  #"5" = "#925E9F"
)


#------------------------------------------------------------------------------#
# Dendogram  ----------------------------------------------------
#------------------------------------------------------------------------------#

dend <- fviz_dend(results$agnes_results,
                  k = 4,
                  cex = 0.7,
                  main = "Agglomerative Hierarchical Clustering of FE estimates (2000-2019)",
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
  custom_rect_dendrogram(stats::as.dendrogram(results$agnes_results),
                         k = 4,
                         k_colors = cluster_colors,
                         rect_fill = TRUE,
                         rect_lty = 2,
                         rect_width_offset = 0.8) +
  scale_y_continuous(breaks = seq(0, 15, by = 5), trans = "reverse") +
  coord_flip()+
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),  # Alle Ränder auf 0
    panel.spacing = unit(0, "lines"))

# Save
ggsave(file = here("output/FE_Clust_Dendo.pdf"),
       plot = dend,
       width = 9, height = 6,
       bg = "white",
       device = cairo_pdf,
       dpi = 300)

ggsave(file = here("output/FE_Clust_Dendo.svg"),
       plot = dend,
       width = 9, height = 6,
       bg = "white",
       #device = cairo_pdf,
       dpi = 300)

#ggsave(file = here("output/FE_Clust_Dendo.png"),
       #width = 9, height = 6, bg = "white")
