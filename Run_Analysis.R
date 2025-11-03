#===============================================================================
  # Create Figures for:
  # Beyond the Developmental State:
  # Exploring the Variety of Development Models in East Asia
  # ============================================================================
# This script reproduces the dataset used for the clustering results and re-
# creates the figures and tables used in the paper. 
# ==============================================================================

# Set correct folder with here-package 

rm(list = ls())
library(here)

# Reproduce dataset macro_world.RData

#source(here("R/data-prep/a_value_added_data.R"))
#source(here("R/data-prep/b_data_prep.R"))
## dataset macro_world.RData already created and saved in data

# Clustering

source(here("R/clustering/Figures_A1-A4.R")) 
## script also creates cluster objects used later
source(here("R/clustering/Figure_8.R")) 
## script also creates cluster objects used later
source(here("R/clustering/Figure_3_Dendogram.R"))
source(here("R/clustering/Figure_4_MDS_Factor_Map.R"))

# Tables with data sources and scaling factors

source(here("R/data-prep/Tables_1-2_Tables_A1_A3.R"))

# Stylized facts

source(here("R/stylized facts/Figure_1_convergence_bubble.R"))
source(here("R/stylized facts/Figure_2_correlation_matrix.R"))
source(here("R/stylized facts/Figure_7_boxplot.R"))
source(here("R/stylized facts/Figures_5_6.R"))
