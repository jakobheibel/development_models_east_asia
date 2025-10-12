rm(list = ls())
library(here)
source(here("packages.R"))
source(here("R/4_a_clusters_functions.R"))

# Load the data ----------------------------------------------------------------

load(here("data/macro_world.RData"))

EA <- c(
  "China", 
  "Hong Kong", 
  "South Korea", 
  "Mongolia",
  "Myanmar", 
  "Philippines", 
  "Thailand",
  "Singapore", 
  "Malaysia", 
  "Vietnam", 
  "Cambodia", 
  "Laos", 
  "Japan", 
  "Indonesia",
  "Taiwan")

var_filter_cce <- c( #manually change variable selection by commenting out variables 
  "Unemp",
  "XinPercGDP",
  "GDPpcPPPDivFromMean", 
  "CAinPercGDP",
  "DebtPercGDP", 
  "FinanceShareVA",
  "ManufacturingShareVA",
  "AgricultureShareVA",
  "MiningShareVA",
  "GiniMkt",
  "FDInetinflow",
  #"FDInetoutflow",
  "ECI"
)

macro_cce1 <- macro_world %>% 
  filter(Country %in% EA,
         Year >= 2000,
         Year <= 2019) %>%
  select(Country, Year, all_of(var_filter_cce))

macro_cce <- macro_cce1 %>% 
  #select(-ISO) %>% 
  pivot_longer(cols = -c(Country, Year),
               names_to = "variable_name", values_to = "value")

# Number of country groupings

k <- 4

# Color palette for dendograms and factor map

palette = "lancet"

#cluster_colors_dendo <- c(
#  "6" = "#42B540", 
#  "5" = "#0099B4",
#  "4" = "#ED0000",
#  "3" = "#004",
#  "2" = "#AD002AFF",
#  "1" = "#925E9F"
#)

cluster_colors <- c( # to ensure that that the colors of the dendogram match
  # those of the Sankey diagram and factor plot
  "1" = "#925E9F",
  "2" = "#ED0000", 
  "4" = "#42B540", 
  "3" = "#0099B4"#, 
  #"5" = "#004",
  #"4" = "#AD002AFF"
)

# Common correlated effects estimation for East Asia ---------------------------

# source: based on "R/clustering/utils/..." from: 
# (https://github.com/dominyj/EconomicPolarizationEU2025)

results_cce <- tibble()

for (var in var_filter_cce) {
  
  # Prepare data
  macro_EA_var <- macro_cce %>% 
    filter(variable_name == var) %>% 
    group_by(!!sym("Year")) %>% 
    mutate( #cross-sectional means
      y_mean = mean(value, na.rm = TRUE)
    ) %>% 
    ungroup()
  
  # Estimate the model
  model <- lm(value ~ factor(Country) + factor(Country):y_mean - 1, 
              data = macro_EA_var)
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
    filter( # Keep cross-sectional means FXs 
      str_detect(coefficient, "^factor\\(Country\\).+y_mean$")) %>% 
    mutate(
      coefficient = str_remove_all(coefficient, "factor\\(Country\\)|:y_mean"))
  
  # Append the results for the current variable to the overall results
  results_cce <- bind_rows(results_cce, results_var)
}

# Scale fixed effects and adjust standard errors -------------------------------

# Prepare data 

results_cce_clean <- results_cce %>%
  pivot_wider(id_cols = all_of("coefficient"),
              names_from = variable,
              values_from = c(estimate, std_error),
              names_glue = "{variable}_{ifelse(.value == 'estimate', 'est', 'se')}"
  ) %>% 
  column_to_rownames("coefficient")

results_cce_scaled <- scale_with_se(results_cce_clean)

# Calculate SE-weighted distances ----------------------------------------------

weighted_distances_cce <- dist_weighted_by_se(results_cce_scaled,
                                              method = "mean",
                                              na_handling = "infinite_se")

# Showing weights assigned to variables ----------------------------------------

variable_weights_cce <- calculate_variable_weights(results_cce_scaled)

xtable(variable_weights_cce, digits = 4)

# Hierarchical clustering ------------------------------------------------------

clustering_cce <- agnes(weighted_distances_cce, method = "ward")

# Alternative: Calculate distances using standard approach (no se weighting)

scaled_normal_cce <- scale(select(results_cce_clean, matches("^.*_est$")))

dist_matrix_normal_cce <- dist(scaled_normal_cce) #default method = "euclidean"

clustering_cce_normal <- agnes(dist_matrix_normal_cce, method = "ward")

# Dendrograms

dendo_cce <- fviz_dend(as.hclust(clustering_cce),
                       main = "Agglomerative Hierarchical Clustering of CCE Estimates (2000-2019)",
                       xlab = "Countries", ylab = "Height",
                       k = k, # Cut in groups
                       #palette = cluster_colors,
                       k_colors = cluster_colors[as.character(1:4)], # enforce custom palette order
                       cex = 0.75, # label size
                       rect = TRUE, # Add rectangle around groups
                       rect_fill = TRUE,
                       horiz = TRUE)

dendo_cce_normal <- fviz_dend(as.hclust(clustering_cce_normal),
                       main = "Agglomerative Hierarchical Clustering of CCE Estimates (2000-2019)",
                       xlab = "Countries", ylab = "Height",
                       k = k, # Cut in groups
                       palette = cluster_colors[as.character(1:4)],
                       #k_colors = cluster_colors[as.character(1:4)],
                       cex = 0.75, # label size
                       rect = TRUE, # Add rectangle around groups
                       rect_fill = TRUE,
                       horiz = TRUE)

# Factor maps

sub_grp_cce <- cutree(as.hclust(clustering_cce), k = k)

factor_map_cce_dim12 <- fviz_cluster(list(
  data = weighted_distances_cce,
  cluster = sub_grp_cce),
  repel = TRUE, # Avoid label overlapping
  show.clust.cent = TRUE, # Show cluster centers
  palette = cluster_colors,
  ggtheme = theme_minimal(),
  main = "Factor Map (Dim1 vs Dim2)") +
  labs(color = "Clusters") +
  guides(color = "none") +
  theme(legend.title = element_blank()
  ) #+ coord_fixed()

factor_map_cce_dim23 <- fviz_cluster(list(
  data = weighted_distances_cce,
  cluster = sub_grp_cce),
  repel = TRUE, # Avoid label overlapping
  show.clust.cent = TRUE, # Show cluster centers
  palette = cluster_colors,
  axes = c(2,3),
  ggtheme = theme_minimal(),
  main = "Factor Map (Dim2 vs Dim3)") +
  labs(color = "Clusters") +
  guides(color = "none") +
  theme(legend.title = element_blank()
  ) #+ coord_fixed()

dendo_factor_cce_rows <- ggpubr::ggarrange(
  factor_map_cce_dim12,
  factor_map_cce_dim23,
  ncol = 2
)

dendo_factor_cce <- ggpubr::ggarrange(
  dendo_cce,
  dendo_factor_cce_rows,
  ncol = 1, nrow = 2,
  heights = c(1, 1))

ggsave(here("output/FE_Clust_CCE.png"),
       dendo_factor_cce,
       width = 11, height = 13.53734, dpi = 300, bg = "white")

# The beta paramter has a direct meaning in the CCE model
# ($Y^k_{it} = \alpha^k_i + \beta^k_i \bar{Y}^k_t + \eta^k_{it}$)

cce_estimates <- results_cce_clean %>% 
  select(matches("^.*_est$")) %>% 
  mutate(Country = rownames(results_cce_clean))

cce_averages <- cce_estimates %>% 
  pivot_longer(cols = 1:12,
               names_to = "Variable", values_to = "Value") %>% 
  group_by(Country) %>% 
  # calculate mean value across als variables for every country
  summarise(`Average CCE` = mean(Value, na.rm = TRUE)) %>% 
  arrange(desc(`Average CCE`)) %>% 
  ungroup() %>% 
  mutate(d = `Average CCE` - dplyr::lead(`Average CCE`))

xtable(cce_averages, digits = 3)

# Further analysis of cce cluster results --------------------------------------

# Silhouette width to see how good each country fits in its assigned cluster
# compared to its "neighbor" cluster (i.e. the nearest one to which it does not
# belong)

sil_cce <- silhouette(sub_grp_cce, weighted_distances_cce)

rownames(sil_cce) <- attr(weighted_distances_cce, "Labels") # country names are too long for the plot
# use iso codes
rownames(sil_cce) <- countrycode(rownames(sil_cce), "country.name", "iso3c")

png(here("output/silhouette_widths_cce.png"), width = 6.5, height = 6.5, units = "in", res = 300)
#width = 650, height = 400)
plot(sil_cce,
     col = c(#"#00468B"
       "#925E9F","#ED0000", "#0099B4", #"#AD002AFF", "#004", 
       "#42B540"#Mongolia has no...
       #...silhouette width because it is a single-unit cluster, therefore ...
       #...no need for a color "#004"
     ),
     #col = pal_lancet("lanonc")(4)[1:4], 
     main = "Silhouette Plot of Cluster Results 
Clustering of CCE Estimates (2000-2019)")
dev.off()

# Cut has taken place at which height?

heights_cce <- clustering_cce$height

cut_height_cce <- sort(heights_cce, decreasing = TRUE)[k-1]
# decreasing order: first element shows cut height to create 2 groups,
# n-th element shows cut height to create n+1 groups - that's why k-1 here!
cut_height_cce

# Step plot showing merger heights

df_ordered_cce <- data.frame(
  Number_of_clusters = 2:(length(heights_cce) + 1),
  Height = sort(heights_cce, decreasing = TRUE)
)

fine_dash <- c(2, 2)

step_plot_cce <- ggplot(df_ordered_cce, aes(x = Number_of_clusters, y = Height)) +
  geom_step(color = pal_lancet("lanonc")(9)[4], linewidth = 0.5) +
  # Horizontal part
  annotate("segment", x = 2, xend = k, y = cut_height_cce, yend = cut_height_cce,
           color = pal_lancet("lanonc")(9)[4],
           linetype = fine_dash, linewidth = 0.5) +
  
  # Vertical part
  annotate("segment", x = k, xend = k, y = cut_height_cce, yend = sort(heights_cce)[1],
           color = pal_lancet("lanonc")(9)[4],
           linetype = fine_dash, linewidth = 0.5) +
  # Emphasize the corner with a point
  annotate("point", x = k, y = cut_height_cce, size = 2.5, shape = 21,
           fill = pal_lancet("lanonc")(9)[4], color = "black", stroke = 0.6) +
  
  # Label the number of clusters at the corner
  #annotate("text", x = k + 0.3, y = cut_height + 0.5, 
  #        label = paste0("g = ", k),
  #       hjust = 0, vjust = 0,
  #      size = 4.2, fontface = "bold",
  #     color = "black",
  #    color = pal_lancet("lanonc")(9)[3]
  #   ) +
  #geom_hline(yintercept = cut_height, 
  #           color = pal_lancet("lanonc")(9)[2], 
  #           linetype = "dashed", linewidth = 1) +
  labs(
    title = "Merger Heights in Dendrogram",
    subtitle = "Clustering of CCE Estimates (2000-2019)",
    x = "Number of Clusters",
    y = "Height"
  ) +
  scale_x_continuous(breaks = 1:max(df_ordered_cce$Number_of_clusters),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16),
                     expand = expansion(mult = c(0, 0.05))) +
  theme_minimal() +
  theme(
    #panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_blank(),  # turn off default lines
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),
    axis.line.y.left = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),  # add ticks
    axis.ticks.length = unit(0.1, "cm"),  # size of ticks
    axis.text = element_text(color = "black", size = 12),
    panel.border = element_blank(),  # remove full border
    panel.grid.minor = element_blank()
  )
# ????scale_x_reverse to emphasize the agglomertaive nature of the clustering

step_plot_cce

ggsave(here("output/step_plot_cce.png"), 
       step_plot_cce,
       width = 6.5, height = 4, dpi = 300, units = "in", bg = "white")

# Gap statistic

gap_stat_cce <- clusGap(
  as.matrix(weighted_distances_cce), 
  FUN = hcut, # hierarchical cut (not a partition method like kmeans)
  K.max = 10, # maximum number of clusters
  B = 100, # number of bootstraps
)

maxSE(gap_stat_cce$Tab[, "gap"],
      gap_stat_cce$Tab[, "SE.sim"],
      method = "firstmax") 
#1 with firstSEmax, 5 with globalSEmax, 3 with firstmax 

gap_stat_plot_cce <- fviz_gap_stat(gap_stat_cce,
                               linecolor = pal_lancet("lanonc")(9)[4]
) +
  labs(
    title = "Gap Statistic for Estimating Optimal Number of Clusters",
    subtitle = "Clustering of CCE Estimates (2000-2019)",
    x = "Number of Clusters",
    y = "Gap Statistic"
  ) +
  theme_minimal() +
  theme(
    #panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_blank(),  # turn off default lines
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),
    axis.line.y.left = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),  # add ticks
    axis.ticks.length = unit(0.1, "cm"),  # size of ticks
    axis.text = element_text(color = "black", size = 12),
    panel.border = element_blank(),  # remove full border
    panel.grid.minor = element_blank()
  )

gap_stat_plot_cce

ggsave(here("output/gap_stat_plot_cce.png"),
       gap_stat_plot_cce,
       width = 6.5, height = 6.5, dpi = 300, units = "in", bg = "white")

# silhouette plot (comparing average silhouette widths for different cluster
# solutions)

silhouette_plot_cce <- fviz_nbclust(
  as.matrix(weighted_distances_cce), hcut, method = "silhouette",
  linecolor = pal_lancet("lanonc")(9)[4]) +
  labs(
    title = "Silhouette Method for Estimating Optimal Number of Clusters",
    subtitle = "Clustering of CCE Estimates (2000-2019)",
    x = "Number of Clusters",
  ) +
  theme_minimal() +
  theme(
    #panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_blank(),  # turn off default lines
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),
    axis.line.y.left = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),  # add ticks
    axis.ticks.length = unit(0.1, "cm"),  # size of ticks
    axis.text = element_text(color = "black", size = 12),
    panel.border = element_blank(),  # remove full border
    panel.grid.minor = element_blank()
  )

silhouette_plot_cce

ggsave(here("output/silhouette_plot_cce.png"),
       silhouette_plot_cce,
       width = 6.5, height = 4, dpi = 300, units = "in", bg = "white")
