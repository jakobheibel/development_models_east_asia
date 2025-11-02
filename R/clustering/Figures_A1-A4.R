rm(list = ls())
library(here)
source(here("packages.R"))
source(here("R/clustering/utils/clusters_functions.R"))

# Calculating and Visualizing all cluster results across multiple periods ------
# also creates sankey diagram
# takes vector which includes all the time periods 
# in the form c(start_year, end_year, start_year, end_year, start_year ...)

# Countries to consider (select or de-selct with #)

EA <- c(#"Brunei", # missing data (ECI)
        "China", 
        "Hong Kong", 
        #"Macao", # missing data (ECI)
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
        #"Timor-Leste", # missing data, founded in 2002
        "Taiwan")

# Variables to consider (select or de-select with #)

var_names <- c(
  "Unemp",
  "XinPercGDP",
  #"GDPgrowth",
  "GDPpcPPPDivFromMean", 
  #"LaborShare", # not enough data
  "CAinPercGDP",
  "DebtPercGDP", 
  "FinanceShareVA",
  "ManufacturingShareVA",
  "AgricultureShareVA",
  "MiningShareVA",
  "GiniMkt",
  "FDInetinflow", 
  #"FDInetoutflow",
  #"FDIabsolute",
  "ECI"#,
  #"LibDem" # Changes Indonesia, Vietnam and China when included
)

# Time periods to consider

time_periods <- c(#2000, 2006, # 1st start_year, 1st end_year
                  #2004, 2010, # 2nd start_year, ...
                  #2000, 2009, # 10 year period
                  #2005, 2014, # 10 years
                  #2010, 2019, # 10 years
                  2000, 2004, # 5 year period
                  #2000, 2002,
                  #2002, 2004,
                  #2004, 2006,
                  #2006, 2008,
                  #2008, 2010,
                  #2010, 2012,
                  #2012, 2014,
                  #2014, 2017,
                  #2017, 2019
                  2003, 2007, # 5 years
                  2006, 2010, # 5 years
                  2009, 2014, # 5 years
                  2012, 2016, # 5 years
                  2015, 2019, # 5 years
                  2000, 2019  # 20 years
                  )
# Selection for every single year
#time_periods <- c(2000, 2001, 2001, 2002, 2002, 2003, 2003, 2004, 2004, 2005,
#                  2005, 2006, 2006, 2007, 2007, 2008, 2008, 2009, 2009, 2010,
#                  2010, 2011, 2011, 2012, 2012, 2013, 2013, 2014, 2014, 2015,
#                  2015, 2016, 2016, 2017, 2017, 2018, 2018, 2019)

# Number of country groupings

k <- 4

# Color palette for dendograms and factor map

#palette = "lancet"

cluster_colors <- c( # to ensure that that the colors of the dendogram match
  # those of the Sankey diagram and factor plot
  #"1" = "#00468B", 
  "1" = "#925E9F",
  "2" = "#ED0000", 
  "4" = "#42B540", 
  "3" = "#0099B4"#, 
  #"5" = "#925E9F"
)

# Load and prepare macro_data --------------------------------------------------

load(here("data/macro_world.RData"))

macro_world <- macro_world %>% 
  select(-ISO) %>% 
  pivot_longer(cols = -c(Country, Year),
               names_to = "variable_name", values_to = "value")

macro_EA <- macro_world %>% 
  filter(Country %in% EA)

# Create clusters --------------------------------------------------------------

dendo_all <- list() # For storing all dendograms
country_groupings <- list() # For storing all country groupings

for (i in seq(1, length(time_periods), by = 2)) {
  start_year <- time_periods[i]
  end_year <- time_periods[i + 1]
  
  # Looping through the time periods with the function period_cluster
  
  results <- period_cluster(macro_EA = macro_EA, 
                            var_names = var_names, 
                            start_year = start_year, 
                            end_year = end_year, 
                            k = k, method = "ward",
                            palette = cluster_colors)
  
  assign(paste0("results_", start_year, "_", end_year), results)
  
  save(results, 
       file = here(paste0("output/cluster_results/FE_Clust_EA_",
                          start_year, "_", end_year, ".RData")))
  
  # Create a list of dendograms
  
  dendo <- #plot(
    get(
      paste0(
        "results_", time_periods[i], "_", time_periods[i + 1]
        )
      )$dendo
  #)
  
  assign(paste0("dendo_", start_year, "_", end_year), dendo)
  
  ggsave(here(paste0("output/dendograms/FE_Clust_EA_", 
                     start_year, "_", 
                     end_year, ".png")), 
         dendo, 
         width = 8, height = 6, dpi = 300)
  
  dendo_all[[paste0("dendo_", start_year, "_", end_year)]] <- dendo
  
  # Create a list of country groupings for the Sankey diagram
  
  groupings <- get(
    paste0(
      "results_", time_periods[i], "_", time_periods[i + 1]
      )
    )$groupings
  
  group_name <- paste0(start_year, "-", end_year)
  
  country_groupings[[group_name]] <- groupings
  
}

# Plot all dendograms

all_dendos <- gridExtra::grid.arrange(grobs = dendo_all, ncol = 2)

# Create Sankey diagram --------------------------------------------------------

create_sankey(country_groupings, time_periods, cluster_colors)

# Further inquiries into the clustering results --------------------------------

agnes_results <- 
  get(
    paste0(
      "results_", time_periods[1], "_", 
      time_periods[length(time_periods)]
      )
    )$agnes_results

# get the original weighted distances used to produce agnes_results

weighted_distances <- get(
  paste0(
    "results_", time_periods[1], "_", 
    time_periods[length(time_periods)]
  )
)$weighted_distances

# Create a histogram to show where the cut in k groups has taken place
# (for the main dendrogram encompassing the complete period)

# Cut has taken place at which height?

heights <- agnes_results$height

cut_height <- sort(heights, decreasing = TRUE)[k-1]
# decreasing order: first element shows cut height to create 2 groups,
# n-th element shows cut height to create n+1 groups - that's why k-1 here!
cut_height

df_heights <- data.frame(heights = heights)

histogram <- ggplot(df_heights, aes(x = heights)) +
  geom_histogram(bins = 30, 
                 fill = pal_lancet("lanonc")(9)[4],  # just pick one color
                 color = "black", 
                 alpha = 0.8) +
  geom_vline(xintercept = cut_height, 
             color = pal_lancet("lanonc")(9)[2], 
             linewidth = 1.2, 
             linetype = "dashed") +
  labs(
    title = "Height of Mergers in Dendrogram",
    x = "Height of Mergers",
    y = "Number of Mergers"
  ) +
  theme_minimal() +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

histogram

df_ordered <- data.frame(
  Number_of_clusters = 2:(length(heights) + 1),
  Height = sort(heights, decreasing = TRUE)
)

fine_dash <- c(2, 2)

step_plot <- ggplot(df_ordered, aes(x = Number_of_clusters, y = Height)) +
  geom_step(color = pal_lancet("lanonc")(9)[4], linewidth = 0.5) +
  # Horizontal part
  annotate("segment", x = 2, xend = k, y = cut_height, yend = cut_height,
           color = pal_lancet("lanonc")(9)[4],
           linetype = fine_dash, linewidth = 0.5) +
  
  # Vertical part
  annotate("segment", x = k, xend = k, y = cut_height, yend = sort(heights)[1],
           color = pal_lancet("lanonc")(9)[4],
           linetype = fine_dash, linewidth = 0.5) +
  # Emphasize the corner with a point
  annotate("point", x = k, y = cut_height, size = 2.5, shape = 21,
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
    subtitle = "Clustering of FE Estimates (2000-2019)",
    x = "Number of Clusters",
    y = "Height"
  ) +
  scale_x_continuous(breaks = 1:max(df_ordered$Number_of_clusters),
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

step_plot

ggsave(here("output/step_plot.pdf"), 
       step_plot,
       width = 6.5, height = 4, dpi = 300, units = "in", bg = "white")

ggsave(here("output/step_plot.svg"), 
       step_plot,
       width = 6.5, height = 4, dpi = 300, units = "in", bg = "white")
       #width = 8, height = 8, dpi = 300, bg = "white")

# Gap statistic to evaluate the optimal number of clusters

set.seed(123)
gap_stat <- clusGap(
  as.matrix(weighted_distances), 
  FUN = hcut, # hierarchical cut (not a partition method like kmeans)
  K.max = 10, # maximum number of clusters
  B = 100, # number of bootstraps
)

maxSE(gap_stat$Tab[, "gap"],
      gap_stat$Tab[, "SE.sim"],
      method = "firstSEmax")  # k = 4 according to the firstSEmax method
# "This, the default, has been proposed by Martin Maechler in 2012, when 
# adding clusGap() to the cluster package, after having seen the "globalSEmax" 
# proposal (in code) and read the "Tibs2001SEmax" proposal." 
# (quote from cluster package documentation, retrieved from help ?clusGap())

gap_stat_plot <- fviz_gap_stat(gap_stat,
                               linecolor = pal_lancet("lanonc")(9)[4]
                               ) +
  labs(
    title = "Gap Statistic for Estimating Optimal Number of Clusters",
    subtitle = "Clustering of FE Estimates (2000-2019)",
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

gap_stat_plot

ggsave(here("output/gap_stat_plot.pdf"),
       gap_stat_plot,
       width = 6.5, height = 4, dpi = 300, units = "in", bg = "white")

ggsave(here("output/gap_stat_plot.svg"),
       gap_stat_plot,
       width = 6.5, height = 4, dpi = 300, units = "in", bg = "white")

# further metrics 

# elbow (minimizing within-cluster variance while also keeping the number of
# clusters small so as to avoid overfitting)

elbow_plot <- fviz_nbclust(
  as.matrix(weighted_distances), hcut, method = "wss",
  linecolor = pal_lancet("lanonc")(9)[4]) +
  labs(
    title = "Elbow Method for Estimating Optimal Number of Clusters",
    subtitle = "Clustering of FE Estimates (2000-2019)",
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

elbow_plot

ggsave(here("output/elbow_plot.pdf"),
       elbow_plot,
       width = 6.5, height = 4, dpi = 300, units = "in", bg = "white")

ggsave(here("output/elbow_plot.svg"),
       elbow_plot,
       width = 6.5, height = 4, dpi = 300, units = "in", bg = "white")

# indicating a solution with 3 or perhaps 4 clusters