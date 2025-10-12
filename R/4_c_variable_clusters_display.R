rm(list = ls())
library(here)
source(here("packages.R"))
source(here("R/4_a_clusters_functions.R"))

# Calculating and Visualizing all cluster results (different sets of vars ------
# also creates sankey diagram
# takes list which includes all the sets of variables

# Countries to consider (select or de-selct with #)

EA <- c(#"Brunei", 
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
        #"Timor-Leste", # missing data, foundend in 2002
        "Taiwan")

# Years to be considered

start_year = 2000
end_year = 2019

# Variable sets 

var_names <- c(
  "Unemp",
  "XinPercGDP", # Exports of Goods and Services (% of GDP)
  #"GDPgrowth",
  #"GDPpcPPP",
  "GDPpcPPPDivFromMean", #GDP per capita (Deviation from Sample Mean) 
  #"LaborShare", # missing data 
  "CAinPercGDP", # Current Account Balance (% of GDP)
  "DebtPercGDP",
  "FinanceShareVA", # Finance & Insurance Share in GVA
  "ManufacturingShareVA",
  "AgricultureShareVA",
  "MiningShareVA",
  "GiniMkt",
  "FDInetinflow", # divides HKG, SGP and Malaysia
  #"FDIoutflow", # Using FDI in- or outflows does hardly change the results
  # (high correlation coefficient between the two variables ~ 0.74)
  #"FDIabsolute",
  "ECI"#,
  #"LibDem" # Changes Indonesia, Vietnam and China when included
)

variable_list <- list(
  `All Variables` = var_names,
  #`Without ECI` = var_names[var_names != "ECI"],
  #`Without GDP p.c.` = var_names[var_names != "GDPpcPPPDivFromMean"],
  `Take One Out` = var_names[var_names != "ECI"],
  `Excl. Sector Shares` = var_names[var_names != "AgricultureShareVA" & 
                                    var_names != "ManufacturingShareVA" & 
                                    var_names != "MiningShareVA" &
                                      var_names != "FinanceShareVA"],
  `Excl. Sector Shares + ECI` = var_names[var_names != "AgricultureShareVA" & 
                                                  var_names != "ManufacturingShareVA" & 
                                                  var_names != "MiningShareVA" &
                                                  var_names != "FinanceShareVA" &
                                            var_names != "ECI"]#,
  #`Macroeconomic Variables` = c("Unemp", "GDPpcPPPDivFromMean", "CAinPercGDP", 
                     #   "XinPercGDP", "DebtPercGDP", "FDInetinflow")#, 
  #`Macroeconomic Variables plus ECI` = c("Unemp", "GDPpcPPPDivFromMean", 
  #                                      "CAinPercGDP", "XinPercGDP", "DebtPercGDP",
  #                                       "FDInetinflow", "ECI"),
  #`Macroeconomic Variables plus Gini` = c("Unemp", "GDPpcPPPDivFromMean", 
  #                                       "CAinPercGDP", "XinPercGDP", "DebtPercGDP",
  #                                       "FDInetinflow", "GiniMkt")
  #`GDP not as dev` = c("GDPpcPPP", "Unemp", "XinPercGDP", "CAinPercGDP", 
  #              "DebtPercGDP", "FinanceShareVA", "ManufacturingShareVA",
  #              "AgricultureShareVA", "MiningShareVA", "GiniMkt",
  #              "FDInetinflow", "ECI"), # GDPpcPPP instead of GDPpcPPPDivFromMean
  #`Without Gini` = var_names[var_names != "GiniMkt"],
  #`Without Unemp` = var_names[var_names != "Unemp"],
  #`Without Exports` = var_names[var_names != "XinPercGDP"],
  #`Without current ac` = var_names[var_names != "CAinPercGDP"],
  #`Without debt` = var_names[var_names != "DebtPercGDP"],
  #`Without FDI` = var_names[var_names != "FDInetinflow"],

  #`Without Exports (% of GDP)` = var_names[var_names != "XinPercGDP"],
  #`Without Finance Share` = var_names[var_names != "FinanceShareVA"],
  #`W O ECI & GDP p.c.` = var_names[var_names != "ECI" & 
  #                                 var_names != "GDPpcPPPDivFromMean"],
  #`ECI` = "ECI",
  #`GDP p.c. (Dev. from Mean)` = "GDPpcPPPDivFromMean",
  #`Exports (% of GDP)` = "XinPercGDP",
  #`GDP per capita (PPP)` = "GDPpcPPP",
  #`Important Variables` = c("FinanceShareVA", "XinPercGDP", 
  #                          "GDPpcPPPDivFromMean", "MiningShareVA", "GiniMkt",
  #                          "FDInetinflow", "AgricultureShareVA", 
  #                          "ManufacturingShareVA")
  #`Gini on Market Income` = "GiniMkt",
  #`Finance Share in GVA` = "FinanceShareVA"#,
  #`Current Account Balance (% of GDP)` = "CAinPercGDP"#,
  #`Mining Share in GVA` = "MiningShareVA",
  #`Unemployment rate` = "Unemp",
  #`FDI Net Inflows (% of GDP)` = "FDInetinflow",
  #`FDI Net Outflows (% of GDP)` = "FDInetoutflow",
  #`Absolute Net FDI flows (% of GDP)` = "FDIabsolute",
  #`Share of Agriculture in Value Added` = "AgricultureShareVA",
  #`Share of Manufacturing in Value Added` = "ManufacturingShareVA",
  #`Public Debt (% of GDP)` = "DebtPercGDP"
  #`Share of Labour Compensation in GDP` = "LaborShare",
  #`Sector Shares` = c("FinanceShareVA", "ManufacturingShareVA", 
  #                  "AgricultureShareVA", "MiningShareVA")#,
)

# Number of country groupings

k <- 4

# Color palette for dendograms and factor map

palette = "lancet"

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

for (i in seq_along(variable_list)) {
  
  var_set_name <- names(variable_list[i])
  
  var_set <- variable_list[[i]]
  
  # Looping through the variable selections with the function variable_cluster
  
  results <- variable_cluster(macro_EA = macro_EA,
                              var_names = var_names,
                              start_year = start_year,
                              var_set = var_set,
                              end_year = end_year, 
                              k = k, method = "ward",
                              palette = cluster_colors)
  
  print(paste0("Results for Variable Selection ", var_set_name, " created."))
  
  assign(paste0("results_selection_", var_set_name), results)
  
  save(results, 
       file = here(paste0("output/cluster_results/FE_Clust_EA_selection_",
                          var_set_name, ".RData")))
  
  # Create a list of dendograms
  
  dendo <- #plot(
    get(
      paste0(
        "results_selection_", var_set_name
      )
    )$dendo
  #)
  
  assign(paste0("dendo_selection_", var_set_name), dendo)
  
  ggsave(here(paste0("output/dendograms/FE_Clust_EA_selection_", 
                     var_set_name, ".png")), 
         dendo, 
         width = 8, height = 6, dpi = 300)
  
  dendo_all[[paste0("dendo_selection_", var_set_name)]] <- dendo
  
  # Create a list of country groupings for the Sankey diagram
  
  groupings <- get(
    paste0(
      "results_selection_", var_set_name
    )
  )$groupings
  
  group_name <- paste0(var_set_name)
  
  country_groupings[[group_name]] <- groupings
  
}

# Plot all dendograms

all_dendos <- gridExtra::grid.arrange(grobs = dendo_all, ncol = 2)

ggsave(here("output/dendograms/FE_Clust_EA_variable_selection_all.png"), 
       all_dendos, 
       width = 15, height = 11, dpi = 300)

# Create Sankey diagram --------------------------------------------------------

# Sankey shows groups with the "highest values" at the bottom
# exception: GDP p.c. and ECI, here the third-highest group is shown at the
# second place from below - manipulation is needed for visualization purposes
# e.g. GDP p.c. group THA and MYS is number 3 in 
# country_groupings$`GDP p.c. (Dev. from Mean)`
# but shown in the Sankey as group two from below 

country_groupings_manipulated <- country_groupings

# For GDP p.c., swap MYS, THA (group 3) with group 2 (HKG, JPN, TWN, KOR)

country_groupings_manipulated[["GDP p.c. (Dev. from Mean)"]] <-
  ifelse(country_groupings_manipulated[["GDP p.c. (Dev. from Mean)"]] == 3, 2,
         ifelse(country_groupings_manipulated[["GDP p.c. (Dev. from Mean)"]] == 2, 3,
                country_groupings_manipulated[["GDP p.c. (Dev. from Mean)"]]
         )
  )

# For ECI, swap IDN, VNM (group 3) with group 2 (CHN, HKG, MYS, THA, PHL)

country_groupings_manipulated[["ECI"]] <- 
  ifelse(country_groupings_manipulated[["ECI"]] == 3, 2,
         ifelse(country_groupings_manipulated[["ECI"]] == 2, 3,
                country_groupings_manipulated[["ECI"]]
         )
  )
         

sankey_variable_selection <- create_sankey_selection(country_groupings_manipulated, 
                                                     variable_list, 
                                                     cluster_colors)
plot(sankey_variable_selection)

# kmeans clustering

kmeans_results <- kmeans(
      `results_selection_All Variables`$weighted_distance, 
  centers = k)

factor_map_kmeans <- fviz_cluster(list(
  data = `results_selection_All Variables`$weighted_distance,
  cluster = kmeans_results$cluster),
  repel = TRUE, # Avoid label overlapping
  show.clust.cent = TRUE, # Show cluster centers
  palette = "Lancet", 
  ggtheme = theme_minimal(),
  main = "K-means Clustering of FE estimates")

# Save the main results --------------------------------------------------------
# (main dendogram and factor map already created and saved in 4_b_period_clusters_display.R)

# Sankey diagram 

ggsave(here(paste0("output/FE_Sankey_EA_selection.png")), 
       sankey_variable_selection, 
       width = 11, height = 6, dpi = 300, bg = "white")
