rm(list = ls())
library(here)
source(here("packages.R"))
source(here("R/clustering/utils/clusters_functions.R"))

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
  `Take One Out` = var_names[var_names != "ECI"],
  `Excl. Sector Shares` = var_names[var_names != "AgricultureShareVA" & 
                                    var_names != "ManufacturingShareVA" & 
                                    var_names != "MiningShareVA" &
                                      var_names != "FinanceShareVA"],
  `Excl. Sector Shares + ECI` = var_names[var_names != "AgricultureShareVA" & 
                                                  var_names != "ManufacturingShareVA" & 
                                                  var_names != "MiningShareVA" &
                                                  var_names != "FinanceShareVA" &
                                            var_names != "ECI"])

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
  "3" = "#0099B4"
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

# Figure 8: Sankey diagram -----------------------------------------------------

ggsave(here(paste0("output/FE_Sankey_EA_selection.pdf")), 
       sankey_variable_selection, 
       width = 11, height = 6, dpi = 300, bg = "white")

ggsave(here(paste0("output/FE_Sankey_EA_selection.svg")), 
       sankey_variable_selection, 
       width = 11, height = 6, dpi = 300, bg = "white")