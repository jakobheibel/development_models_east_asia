rm(list = ls())
library(here)
source(here("packages.R"))

# Country Summary Tables (from 1_data_prep.R) ----------------------------------

#load(here("data/macro_summary_years.RData"))
#load(here("data/macro_summary_years_EA.RData"))
#load(here("data/macro_summary_stat.RData"))
#load(here("data/macro_summary_stat_EA.RData")) 
#load(here("data/macro_summary_stat_important.RData"))
#load(here("data/country_summary_table.RData"))
#load(here("data/country_summary_table_EA.RData"))

# Load and prepare macro_data --------------------------------------------------

load(here("data/macro_world.RData"))

# Countries to consider (select or de-selct with #)

EA2 <- c(#"Brunei", # missing data (ECI)
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

macro_EA2 <- macro_world %>% 
  filter(Country %in% EA2)

# Years to be considered

macro_EA2 <- macro_EA2 %>% 
  filter(Year >= 2000 
         & Year <= 2019
         )

# Variable definition (select or de-selct with #)

vars <- c(
  "Unemp",
  "XinPercGDP",
  "GDPgrowth",
  "GDPpcPPPDivFromMean",
  "LaborShare",
  "CAinPercGDP",
  "DebtPercGDP", 
  "FinanceShareVA",
  "ManufacturingShareVA",
  "AgricultureShareVA",
  "MiningShareVA",
  "GiniMkt",
  "FDInetinflow",
  "FDInetoutflow",
  "FDIabsolute",
  "KOFEcGI",
  "ECI",
  "LibDem"
)

# Use plotly to plot every variable for every country in the dataset -----------
# Check for availability, missing data, outliers etc.

# Proposed country groupings

periphery <- c("VNM", "MNG", "MMR", "KHM", "LAO", "IDN") # edge case IDN,  VNM??

financial_hubs <- c("HKG", "SGP")

developmental <- c("TWN", "JPN", "KOR")

emerging <- c("CHN", "MYS", "PHL", "THA")

east_asia <- c("CHN", "HKG", "MYS", "SGP", "TWN", "JPN", "KOR", 
               "MNG", "MMR", "VNM", "KHM", "LAO", "IDN", 
               "PHL", "THA")

# Group colors

group_colors <- c(
  "Periphery" = "#925E9F", 
  "Emerging" = "#ED0000", 
  "Developmental" = "#42B540", 
  "Finance" = "#0099B4"
)

plot_list <- list()

for (var in vars) {
  macro_EA_filtered <- macro_EA2 %>% 
    filter(!is.na(!!sym(var))) %>%
    mutate(Cluster = case_when(
      ISO %in% periphery ~ "Periphery",
      ISO %in% financial_hubs ~ "Finance",
      ISO %in% developmental ~ "Developmental",
      ISO %in% emerging ~ "Emerging",
      TRUE ~ "Other"))
  
  p <- ggplot(macro_EA_filtered, 
              aes(x = Year, 
                  y = !!sym(var), 
                  color = Cluster, 
                  group = Country)) +
    geom_line(size = 1) +
    scale_color_manual(values = group_colors) +
    theme_minimal() +
    labs(title = paste0(var, " Over Time"),
         x = "Year",
         y = var,
         color = "Country")
  
  p_interactive <- ggplotly(p) %>%
    layout(legend = list(title = list(text = "Country")))
  
  # Store in the list
  plot_list[[var]] <- p_interactive
}

# Save plot_list 

save(plot_list, file = here("data/plots_all_Variables.RData"))

# Fiding problems in the data
plot_list[["GDPpcPPPDivFromMean"]] 

for (var in vars) {
  print(plot_list[[var]])
}

# Correlation between Variables ------------------------------------------------

# Correlation between FDI in- and outflows

cor(macro_EA2 %>% 
      filter(!is.na(FDInetinflow) & !is.na(FDInetoutflow)) %>% 
      select(FDInetinflow, FDInetoutflow))  # ~ 0.739

cor(macro_EA2 %>% 
      filter(!is.na(ECI) & !is.na(GiniMkt)) %>% 
      select(ECI, GiniMkt)) # ~ 0.309

numeric_data <- macro_EA2 %>% 
  select_if(is.numeric) %>% 
  select(-c(Year, GDPpcPPP, GDPpcPPPimf, GDPgrowth, LaborShareDivFromMean, 
            CAinPercGDPDivFromMean, DebtPercGDPDivFromMean, 
            GiniMktDivFromMean, FinanceShareVADivFromMean, FDInetoutflow,
            FDInetoutflowWB, FDInetinflowWB, FDIabsoluteWB, XinPercGDPWB,
            XinPercGDPTWN, XinPercGDPWITS, KOFEcGI, CapAccGross,
            LaborShare, FDIabsolute)) %>% 
  rename(
    `GDP p.c., PPP (deviation from sample mean)` = GDPpcPPPDivFromMean,
    #`Labor share of income` = LaborShare,
    `Unemployment rate` = Unemp,
    `Exports of goods and services (% of GDP)` = XinPercGDP,
    `Current account balance (% of GDP)` = CAinPercGDP,
    `Public debt (% of GDP)` = DebtPercGDP,
    `Finance share of gross value added` = FinanceShareVA,
    `Manufacturing share of gross value added` = ManufacturingShareVA,
    `Agriculture share of gross value added` = AgricultureShareVA,
    `Mining share of gross value added` = MiningShareVA,
    `Gini on market income` = GiniMkt,
    `FDI net inflows (% of GDP)` = FDInetinflow,
    #`Absolute FDI net flows (% of GDP)` = FDIabsolute,
    `Economic Complexity Index` = ECI,
    `Liberal Democracy Index` = LibDem
  )

# Calculate correlation matrix with pairwise complete observations

cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

png(here("output/correlation_plot.png"),
    width = 11, height = 6, units = "in", res = 300)
    #width = 1800, height = 1200, res = 150)
corrplot(cor_matrix, method = "color", type = "lower", 
         tl.col = "black", tl.srt = 45)
dev.off()

# Which variable pairs exhibit the highest and lowest correlation coefficients?
# (measured across the panel)

extreme_values <- as.data.frame(as.table(cor_matrix)) %>% 
  filter(as.character(Var1) < as.character(Var2)) %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq) %>% 
  arrange(desc(abs(Correlation)))

xtable(head(extreme_values, 10), digits = 3)
