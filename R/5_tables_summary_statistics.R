rm(list = ls())
library(here)
source(here("packages.R"))

# Note: The LaTeX tables created here with xtable() do not 100% represent the 
# LaTeX tables in the Thesis, as further changes were made directly in LaTeX! 

EA3 <- c(#"Brunei", # missing data (ECI)
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

ISO_list_EA3 <- c("TWN", "CHN", "HKG", "IDN", "JPN", "KOR", "MYS", "PHL", "SGP",
                  "THA", "VNM", "MNG", "MMR", "KHM", "LAO")

# Country Summary Tables (from data_prep.R) ----------------------------------

load(here("data/macro_summary_years.RData"))
load(here("data/macro_summary_years_EA.RData"))
load(here("data/macro_summary_stat.RData"))
load(here("data/macro_summary_stat_EA.RData")) 
load(here("data/macro_summary_stat_important.RData"))
load(here("data/country_summary_table.RData"))
load(here("data/country_summary_table_EA.RData"))

# Agnes Cluster Results --------------------------------------------------------

load(here("output/cluster_results/FE_Clust_EA_selection_All Variables.RData"))
#load(here("output/cluster_results/FE_Clust_EA_2000_2019.RData")) # identical

# Summary tables ---------------------------------------------------------------

imporant_var_names <- c(
  "Unemp",
  "XinPercGDP", # Exports of Goods and Services (% of GDP)
  #"GDPgrowth",
  "GDPpcPPP",
  #"GDPpcPPPDivFromMean", #GDP per capita (Deviation from Sample Mean) 
  #"LaborShare", # missing data 
  "CAinPercGDP", # Current Account Balance (% of GDP)
  "DebtPercGDP",
  "FinanceShareVA", # Finance & Insurance Share in GVA
  "ManufacturingShareVA",
  "AgricultureShareVA",
  "MiningShareVA",
  "GiniMkt",
  "FDInetinflow", 
  #"FDIoutflow", # Using FDI in- or outflows does hardly change the results
  # (high correlation coefficient between the two variables ~ 0.74)
  #"FDIabsolute",
  "ECI",
  "LibDem" 
)

# Table with Country and years covered for all variables

years <- summary_years_EA %>% 
  select(-c("Brunei", "Macao SAR China", "Timor-Leste")) %>% 
  rename(Myanmar = `Myanmar (Burma)`,
         `Hong Kong` = `Hong Kong SAR China`,
         Variable = Country) %>% 
  filter(Variable %in% imporant_var_names) %>%
  pivot_longer(-Variable, names_to = "Country", values_to = "Value") %>%
  pivot_wider(names_from = Variable, values_from = Value) %>% 
  rename(`GDP p.c. at PPP (Dev. from Mean)` = GDPpcPPP,
         `Public Debt (% of GDP)` = DebtPercGDP,
         `Gini on Market Income` = GiniMkt,
         `Economic Complexity Index` = ECI,
         `Current Account Balance (% of GDP)` = CAinPercGDP,
         `Exports of Goods and Services (% of GDP)` = XinPercGDP,
         `Finance & Insurance Share in GVA` = FinanceShareVA,
         `Manufacturing Share in GVA` = ManufacturingShareVA,
         `Agriculture Share in GVA` = AgricultureShareVA,
         `Mining Share in GVA` = MiningShareVA,
         `FDI Net Inflows (% of GDP)` = FDInetinflow,
         #`Absolute FDI net flows (% of GDP)` = FDIabsolute,
         `Unemployment` = Unemp,
         `Liberal Democracy Index` = LibDem)

# as latex talbe with package xtable

xtable(years, caption = "Summary of years covered for all variables", digits = 0)

# which country/variable is not convered in the period 2000-2019?

# Function to extract the start and end years
extract_years <- function(range) {
  years <- str_extract_all(range, "\\d{4}") %>% unlist() %>% as.numeric()
  if (length(years) == 2) return(years)  # If both start and end years exist
  return(c(NA, NA))  # If not a valid range, return NA
}

# Check coverage for each country-variable pair
missing_coverage <- years %>%
  pivot_longer(-Country, names_to = "Variable", values_to = "Data Availability") %>%
  mutate(
    StartYear = map_dbl(`Data Availability`, ~extract_years(.x)[1]),
    EndYear = map_dbl(`Data Availability`, ~extract_years(.x)[2]),
    Covers_2000_2019 = ifelse(is.na(`Data Availability`) | 
                                StartYear > 2000 | 
                                EndYear < 2019, 
                              FALSE, TRUE)) %>%
  filter(!Covers_2000_2019 | is.na(`Data Availability`)) %>%  # Also filters completely missing data (NA)
  select(Country, Variable, `Data Availability`)

# as LaTeX table

xtable(missing_coverage, digits = 0)

# Table with Variable names, data sources and a comment section

source_list <- list(
  "Public Debt (% of GDP)" = list(
    "Source" = "Global Debt Databse (IMF)",
    "Comments" = "Data covers general government debt for all countries, except Laos, Myanmar, Hong Kong, and Singapore for which only central government debt is available."
  ),
  "Exports of Goods and Services (% of GDP)" = list(
    "Source" = "World Development Indicators, World Bank",
    "Comments" = "Data for Taiwan is from the National Statistics of Taiwan."
  ),
  "GDP p.c. at PPP (Dev. from Mean)" = list(
    "Source" = "IMF World Economic Outlook October 2024",
    "Comments" = "GDP per capita data, current prices in international dollars at PPP, deviation from sample average based on own calculation."
  ),        
  "Unemployment" = list(
    "Source" = "World Development Indicators, World Bank",
    "Comments" = "Data for Taiwan from IMF World Economic Outlook October 2024"
  ),                            
  "Current Account Balance (% of GDP)" = list(
    "Source" = "World Development Indicators, World Bank",
    "Comments" = "Data for Taiwan from IMF World Economic Outlook October 2024"
  ),      
  "Finance & Insurance Share in GVA" = list(
    "Source" = "Compiled from various sources, see Appendix A",
    "Comments" = "Own calculation of shares based on data sources, see Appendix A for the list of sources used for each country."
  ),        
  "Manufacturing Share in GVA" = list(
    "Source" = "Compiled from various sources, see Appendix A",
    "Comments" = "Own calculation of shares based on data sources, see Appendix A for the list of sources used for each country."
  ),              
  "Agriculture Share in GVA" = list(
    "Source" = "Compiled from various sources, see Appendix A",
    "Comments" = "Own calculation of shares based on data sources, see Appendix A for the list of sources used for each country."
  ),                
  "Mining Share in GVA" = list(
    "Source" = "Compiled from various sources, see Appendix A",
    "Comments" = "Own calculation of shares based on data sources, see Appendix A for the list of sources used for each country."
  ),                     
  "Gini on Market Income" = list(
    "Source" = "Standardized World Income Inequality Database",
    "Comments" = ""
  ),                   
  "FDI Net Inflows (% of GDP)" = list(
    "Source" = "UN Trade and Development",
    "Comments" = ""
  ),
  "Economic Complexity Index" = list(
    "Source" = "The Atlas of Economic Complexity",
    "Comments" = "Economic Complexity Index computed using SITC product classification"
  ),               
  "Liberal Democracy Index" = list(
    "Source" = "V-Dem Varieties of Democracy",
    "Comments" = ""
  ) 
)

source_df <- bind_rows(lapply(names(source_list), function(var) {
  data.frame(
    Variable = var,
    Source = source_list[[var]]$Source,
    Comments = source_list[[var]]$Comments,
    stringsAsFactors = FALSE
  )
}))

years_sources <- years %>% 
  pivot_longer(-Country, names_to = "Variable", values_to = "Data Availability") %>% 
  select(-c(Country, `Data Availability`)) %>% 
  unique() %>%
  left_join(source_df, by = "Variable")

xtable(years_sources, caption = "Data sources and comments for all variables", digits = 0)

# Table for Appendix A, detailing the gva share sources
# The table is created in LaTeX directly

gva_shares_sources_list <- list(
  "Finance & Insurance Share in GVA" = list(
    "Source" = "UN National Accounts Official Country Data (Table 2.4 Value added by industries at current prices, ISIC Rev. 4),
    Asian Development Bank (Gross value added at current prices),
    OECD (National Accounts at a Glance, Chapter 4: Production),
    OECD TiVA (Trade in value-added, indicator value added)
    ",
    "Comments" = "Own calculation of shares based on data sources, see 
    Appendix A for the list of sources used for each country."
  ),        
  "Manufacturing Share in GVA" = list(
    "Source" = "UN National Accounts Estimates of Main Aggregates
    (Gross Value Added by Kind of Economic Activity at current prices - 
    National currency,
    National Statistics of Taiwan,
    OECD TiVA (Trade in value-added, indicator value added)",
    "Comments" = "Own calculation of shares based on data sources, see 
    Appendix A for the list of sources used for each country."
  ),              
  "Agriculture Share in GVA" = list(
    "Source" = "UN National Accounts Estimates of Main Aggregates
    (Gross Value Added by Kind of Economic Activity at current prices - 
    National currency,
    National Statistics of Taiwan,
    National Statistics of China",
    "Comments" = "Own calculation of shares based on data sources, see 
    Appendix A for the list of sources used for each country."
  ),                
  "Mining Share in GVA" = list(
    "Source" = "OECD TiVA (Trade in value-added, indicator value added),
    UN National Accounts Official Country Data 
    (Table 2.4 Value added by industries at current prices, ISIC Rev. 4",
    "Comments" = "Own calculation of shares based on data sources, see 
    Appendix A for the list of sources used for each country."
  )
)

# Table with Variables and scaling factors w_k

#variables_w_k <- years %>%
#  pivot_longer(-Country, names_to = "Variable", values_to = "Data Availability") %>% 
#  select(Variable) %>% 
#  unique() %>% 
#  filter(Variable != "Liberal Democracy Index")

var_name_replacement <- c("GDPpcPPPDivFromMean" = "GDP p.c. at PPP (Dev. from Mean)",
                          "DebtPercGDP" = "Public Debt (% of GDP)",
                          "GiniMkt" = "Gini on Market Income",
                          "ECI" = "Economic Complexity Index",
                          "CAinPercGDP" = "Current Account Balance (% of GDP)",
                          "XinPercGDP" = "Exports of Goods and Services (% of GDP)",
                          "FinanceShareVA" = "Finance & Insurance Share in GVA",
                          "ManufacturingShareVA" = "Manufacturing Share in GVA",
                          "AgricultureShareVA" = "Agriculture Share in GVA",
                          "MiningShareVA" = "Mining Share in GVA",
                          "FDInetinflow" = "FDI Net Inflows (% of GDP)",
                          #"FDIabsolute" = "Absolute FDI flows (% of GDP)",
                          "Unemp" = "Unemployment")

w_k <- results$variable_weights %>% 
  select(variable, avg_weight) %>% 
  rename(Variable = variable,
         `Scaling Factor` = avg_weight) %>% 
  # recode the elements in column Variable
  mutate(Variable = recode(Variable, !!!var_name_replacement)) %>% 
  left_join(source_df, by = "Variable") %>% 
  select(-Comments)
  
xtable(w_k, digits = 4)

# Figure 1 ---------------------------------------------------------------------
# Population - Penn Penn World Table 10.0
# GDP p.c. PPP - IMF
# average GDP growth rate over the period 2000-2019 - IMF

pennRaw <- read_excel(here("data/raw/pwt1001.xlsx"),
                      sheet = "Data")

penn_pop <- pennRaw %>% 
  rename(`Population, in millions (2019)` = pop,
         Country = country,
         Year = year,
         ISO = countrycode) %>% 
  select(Country, ISO, Year, `Population, in millions (2019)`) %>% 
  filter(Year == 2019,
         ISO %in% ISO_list_EA3) %>% 
  mutate(Country = recode(Country,
                          "China, Hong Kong SAR" = "Hong Kong",
                          "Republic of Korea" = "South Korea",
                          "Lao People's DR" = "Laos",
                          "Viet Nam" = "Vietnam")) %>% 
  select(-c(ISO, Year))

load(here("data/raw/WEOvars.RData"))

WEO_GDPpc2019 <- WEOvars %>%
  select(ISO, Country, Year, GDPpcPPPimf) %>%
  filter(ISO %in% ISO_list_EA3,
         #Year == 2000) %>% 
         Year == 2019) %>%
  rename(`GDP p.c. at PPP, in 2017 intl. $ (2019)` = GDPpcPPPimf) %>% 
  #rename(`GDP p.c. at PPP, in 2017 intl. $ (2000)` = GDPpcPPPimf) %>% 
  select(Country, `GDP p.c. at PPP, in 2017 intl. $ (2019)`) %>% 
  #select(Country, `GDP p.c. at PPP, in 2017 intl. $ (2000)`) %>% 
  mutate(Country = recode(Country,
                          "Hong Kong SAR" = "Hong Kong",
                          "Korea" = "South Korea",
                          "Lao P.D.R." = "Laos",
                          "Taiwan Province of China" = "Taiwan"))

WEO_GDPpc2000 <- WEOvars %>%
  select(ISO, Country, Year, GDPpcPPPimf) %>%
  filter(ISO %in% ISO_list_EA3,
         Year == 2000) %>% 
  rename(`GDP p.c. at PPP, in 2017 intl. $ (2000)` = GDPpcPPPimf) %>% 
  select(Country, `GDP p.c. at PPP, in 2017 intl. $ (2000)`) %>% 
  mutate(Country = recode(Country,
                          "Hong Kong SAR" = "Hong Kong",
                          "Korea" = "South Korea",
                          "Lao P.D.R." = "Laos",
                          "Taiwan Province of China" = "Taiwan"))

growth_rate_data <- WEO_GDPpc2000 %>% 
  left_join(WEO_GDPpc2019) %>% 
  mutate(`Average annual log GDP p.c. growth (2000–2019, in %)` = 100*
           (log(`GDP p.c. at PPP, in 2017 intl. $ (2019)`) 
            - log(`GDP p.c. at PPP, in 2017 intl. $ (2000)`)) 
         / (2019 - 2000))
          
vdem_raw <- vdem

libdem_avg <- vdem_raw %>% 
  select(country_text_id, year, v2x_libdem) %>% 
  rename(ISO = country_text_id,
         Year = year,
         LibDem = v2x_libdem) %>% 
  filter(ISO %in% ISO_list_EA3) %>% 
  filter(ISO %in% ISO_list_EA3,
         Year >= 2000 & Year <= 2019) %>% 
  group_by(ISO) %>% 
  summarise(`Average Liberal Democracy Index (2000-2019)` =
              mean(LibDem, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(Country = countrycode(ISO, 
                               origin = "iso3c",
                               destination = "country.name")) %>% 
  mutate(Country = recode(Country,
                          "Hong Kong SAR China" = "Hong Kong",
                          "Myanmar (Burma)" = "Myanmar")) %>%
  select(Country, `Average Liberal Democracy Index (2000-2019)`) %>% 
  arrange(Country)

rm(vdem_raw)

convergence_bubble_data <- penn_pop %>% 
  left_join(growth_rate_data) %>%
  left_join(libdem_avg)

# Convergence bubble plot

convergence_bubble <- ggplot(convergence_bubble_data, aes(
  #x = `GDP p.c. at PPP, in 2017 intl. $ (2019)`,
  x = `GDP p.c. at PPP, in 2017 intl. $ (2000)`,
  y = `Average annual log GDP p.c. growth (2000–2019, in %)`,
  size = `Population, in millions (2019)`,
  color = `Average Liberal Democracy Index (2000-2019)`
)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(
    aes(label = Country),
    size = 3.5,
    nudge_x = 0.15,
    direction = "y",
    segment.color = "black",
    segment.size = 0.4,
    box.padding = 0.5,
    show.legend = FALSE
  ) +
  #scale_size_continuous(range = c(2, 15), guide = "legend") +
  scale_size_continuous(
    range = c(1, 15),
    breaks = c(5, 20, 100, 500, 1000),
    #labels = c("10m", "50m", "100m", "500m", "1b"),
    guide = "legend"
  ) +
  scale_color_gradientn(colors = c("#925E9F", "#ED0000", "#0099B4", "#42B540"))+
  #scale_color_viridis_c(option = "cividis") +
  scale_x_log10(labels = scales::comma,
                expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  labs(
    title = "Country sample",
    #x = "GDP p.c. at PPP, in 2017 intl. $ (log scale, 2019)",
    x = "GDP p.c. at PPP, in 2017 intl. $ (log scale, 2000)",
    y = "Average annual log GDP p.c. growth (2000–2019, in %)",
    size = "Population (millions, 2019)",
    color = "Liberal Democracy Index\n(2000–2019 avg.)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_blank(),
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),
    axis.line.y.left   = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.1, "cm"),
    axis.text = element_text(color = "black", size = 12),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

convergence_bubble

ggsave(here("output/convergence_bubble.pdf"),
       convergence_bubble,
       width = 11, height = 6, dpi = 300, units = "in", bg = "white")

ggsave(here("output/convergence_bubble.svg"),
       convergence_bubble,
       width = 11, height = 6, dpi = 300, units = "in", bg = "white")
