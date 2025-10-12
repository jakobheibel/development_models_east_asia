rm(list = ls())
library(here)
source(here("packages.R"))

ISO_list_EA <- c("BRN", "MAC", "TWN", "CHN", "HKG", "IDN", "JPN", "KOR", "MYS",
                 "PHL", "SGP", "THA", "VNM", "MNG", "MMR", "KHM", "LAO", "TLS")

# ILO has data for labor share of income ---------------------------------------

#ILOraw <- get_ilostat(id = "SDG_1041_NOC_RT_A", segment = "indicator")
#save(ILOraw, file = here("data/raw/ILO_labour_share.RData"))

load(here("data/raw/ILO_labour_share.RData"))

ILO <- ILOraw %>% 
  mutate(Country = countrycode(ref_area,
                               origin = "iso3c",
                               destination = "country.name")) %>% 
  rename(ISO = ref_area,
         Year = time,
         LaborShareILO = obs_value) %>% 
  select(-c(source, indicator, obs_status)) %>% 
  mutate(LaborShareILO = LaborShareILO/100) %>% 
  mutate(Year = as.integer(Year)) %>% 
  mutate(Country = ifelse(ISO == "CHA", "Channel Island", Country)) %>%
  filter(!is.na(Country))

# World Bank has data for ------------------------------------------------------
# Exports of goods and services (% of GDP) - Indicator: NE.EXP.GNFS.ZS
# Current account balance (% of GDP) - Indicator: BN.CAB.XOKA.GD.ZS
# GDP growth (annual %) - Indicator: NY.GDP.MKTP.KD.ZG
# GDP per capita, PPP (const 2021 int $) - Indicator: NY.GDP.PCAP.PP.KD
# Unemployment (% of total labor force) - Indicator: SL.UEM.TOTL.ZS 
# Foreign direct investment, net inflows (% of GDP)(BX.KLT.DINV.WD.GD.ZS)
# Foreign direct investment, net outflows (% of GDP)(BM.KLT.DINV.WD.GD.ZS)

#WBApr2025 <- wb_data(c("NE.EXP.GNFS.ZS",
#                       "BN.CAB.XOKA.GD.ZS",
#                       "NY.GDP.MKTP.KD.ZG",
#                       "NY.GDP.PCAP.PP.KD",
#                       "SL.UEM.TOTL.ZS",
#                       "NV.SRV.TOTL.ZS",
#                       "BX.KLT.DINV.WD.GD.ZS",
#                       "BM.KLT.DINV.WD.GD.ZS"
#                       ))

#save(WBApr2025, file = here("data/raw/WBApr2025.RData"))

load(here("data/raw/WBApr2025.RData"))

WB <- WBApr2025 %>% 
  rename(Country = country) %>% 
  rename(Year = date,
         ISO = iso3c,
         XinPercGDPWB = NE.EXP.GNFS.ZS,
         CAinPercGDPWB = BN.CAB.XOKA.GD.ZS,
         GDPgrowthWB = NY.GDP.MKTP.KD.ZG,
         GDPpcPPPwb = NY.GDP.PCAP.PP.KD,
         UnempWB = SL.UEM.TOTL.ZS,
         ServPercGDP = NV.SRV.TOTL.ZS,
         FDInetinflowWB = BX.KLT.DINV.WD.GD.ZS,
         FDInetoutflowWB = BM.KLT.DINV.WD.GD.ZS
         ) %>% 
  mutate(FDIabsoluteWB = abs(FDInetinflowWB)) %>% 
  select(-iso2c)

# Taiwan XinPercGDP 

twnX_raw <- read_excel(here("data/raw/taiwan_national_accounts.xlsx"))

twnX <- twnX_raw %>% 
  mutate(XinPercGDPTWN = XinPercGDP*100) %>% 
  select(Year, ISO, XinPercGDPTWN) %>%
  mutate(Country = "Taiwan")

#WB <- bind_rows(WB, twnX)

# Trade Indicators from WITS (World Bank) --------------------------------------

wits_raw <- fread(here("data/raw/wits_exports.csv"))

wits <- wits_raw %>% 
  rename(ISO = ReporterISO3,
         Country = ReporterName,
         XinPercGDPWITS = `Exports (% of GDP)`) %>% 
  select(c(ISO, Country, Year, XinPercGDPWITS))

# IMF Dataset World Economic Outlook October 2024 includes data for ------------
# CA balance (% of GDP), Real GDP growth, Unemployment rate, 
# GDP per capita data:
# Gross domestic product per capita, constant prices,
# unit: Purchasing power parity; 2017 international dollar

WEOOct2024 <- fread(here("data/raw/WEOOct2024all.csv"))

WEOvars_raw <- WEOOct2024 %>% 
  filter(`WEO Subject Code` %in% c("BCA_NGDPD", 
                                   "NGDP_RPCH",
                                   "NGDPRPPPPC",
                                   #"PPPPC",
                                   "LUR")) %>% 
  mutate(across(10:59, ~ as.double(gsub(",", "", .))))

for (i in 1:nrow(WEOvars_raw)) {  # removing all the estimates
  if (!is.na(WEOvars_raw$`Estimates Start After`[i])) {
    start_year <- WEOvars_raw$`Estimates Start After`[i] + 1
    end_year <- 2029
    for (year in start_year:end_year) {
      year_col <- as.character(year)
      if (year_col %in% colnames(WEOvars_raw)) {
        WEOvars_raw[i, year_col] <- NA
      }
    }
  }
}

WEOvars <- WEOvars_raw %>% 
  select(-c(55:59)) %>% 
  pivot_longer(cols = 10:54,
               names_to = "Year",
               values_to = "Values") %>%
  mutate(Year = as.integer(Year)) %>%
  select(-c(`WEO Country Code`, `Subject Descriptor`, `Subject Notes`, Units,
            Scale, `Country/Series-specific Notes`, `Estimates Start After`)) %>% 
  pivot_wider(names_from = `WEO Subject Code`,
              values_from = "Values") %>% 
  rename("CAinPercGDPIMF" = "BCA_NGDPD") %>% 
  rename("GDPgrowthIMF" = "NGDP_RPCH") %>% 
  rename("UnempIMF" = "LUR") %>% 
  rename("GDPpcPPPimf" = "NGDPRPPPPC")

save(WEOvars, file = here("data/raw/WEOvars.RData"))

# FDI (% of GDP) from UNCTAD ---------------------------------------------------
# https://unctadstat.unctad.org/datacentre/dataviewer/US.FdiFlowsStock

fdi_raw <- fread(here("data/raw/fdi_in_outflows_un.csv"))

fdi <- fdi_raw %>% 
  rename(Country = Economy_Label,
         FDInetinflow = Inward_Percentage_of_Gross_Domestic_Product_Value,
         FDInetoutflow = Outward_Percentage_of_Gross_Domestic_Product_Value) %>%
  filter(!(Country == "Indonesia" & Year <= 2002),
         !(Country == "Indonesia (...2002)" & Year > 2002)) %>%
  mutate(Country = ifelse(
    Country == "Indonesia (...2002)", "Indonesia", Country)) %>% 
  select(Country, Year, FDInetinflow, FDInetoutflow) %>%
  mutate(ISO = countrycode(Country,
                           origin = "country.name",
                           destination = "iso3c")) %>% 
  mutate(ISO = ifelse(Country == "Kosovo", "UVK", ISO)) %>% 
  mutate(FDIabsolute = abs(FDInetinflow))

# Global Debt Database (IMF) for debt data (general gov debt % of GDP) ---------
# Use ISO Code UVK for Kosovo (according to IMF WEO Database)

GDD <- read_excel(here("data/raw/general_gov_debt.xls"),
                  na = "no data")

GDD <- GDD[2:89,]

generalDebt <- GDD %>% 
  rename(Country = `General Government Debt (Percent of GDP)`) %>% 
  mutate(ISO = countryname(Country,
                           destination = "iso3c")) %>%
  mutate(ISO = ifelse(Country == "Kosovo", "UVK", ISO)) %>% 
  relocate(ISO, .after = Country) %>% 
  pivot_longer(cols = 3:76,
               names_to = "Year",
               values_to = "generalDebtPercGDP")

# Missing countries: Use central government debt (LAO, MMR, HKG, TLS, BRN, SGP)
# Macau missing - that's because Macau has no government debt - just put zero?

centralGovDebt <- read_excel(here("data/raw/central_gov_debt.xls"),
                             na = "no data")

centralGovDebt <- centralGovDebt[2:175,]

centralDebtEA <- centralGovDebt %>% 
  rename(Country = `Central Government Debt (Percent of GDP)`) %>% 
  mutate(ISO = countryname(Country,
                           destination = "iso3c")) %>% 
  mutate(ISO = ifelse(Country == "Kosovo", "UVK", ISO)) %>% 
  relocate(ISO, .after = Country) %>% 
  pivot_longer(cols = 3:76,
               names_to = "Year",
               values_to = "centralDebtPercGDP") %>% 
  filter(ISO %in% c("LAO", "MMR", "HKG", "TLS", "BRN", "SGP")) 
# LAO, MMR, HKG, TLS, BRN, SGP are the countries where central gov debt is used
# if general gov debt is not available! 
# coalescing data for THA and KOR may be problematic, as using only central debt
# may underestimate general debt

debt <- full_join(generalDebt, centralDebtEA)

debt <- debt %>% 
  mutate(DebtPercGDP = coalesce(generalDebtPercGDP, centralDebtPercGDP)) %>% 
  mutate(Year = as.integer(Year))

#debtMAC <- data.frame(Country = "Macau", ISO = "MAC", Year = 1990:2024, DebtPercGDP = 0)

#debt <- bind_rows(debt, debtMAC)

# Penn World Table provides data -----------------------------------------------
# ...on population (pop) and real GDP (rgdpna): calculating GDP per capita ppp
# ...on real capital stock (rnna)
# ...on labour share of income (labsh)
# Can be used to calculate:
# net capital accumulation rate (using share of gross capital formation at
# current PPPs (csh_i), average depreciation rate of the capital stock (delta), 
# together with real GDP

pennRaw <- read_excel(here("data/raw/pwt1001.xlsx"),
                   sheet = "Data")

penn <- pennRaw %>% 
  mutate(GDPpcPPPpenn = rgdpna/pop) %>%  #Real GDP at constant 2017 national prices (in mil. 2017 US$)!
  mutate(GFCF = csh_i*rgdpna) %>% #gross fixed capital formation
  mutate(NFCF = GFCF - (delta*rnna)) %>%  #net fixed capital formation
  mutate(CapAccGross = GFCF/rnna) %>% #gross capital accumulation
  mutate(CapAccNet = NFCF/rnna) %>% #net capital formation
  select(countrycode, country, year, CapAccGross, GDPpcPPPpenn, labsh) %>% 
  rename(LaborShare = labsh,
         Country = country,
         Year = year,
         ISO = countrycode)

# Value added shares ----------------------------------------------------------- 

load(here("data/raw/value_added_shares.RData"))

value_added_shares <- value_added_shares_EA %>% 
  #select(-c(`Source`)) %>% 
  rename(FinanceShareVA = finance_share_VA,
         ManufacturingShareVA = manufacturing_share_VA,
         MiningShareVA = mining_share_VA,
         AgricultureShareVA = agric_share_VA) %>% 
  mutate(FinanceShareVA = 100*FinanceShareVA,
         ManufacturingShareVA = 100*ManufacturingShareVA,
         MiningShareVA = 100*MiningShareVA,
         AgricultureShareVA = 100*AgricultureShareVA)

# SWIID data -------------------------------------------------------------------

load(here("data/raw/swiid9_8/swiid9_8.rda"))

swiid_data <- swiid_summary %>% 
  rename(Country = country,
         Year = year,
         GiniMkt = gini_mkt) %>% 
  mutate(ISO = countrycode(Country,
                           origin = "country.name",
                           destination = "iso3c")) %>% 
  select(Country, ISO, Year, GiniMkt)

# Economic Complexity Index ----------------------------------------------------

eci_raw <- fread(here("data/raw/eci_v6/rankings.tab"))

eci <- eci_raw %>% 
  mutate(ISO = countrycode(country_id,
                           origin = "un",
                           destination = "iso3c")) %>% 
  # Some values were not matched unambiguously: 158 
  # See UN: "Q: Why is Taiwan Province of China not shown in the M49?"
  # https://unstats.un.org/unsd/methodology/m49/
  # 158 is Taiwan
  mutate(ISO = ifelse(country_id == 158, "TWN", ISO)) %>% 
  rename(#ECI = sitc_eci_rank,
         ECI = sitc_eci,
         #ECI = hs_eci,  
         Year = year) %>%
  select(ISO, Year, ECI)

# V-Dem data -------------------------------------------------------------------

vdem_raw <- vdem

libdem <- vdem_raw %>% 
  select(country_text_id, year, v2x_libdem) %>% 
  rename(ISO = country_text_id,
         Year = year,
         LibDem = v2x_libdem)

rm(vdem_raw)

# KOF economic globalization index ---------------------------------------------

kof_raw <- read_excel(here("data/raw/KOFGI_2024_public.xlsx"))

KOFEcGI <- kof_raw %>% 
  select(code, year, KOFEcGI) %>% 
  rename(ISO = code,
         Year = year)
  
# Merging all datasets ---------------------------------------------------------

custom_iso_mapping <- c( 
      "XKX" = "Kosovo",
      "CHA" = "Channel Islands",
      "EAS" = "East Asia", 
      "ECS" = "Europe and Central Asia", 
      "HIC" = "High-Income Countries", 
      "LCN" = "Latin America and the Caribbean", 
      "LIC" = "Low-Income Countries", 
      "LMC" = "Lower Middle-Income Countries", 
      "MEA" = "Middle East and North Africa", 
      "NAC" = "North America", 
      "SAS" = "South Asia", 
      "SSF" = "Sub-Saharan Africa", 
      "UMC" = "Upper Middle-Income Countries", 
      "WBG" = "West Bank and Gaza", 
      "WLD" = "World")

iso_replacements <- c("ZAR" = "COD", "ADO" = "AND", "ROM" = "ROU", "TMP" = "TLS", "UVK" = "XKX", "CHI" = "CHA")

# List of datasets to merge
datasets <- list(WEOvars, 
                 penn, 
                 WB, 
                 ILO, 
                 debt, 
                 value_added_shares, 
                 swiid_data, 
                 fdi,
                 eci,
                 libdem,
                 twnX,
                 wits,
                 KOFEcGI)

# Function to clean ISO and Year columns
clean_data <- function(df) {
  df %>%
    filter(!is.na(ISO) & !is.na(Year)) %>%  # Remove rows with NA in ISO or Year
    mutate(ISO = as.character(ISO),         # Ensure ISO is character
           Year = as.integer(Year))         # Ensure Year is integer
}

# Clean each dataset
datasets <- lapply(datasets, clean_data)

# Merge all datasets
macro_world <- reduce(datasets, full_join, by = c("Year", "ISO"))

# Recode ISO codes
macro_world <- macro_world %>%
  mutate(ISO = recode(ISO, !!!iso_replacements))

# Remove existing Country columns
macro_world <- macro_world %>%
  select(-matches("Country"))

# Convert ISO codes to country names
macro_world <- macro_world %>%
  mutate(Country = countrycode(ISO, origin = "iso3c", destination = "country.name"))

# Apply custom ISO mappings
macro_world <- macro_world %>%
  mutate(Country = coalesce(custom_iso_mapping[ISO], Country))

# Relocate Country column
macro_world <- macro_world %>%
  relocate(Country, .before = ISO)

# Remove duplicate rows
macro_world <- macro_world %>%
  distinct(Year, ISO, .keep_all = TRUE)

# Which data source to use? ----------------------------------------------------
# Comparing IMF and WB variables using the summary statistics tables 
# GDPgrowth - coalesce GDPgrowthWB and GDPgrowthIMF
# Unemp - WB data, IMF data for Taiwan
# FDIturnover - just use UN data
# CAinPercGDP - WB data for all countries, except Taiwan (IMF)
# XinPercGDP - WITS, WB, what about Taiwan? - see comments below
# ILO and Penn data for Labour Share of income - use PWT data, consistent methdology across countries

# which FDI net inflow?

FDInetinflow <- macro_world %>% 
  select(ISO, Year, FDInetinflowWB, FDInetinflow) %>% 
  filter(ISO %in% ISO_list_EA)

FDInetinflow_long <- FDInetinflow %>%
  pivot_longer(
    cols = starts_with("FDInetinflow"),
    names_to = "Source",
    values_to = "FDInetinflow"
  )

p_FDInetinflow <- ggplot(FDInetinflow_long, 
                    aes(x = Year, 
                        y = FDInetinflow, 
                        color = Source)) +
  geom_line() +
  facet_wrap(~ ISO, scales = "free_y") +
  labs(title = "FDI Net Inflows by Source",
       x = "Year", y = "FDI Net Inflow") +
  theme_minimal()

ggplotly(p_FDInetinflow) %>%
  layout(legend = list(title = list(text = "Source")))
# use UN data (FDInetinflow)

# which XinPercGDP?

XinPercGDP <- macro_world %>% 
  select(ISO, Year, XinPercGDPWITS, XinPercGDPTWN, XinPercGDPWB) %>% 
  filter(ISO %in% ISO_list_EA)

XinPercGDP_long <- XinPercGDP %>%
  pivot_longer(
    cols = starts_with("XinPercGDP"),
    names_to = "Source",
    values_to = "XinPercGDP"
  )

p_XinPercGDP <- ggplot(XinPercGDP_long, 
                    aes(x = Year, 
                        y = XinPercGDP, 
                        color = Source)) +
  geom_line() +
  facet_wrap(~ ISO, scales = "free_y") +
  labs(title = "Exports in % of GDP by Source",
       x = "Year", y = "Exports in % of GDP") +
  theme_minimal()

ggplotly(p_XinPercGDP) %>%
  layout(legend = list(title = list(text = "Source")))
# WITS and WB data identical (of course both is WB)
# but WITS includes some data for Myanmar
# WB has more years for some countries
# use WB data, WITS for Myanmar
# use Taiwan National Statistics data for Taiwan

# Which CAinPercGDP?

CAinPercGDP <- macro_world %>% 
  select(ISO, Year, CAinPercGDPWB, CAinPercGDPIMF) %>% 
  filter(ISO %in% ISO_list_EA)

CAinPercGDP_long <- CAinPercGDP %>%
  pivot_longer(
    cols = starts_with("CAinPercGDP"),
    names_to = "Source",
    values_to = "CAinPercGDP"
  )

p_GDPpcPPP <- ggplot(CAinPercGDP_long, 
                    aes(x = Year, 
                        y = CAinPercGDP, 
                        color = Source)) +
  geom_line() +
  facet_wrap(~ ISO, scales = "free_y") +
  labs(title = "Current Account Balance by Source",
       x = "Year", y = "CA in % of GDP") +
  theme_minimal()

ggplotly(p_GDPpcPPP) %>% 
  layout(legend = list(title = list(text = "Source")))
# WB data for all countries, IMF data for Taiwan

# Which GDPpc PPP?

GDPpcPPP <- macro_world %>% 
  select(ISO, Year, GDPpcPPPwb, GDPpcPPPimf, GDPpcPPPpenn) %>% 
  filter(ISO %in% ISO_list_EA)

GDPpcPPP_long <- GDPpcPPP %>% 
  pivot_longer(
    cols = starts_with("GDPpcPPP"),
    names_to = "Source",
    values_to = "GDPpcPPP"
  )

p_GDPpcPPP <- ggplot(GDPpcPPP_long, 
                    aes(x = Year, 
                        y = GDPpcPPP, 
                        color = Source)) +
  geom_line() +
  facet_wrap(~ ISO, scales = "free_y") +
  labs(title = "GDP by Source",
       x = "Year", y = "GDP pc PPP") +
  theme_minimal()

ggplotly(p_GDPpcPPP) %>% 
  layout(legend = list(title = list(text = "Source")))

# Which gdp growth (WB or IMF)?

GDPgrowth <- macro_world %>% 
  select(ISO, Year, GDPgrowthWB, GDPgrowthIMF) %>% 
  filter(ISO %in% ISO_list_EA)

GDPgrowth_long <- GDPgrowth %>%
  pivot_longer(
    cols = starts_with("GDPgrowth"),
    names_to = "Source",
    values_to = "GDPgrowth"
  )

p_GDPgrowth <- ggplot(GDPgrowth_long, 
                    aes(x = Year, 
                        y = GDPgrowth, 
                        color = Source)) +
  geom_line() +
  facet_wrap(~ ISO, scales = "free_y") +
  labs(title = "GDP Growth by Source",
       x = "Year", y = "GDP Growth") +
  theme_minimal()

ggplotly(p_GDPgrowth) %>%
  layout(legend = list(title = list(text = "Source")))
# just IMF data

# IMF has the best coverage (Gross domestic product per capita, current prices, PPP)

macro_world <- macro_world %>% 
  mutate(GDPgrowth = GDPgrowthIMF) %>% 
  mutate(Unemp = UnempWB) %>%
  mutate(Unemp = ifelse(Country == "Taiwan", UnempIMF, Unemp)) %>%
  mutate(XinPercGDP = ifelse(Country == "Taiwan", XinPercGDPTWN,
                             ifelse(ISO == "MMR", XinPercGDPWITS,
                                    XinPercGDPWB))) %>% 
  mutate(CAinPercGDP = ifelse(Country == "Taiwan", CAinPercGDPIMF, CAinPercGDPWB)) %>% 
  #mutate(LaborShare = 100* (coalesce(LaborSharePenn, LaborShareILO))) %>% 
  mutate(GDPpcPPP = GDPpcPPPimf)

# remove regional aggreagtes ---------------------------------------------------

ISOlistNonRegionalAggregates <- c("WLD", "EAS", "ECS", "LCN", "MEA", "NAC", 
                                  "SAS", "SSF", "HIC", "LIC", "LMC", "UMC")
macro_world <- macro_world %>% 
  filter(!ISO %in% ISOlistNonRegionalAggregates)

# Create deviations from sample average of variables that show a trend ---------
# (by year)

# Variables that show a trend

trend_vars <- c("GDPpcPPP", 
                "LaborShare", 
                "CAinPercGDP",
                "DebtPercGDP",
                "GiniMkt",
                "FinanceShareVA")

macro_world <- macro_world %>%
  group_by(Year) %>%
  mutate(across(all_of(trend_vars),
                list(
                  SampleMean = ~mean(.x, na.rm = TRUE),
                  DivFromMean = ~.x - mean(.x, na.rm = TRUE)
                ),
                .names = "{.col}{.fn}"
  )) %>%
  ungroup()

# Descriptive summary statistic------------------------------------------------

VarList <- c(#"GDPgrowthIMF",
             #"UnempIMF",
             #"CAinPercGDPIMF",
             #"CAinPercGDPWB",
             "DebtPercGDP",
             "XinPercGDP",
             #"ServPercGDP",
             #"GDPgrowthWB",
             #"GDPpcPPPwb",
             "GDPpcPPP",
             #"UnempWB",
             #"LaborShareILO",
             #"LaborSharePenn",
             "LaborShare",
             "GDPgrowth",
             "Unemp",
             "CAinPercGDP",
             #"CapAccGross",
             "FinanceShareVA",
             "ManufacturingShareVA",
             "AgricultureShareVA",
             "MiningShareVA",
             "GiniMkt",
             "FDInetinflow",
             "FDInetoutflow",
             "FDIabsolute",
             "ECI",
             "LibDem",
             "KOFEcGI")

VarListImportant <- c("GDPgrowth",
                      "Unemp",
                      "CAinPercGDP",
                      "DebtPercGDP",
                      "XinPercGDP",
                      "LaborShare",
                      "GDPpcPPP",
                      "FinanceShareVA",
                      "ManufacturingShareVA",
                      "AgricultureShareVA",
                      "MiningShareVA",
                      "GiniMkt",
                      "FDInetinflow",
                      "FDInetoutflow",
                      "FDIabsolute",
                      "ECI",
                      "LibDem",
                      "KOFEcGI")

EA_all <- c("Brunei", "China", "Hong Kong SAR China", "Macao SAR China", 
        "South Korea", "Mongolia", "Myanmar (Burma)", "Philippines", "Thailand",
        "Singapore", "Malaysia", "Vietnam", "Cambodia", "Laos", "Japan", 
        "Indonesia", "Timor-Leste", "Taiwan")

get_year_range <- function(variable) {
  macro_world %>%
    filter(!is.na(.data[[variable]])) %>%
    group_by(Country) %>%
    summarize(YearRange = paste(min(Year), max(Year), sep = "-"), .groups = "drop") %>%
    select(Country, YearRange)
}

summary_years_list <- list()

for (var in VarList) {
  summary_years_list[[length(summary_years_list) + 1]] <- get_year_range(var)
}

summary_years <- summary_years_list %>% 
  reduce(full_join, by = "Country")

colnames_summary_years <- c("Country", VarList)

names(summary_years) <- colnames_summary_years

summary_years <- data.table::transpose(summary_years, keep.names = "Variable")

colnames(summary_years) <- summary_years[1, ]

summary_years <- summary_years[-1, ]

#summary_years <- rename(summary_years, Variable = Country)

save(summary_years, file = here("data/macro_summary_years.RData"))

summary_years_EA <- summary_years %>% 
  select(Country, all_of(EA_all))

save(summary_years_EA, file = here("data/macro_summary_years_EA.RData"))

# Summary statistics for each variable ----------------------------------------

# World Level

summary_stat <- macro_world %>% 
  summarise(across(
    all_of(VarList), #list of variables
    list(Mean   = ~mean(.x, na.rm = TRUE),
         SD     = ~sd(.x, na.rm = TRUE), 
         Min    = ~min(.x, na.rm = TRUE), 
         Max    = ~max(.x, na.rm = TRUE),
         Median = ~median(.x, na.rm = TRUE),
         IQR    = ~IQR(.x, na.rm = TRUE),
         Q1     = ~quantile(.x, probs = 0.25, na.rm = TRUE),
         Q3     = ~quantile(.x, probs = 0.75, na.rm = TRUE),
         NonNAobservations  = ~sum(!is.na(.x)))))%>% 
  pivot_longer(everything(), 
               names_to = c("variable", "statistic"), 
               names_sep = "_") %>%
  pivot_wider(names_from = variable, values_from = value)

summary_stat <- data.table::transpose(summary_stat, keep.names = "Variable")

colnames(summary_stat) <- summary_stat[1, ]

summary_stat <- summary_stat[-1, ]

summary_stat <- rename(summary_stat, Variable = statistic)

summary_stat <- summary_stat %>% 
  mutate(across(-Variable, as.numeric))

# Calculate the number of countries for each variable

no_countries <- macro_world %>%
  summarise(across(
    all_of(VarList),
    ~n_distinct(Country[!is.na(.x)])
  )) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "NoCountries")

# Add NoCountries column

summary_stat <- summary_stat %>%
  left_join(no_countries, by = c("Variable" = "variable"))

save(summary_stat, file = here("data/macro_summary_stat.RData"))

# Only East Asia

summary_stat_EA <- macro_world %>% 
  filter(Country %in% EA_all) %>% 
  summarise(across(
    all_of(VarList), #list of variables
    list(Mean   = ~mean(.x, na.rm = TRUE),
         SD     = ~sd(.x, na.rm = TRUE), 
         Min    = ~min(.x, na.rm = TRUE), 
         Max    = ~max(.x, na.rm = TRUE),
         Median = ~median(.x, na.rm = TRUE),
         IQR    = ~IQR(.x, na.rm = TRUE),
         Q1     = ~quantile(.x, probs = 0.25, na.rm = TRUE),
         Q3     = ~quantile(.x, probs = 0.75, na.rm = TRUE),
         NonNAobservations  = ~sum(!is.na(.x)))))%>% 
  pivot_longer(everything(), 
               names_to = c("variable", "statistic"), 
               names_sep = "_") %>%
  pivot_wider(names_from = variable, values_from = value)

summary_stat_EA <- data.table::transpose(summary_stat_EA, keep.names = "Variable")

colnames(summary_stat_EA) <- summary_stat_EA[1, ]

summary_stat_EA <- summary_stat_EA[-1, ]

summary_stat_EA <- rename(summary_stat_EA, Variable = statistic)

summary_stat_EA <- summary_stat_EA %>% 
  mutate(across(-Variable, as.numeric))

# Calculate the number of countries for each variable

no_countries_EA <- macro_world %>%
  filter(Country %in% EA_all) %>%
  summarise(across(
    all_of(VarList),
    ~n_distinct(Country[!is.na(.x)])
  )) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "NoCountries")

# Add NoCountries column

summary_stat_EA <- summary_stat_EA %>%
  left_join(no_countries_EA, by = c("Variable" = "variable"))

save(summary_stat_EA, file = here("data/macro_summary_stat_EA.RData"))

# Summary statistics for important variables -----------------------------------

summary_stat_important <- macro_world %>% 
  summarise(across(
    all_of(VarListImportant), #list of variables
    list(Mean   = ~mean(.x, na.rm = TRUE),
         SD     = ~sd(.x, na.rm = TRUE), 
         Min    = ~min(.x, na.rm = TRUE), 
         Max    = ~max(.x, na.rm = TRUE),
         Median = ~median(.x, na.rm = TRUE),
         IQR    = ~IQR(.x, na.rm = TRUE),
         Q1     = ~quantile(.x, probs = 0.25, na.rm = TRUE),
         Q3     = ~quantile(.x, probs = 0.75, na.rm = TRUE),
         NonNAobservations  = ~sum(!is.na(.x)))))%>% 
  pivot_longer(everything(), 
               names_to = c("variable", "statistic"), 
               names_sep = "_") %>%
  pivot_wider(names_from = variable, values_from = value)

summary_stat_important <- data.table::transpose(summary_stat_important, keep.names = "Variable")

colnames(summary_stat_important) <- summary_stat_important[1, ]

summary_stat_important <- summary_stat_important[-1, ]

summary_stat_important <- rename(summary_stat_important, Variable = statistic)

summary_stat_important <- summary_stat_important %>% 
  mutate(across(-Variable, as.numeric))

# Calculate the number of countries for each variable

no_countries_important <- macro_world %>%
  summarise(across(
    all_of(VarListImportant),
    ~n_distinct(Country[!is.na(.x)])
  )) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "NoCountries")

# Add NoCountries column

summary_stat_important <- summary_stat_important %>%
  left_join(no_countries_important, by = c("Variable" = "variable"))

save(summary_stat_important, file = here("data/macro_summary_stat_important.RData"))

# Observations and year range for respective countries in the dataset ----------

# Function to get the number of observations and year range for each variable and country
get_summary <- function(df, variable) {
  df %>%
    filter(!is.na(.data[[variable]])) %>%
    group_by(Country) %>%
    summarise(
      Observations = n(),
      YearRange = paste(min(Year), max(Year), sep = "-"),
      .groups = "drop"
    ) %>%
    mutate(Variable = variable)
}

# Apply the function to each variable in VarListImportant
summary_list <- lapply(VarListImportant, 
                       function(var) get_summary(macro_world, var))

summary_list_EA <- lapply(VarListImportant, 
                          function(var) get_summary(macro_world %>% filter(Country %in% EA_all), var))

# Combine the results into a summary table
country_summary_table <- bind_rows(summary_list)

country_summary_table_EA <- bind_rows(summary_list_EA)

save(country_summary_table, file = here("data/country_summary_table.RData"))
save(country_summary_table_EA, file = here("data/country_summary_table_EA.RData"))

# complete countries (not so important!??)

complete_countries <- country_summary_table %>%
  group_by(Country) %>%
  summarise(Count = n()) %>%
  filter(Count == 12)

missing_data <- setdiff(macro_world$Country, complete_countries$Country)

# Complete cases: Countries for which all important variables are available ----

# Create a logical condition to check for non-missing values for all important variables
macro_world_complete <- macro_world %>%
  filter(across(all_of(VarListImportant), ~ !is.na(.x)))

# Find the countries with complete data for all important variables
countries_with_complete_data <- macro_world_complete %>%
  select(Country) %>%
  distinct()

# Remove unimportant variables -------------------------------------------------

macro_world <- macro_world %>%
  select(-c("GDPgrowthIMF",
            "UnempIMF",
            "CAinPercGDPIMF",
            "CAinPercGDPWB",
            "UnempWB",
            "LaborShareILO",
            "ServPercGDP",
            "GDPgrowthWB",
            "GDPpcPPPwb",
            "GDPpcPPPpenn",
            "generalDebtPercGDP",
            "centralDebtPercGDP",
            "GDPpcPPPSampleMean",
            "LaborShareSampleMean",
            "CAinPercGDPSampleMean",
            "DebtPercGDPSampleMean",
            "GiniMktSampleMean",
            "FinanceShareVASampleMean"))

# Customize country names ------------------------------------------------------

macro_world <- macro_world %>%
  mutate(Country = recode(Country,
                          "Myanmar (Burma)" = "Myanmar",
                          "Hong Kong SAR China" = "Hong Kong",
                          "Macao SAR China" = "Macao"
                          ))
  
# Save the data ----------------------------------------------------------------

save(macro_world, file = here("data/macro_world.RData"))

save(macro_world_complete, file = here("data/macro_world_complete.RData"))
