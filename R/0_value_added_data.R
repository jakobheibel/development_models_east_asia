library(here)
source(here("packages.R"))

# Gathering data for the share of value added of the following industries:
# (by NACE Rev. 2 / ISIC Rev. 4 classification)
# K - Financial and insurance activities
# B - Mining and quarrying
# C - Manufacturing
# A - Agriculture, forestry and fishing
# Total - all NACE activities

# World Bank data --------------------------------------------------------------
# has GVA data (total)
# some national accounts dataasets (e.g. ADB, UN) are missing total GVA values for some countries
# Gross value added at basic prices (GVA) (current LCU) (NY.GDP.FCST.CN)
# Manufacturing, value added (current LCU) (NV.IND.MANF.CN)
# Agriculture, forestry, and fishing, value added (current LCU)(NV.AGR.TOTL.CN)
# Industry (including construction), value added (current LCU)(NV.IND.TOTL.CN) 
# (Important: Industry also includes manufacturing!)
# Services, value added (current LCU)(NV.SRV.TOTL.CN)

#wb_gva_raw <- wb_data(c("NY.GDP.FCST.CN", 
#                        "NV.IND.MANF.CN",
#                        "NV.AGR.TOTL.CN",
#                        "NV.IND.TOTL.CN",
#                        "NV.SRV.TOTL.CN"))
#save(wb_gva_raw, file = here("data/raw/value added data/wb_gva_raw.RData"))
load(here("data/raw/value added data/wb_gva_raw.RData"))

wb_gva <- wb_gva_raw %>% 
  rename(ISO = iso3c,
         Year = date,
         GVA_wb = NY.GDP.FCST.CN,
         Manufacturing = NV.IND.MANF.CN,
         Agriculture = NV.AGR.TOTL.CN,
         Services = NV.SRV.TOTL.CN,
         Industry = NV.IND.TOTL.CN) %>%  # GVA in local curreny unit!
  mutate(GVA_sum = 
           ifelse(!is.na(Industry) & !is.na(Services) & !is.na(Agriculture),
                  Industry + Services + Agriculture,
                  GVA_wb)
  ) %>% 
  # WB calculates GVA as sum of GVA of industry, services and agriculture
  # however the sum does not always match the GVA value :((((
  # but the data basis is better if I use self-summed up GVA_sum!!!! 
  mutate(manufacturing_share_VA = Manufacturing / GVA_sum,
         agric_share_VA = Agriculture / GVA_sum,
         Source = "World Bank")


# UN Data (1) ------------------------------------------------------------------
# source:  National Accounts Official Country Data 
# Table 2.4 Value added by industries at current prices (ISIC Rev. 4)
# Filters: 
# "Agriculture, forestry and fishing, Mining and quarrying, 
# Manufacturing, Financial and insurance activities, 
# Equals: VALUE ADDED, GROSS, at basic prices"
# http://data.un.org/Data.aspx?d=SNA&f=group_code%3a204

un_raw <- fread(here("data/raw/value added data/UNdata_NA_Table2.4 neu.txt"),
                sep = ";")

un <- un_raw %>%
  filter(`Fiscal Year Type` == "Western calendar year")

now_missing <- setdiff(un_raw$`Country or Area`, un$`Country or Area`)

un_missing <- un_raw %>%
  filter(`Country or Area` %in% now_missing)

un <- bind_rows(un, un_missing)

duplicates <- un %>%
  group_by(`Country or Area`, Year, Item) %>%
  filter(n() > 1)

un <- un %>%
  group_by(`Country or Area`, Year, Item) %>%
  summarise(first(Value), .groups = "drop") %>% 
  pivot_wider(names_from = Item,
              values_from = `first(Value)`) 

# Separate rows for Tanzania - Mainland and Zanzibar
tanzania_data <- un %>%
  filter(`Country or Area` %in% c("Tanzania - Mainland", "Zanzibar"))

# Aggregate the values for Tanzania - Mainland and Zanzibar
tanzania_aggregated <- tanzania_data %>%
  group_by(Year) %>%
  summarise(across(2:5, sum, na.rm = TRUE)) %>%
  mutate(`Country or Area` = "Tanzania")

# Combine the aggregated data with the original data
un_combined <- un %>%
  filter(!`Country or Area` %in% c("Tanzania - Mainland", "Zanzibar")) %>%
  bind_rows(tanzania_aggregated) %>% 
  mutate(finance_share_VA = `Financial and insurance activities` /
           `Equals: VALUE ADDED, GROSS, at basic prices`,
         manufacturing_share_VA = `Manufacturing` /
           `Equals: VALUE ADDED, GROSS, at basic prices`,
         mining_share_VA = `Mining and quarrying` /
           `Equals: VALUE ADDED, GROSS, at basic prices`,
         agric_share_VA = `Agriculture, forestry and fishing` /
           `Equals: VALUE ADDED, GROSS, at basic prices`) %>% 
  mutate(ISO = countrycode(`Country or Area`, "country.name", "iso3c")) %>%
  rename(GVA = `Equals: VALUE ADDED, GROSS, at basic prices`) %>% 
  mutate(Source = "UN")

# UN data (2) ------------------------------------------------------------------
# source: National Accounts Estimates of Main Aggregates
# Gross Value Added by Kind of Economic Activity at current prices - 
# National currency
# https://data.un.org/Data.aspx?q=gross+value+added+datamart%5bSNAAMA%5d&d=SNAAMA&f=grID%3a201%3bcurrID%3aNCU%3bpcFlag%3a0

un_raw2 <- fread(here(
  "data/raw/value added data/un_gva_national_account_estimates.csv"))
 
un2 <- un_raw2 %>%
  mutate(ISO = countrycode(`Country or Area`, "country.name", "iso3c")) %>%
  rename(GVA = `Total Value Added`,
         Agriculture = `Agriculture, hunting, forestry, fishing (ISIC A-B)`,
         Manufacturing = `Manufacturing (ISIC D)`) %>% 
  mutate(Source = "UN NA Estimates",
         agric_share_VA = Agriculture / GVA,
         manufacturing_share_VA = Manufacturing / GVA)

# Eurostat data ----------------------------------------------------------------
# source: Annual National Accounts - 
# National accounts aggregates by industry (up to NACE A*64) (nama_10_a64)

estat_raw <- fread(here("data/raw/value added data/estat_nama_10_a64_filtered_en.csv"))

estat <- estat_raw %>% 
  pivot_wider(names_from = nace_r2,
              values_from = OBS_VALUE) %>%
  mutate(finance_share_VA = 
           `Financial and insurance activities` /
           `Total - all NACE activities`,
         mining_share_VA =
           `Mining and quarrying` /
           `Total - all NACE activities`,
         manufacturing_share_VA =
           `Manufacturing` /
           `Total - all NACE activities`,
         agric_share_VA =
           `Agriculture, forestry and fishing` /
           `Total - all NACE activities`) %>% 
  select(c(6, 7, 15:18)) %>% 
  rename("Year" = "TIME_PERIOD",
         "Country" = "geo") %>%
  filter(Country != "European Union - 27 countries (from 2020)") %>% 
  mutate(ISO = countrycode(Country, "country.name", "iso3c"))

# ADB data ---------------------------------------------------------------------
# source: ADB statistics - National Accounts - Gross Value at current prices

adb_raw <- fread(here("data/raw/value added data/adb_gva neu.csv"),
                 header = TRUE)

#adb_raw_EA <- adb_raw %>% 
#  mutate(ISO = countrycode(Economy, "country.name", "iso3c")) %>% 
#  filter(ISO %in% ISO_list_EA) %>% 
#  select(Economy, ISO, Indicator, Footnotes)

adb <- adb_raw %>%
  mutate(across(4:27, as.double)) %>%
  select(-c(3, 28:34)) %>% 
  pivot_longer(cols = 3:26,
               names_to = "Year",
               values_to = "value") %>%
  pivot_wider(names_from = Indicator,
              values_from = value) %>%
  rename("Country" = "Economy") %>%
  mutate(Year = as.integer(Year),
         GVA = `Gross value added at current prices, Total`,
         Manufacturing = `Manufacturing at current prices`,
         Mining_and_quarrying = `Mining and quarrying at current prices`,
         Financial_and_insurance = `Financial and insurance activities at current prices`,
         Agriculture = `Agriculture, forestry, and fishing at current prices`) %>%
  select(-c(3:7)) %>%
  mutate(finance_share_VA = Financial_and_insurance / GVA,
         mining_share_VA = Mining_and_quarrying / GVA,
         manufacturing_share_VA = Manufacturing / GVA,
         agric_share_VA = Agriculture / GVA) %>% 
  mutate(ISO = countrycode(Country, "country.name", "iso3c"))

# Add World Bank GVA_sum data to ADB data

adb_wb <- adb %>%
  left_join(
    wb_gva %>% select(ISO, Year, GVA_sum),
    by = c("ISO", "Year")) %>% 
  mutate(finance_share_VA = Financial_and_insurance / GVA_sum,
         mining_share_VA = Mining_and_quarrying / GVA_sum,
         manufacturing_share_VA = Manufacturing / GVA_sum,
         agric_share_VA = Agriculture / GVA_sum,
         Source = "adb plus wb gva_sum")

# OECD NAAG data ---------------------------------------------------------------
# source: OECD - NAAG (National Accounts at a Glance) Chapter 4: Production

oecd_raw <- fread(here("data/raw/value added data/oecd_naag neu.csv"))

oecd <- oecd_raw %>% 
  select(c(7, 12, 17, 19)) %>%
  pivot_wider(names_from = `Economic activity`,
              values_from = OBS_VALUE) %>%
  rename(ISO = REF_AREA,
         Year = TIME_PERIOD) %>% 
  mutate(finance_share_VA = `Financial and insurance activities` / 100,
         manufacturing_share_VA = `Manufacturing` / 100,
         agric_share_VA = `Agriculture, forestry and fishing` / 100) %>% 
  mutate(Country = countrycode(ISO, "iso3c", "country.name"))

# African Development Bank group data ------------------------------------------
# source: https://dataportal.opendataforafrica.org/ydixvvd - national accounts

afdb_raw <- fread(here("data/raw/value added data/ObservationData_nnqukw.csv"))

afdb <- afdb_raw %>% 
  pivot_wider(names_from = indicators,
              values_from = Value) %>%
  select(c(1, 3:8)) %>% 
  mutate(finance_share_VA = `Financial and insurance activities, value added` /
           `Gross Value Added (GVA) at Basic Prices`,
         mining_share_VA = `Mining and quarrying, value added` /
           `Gross Value Added (GVA) at Basic Prices`,
         manufacturing_share_VA = `Manufacturing, value added` /
           `Gross Value Added (GVA) at Basic Prices`,
         agric_share_VA = `Agriculture, forestry and fishing, value added` /
           `Gross Value Added (GVA) at Basic Prices`) %>% 
  rename(Country = `country-and-regions`,
         Year = Date) %>% 
  mutate(ISO = countrycode(Country, "country.name", "iso3c"))

# CEPALSTAT Data ---------------------------------------------------------------
# source: Annual Gross Domestic Product (AGD) by Activity at current prices in National Currency

cepal_raw <- read_excel(here("data/raw/value added data/cepalstat.xlsx"))

cepal <- cepal_raw %>% 
  row_to_names(row_number = 1) %>% # Use the first row as column names
  clean_names() %>% 
  rename(Country = na,
         Year = na_2) %>% 
      slice(-1) %>%
  mutate(Country = str_remove(Country, "\\s*\\[.*\\]")) %>% # Remove base year info
  fill(Country) %>%  # Fill down country names
  mutate(Year = as.integer(Year)) %>% 
  mutate(across(4:21, as.double)) %>% 
  select(-3) %>% 
  mutate(finance_share_VA = financial_intermediation / total_value_added,
         mining_share_VA = mining_and_quarrying / total_value_added,
         manufacturing_share_VA = manufacturing / total_value_added,
         agric_share_VA = agriculture_hunting_forestry_and_fishing / 
           total_value_added) %>% 
  mutate(Country = case_match(Country,
                              "Chili" ~ "Chile",
                              .default = Country)) %>% 
  mutate(ISO = countrycode(Country, "country.name", "iso3c")) %>% 
  filter(!is.na(Year))

# OECD TiVa data ---------------------------------------------------------------
# source: OECD - Trade in Value Added (TiVA) - Value added by industry
# https://www.oecd.org/en/tiva.html
# cf. Indicator guide (indicator used is VALU, Value added in millions USD)
# https://web-archive.oecd.org/2023-11-24/644737-TiVA_2023_Indicators_Guide.pdf

oecd_tiva_raw <- fread(here("data/raw/value added data/oecd_tiva neu.csv"))

oecd_tiva <- oecd_tiva_raw %>% 
  select(c(`REF_AREA`, `Economic activity`, `TIME_PERIOD`, `OBS_VALUE`)) %>% 
  pivot_wider(names_from = `Economic activity`,
              values_from = OBS_VALUE) %>%
  rename(ISO = REF_AREA,
         Year = TIME_PERIOD) %>%
  mutate(finance_share_VA = `Financial and insurance activities` / 
           `Total - all activities`,
         manufacturing_share_VA = `Manufacturing` / 
           `Total - all activities`,
         mining_share_VA = `Mining and quarrying` / 
           `Total - all activities`,
         agric_share_VA = `Agriculture, forestry and fishing` /
           `Total - all activities`) %>% 
  mutate(Country = countrycode(ISO, "iso3c", "country.name"))

# Add source column to each dataset
estat <- estat %>% mutate(Source = "estat")
adb <- adb %>% mutate(Source = "adb")
oecd <- oecd %>% mutate(Source = "oecd_naag")
oecd_tiva <- oecd_tiva %>% mutate(Source = "oecd_tiva")
afdb <- afdb %>% mutate(Source = "afdb")
cepal <- cepal %>% mutate(Source = "cepal")

# Further countries and data

# Indonesia --------------------------------------------------------------------

# Malysia ----------------------------------------------------------------------
# source: https://www.ekonomi.gov.my/en/socio-economic-statistics/socio-economic/national-accounts
# Table 2.1.4, 2.4.3.1, 2.4.4.2 
# GDP by Kind of Economic Activity at Current Prices (mill. Malaysian Ringgit)


# China ------------------------------------------------------------------------
# source: https://data.stats.gov.cn/english/index.htm -> annual data
# National Accounts -> Value-added by industries 

CHN_raw <- fread(here("data/raw/value added data/china_value_added.csv"))

CHN <- CHN_raw %>% 
  mutate(ISO = "CHN",
         GVA = rowSums(across(3:11)),
         agric_share_VA = `Value-added of Agriculture Forestry Animal Husbandry and Fishery industries(100 million yuan)` /
           GVA,
         finance_share_VA = `Value-added of Financial Intermediation(100 million yuan)` /
           GVA,
         Source = "China National Statistics",
         GVA_adb = GVA*100000000) # CHN data is in 100 million Yuan, ADB in unit Yuans
# Calculated data fits well OECD data (see plotlys below) - GVA can be used to
# complement ADB data, where Total GVA is missing!

adb_chn <- adb %>%
  filter(ISO == "CHN")

adb_chn_updated <- adb_chn %>%
  left_join(
    CHN %>% select(Year, GVA_adb),
    by = "Year"
  ) %>%
  mutate(GVA = GVA_adb) %>%
  select(-GVA_adb) %>% 
  mutate(finance_share_VA = Financial_and_insurance / GVA, #calculation now possible
         mining_share_VA = Mining_and_quarrying / GVA, #mining still missing though!!!
         manufacturing_share_VA = Manufacturing / GVA,
         agric_share_VA = Agriculture / GVA)

# Put the updated China rows back into adb
adb_updated <- adb %>%
  filter(ISO != "CHN") %>%
  bind_rows(adb_chn_updated)

# Singapore --------------------------------------------------------------------
# Source: https://tablebuilder.singstat.gov.sg/table/TS/M015781
# Share Of Nominal Gross Value Added, By Industry (SSIC 2020)

SGP_raw <- fread(here("data/raw/value added data/sgp_gva.csv"))

SGP_colnames <- unname(unlist(SGP_raw[10,]))
colnames(SGP_raw) <- SGP_colnames

SGP <- SGP_raw %>% 
  slice(11:75) %>% 
  mutate(across(2:27, as.double)) %>% 
  mutate(Year = as.integer(`Data Series`),
         ISO = "SGP") %>% 
  mutate(manufacturing_share_VA = 
           `Goods Producing Industries -> Manufacturing (Per Cent)` / 100,
         finance_share_VA = 
           `Services Producing Industries -> Finance & Insurance (Per Cent)` / 100,
         Source = "Singstat")

# Japan ------------------------------------------------------------------------
# source: https://www.esri.cao.go.jp/en/sna/valueadded/valueadded_top.html 

JPN_raw <- fread(here("data/raw/value added data/jpn_va.csv")) # only availabe in chained 2015 dollars :(

JPN <- JPN_raw %>% 
  rename(Agriculture = V4,
         Mining = V5,
         Manufacturing = V6,
         Finance = V27,
         Year = V1,
         Quarter = V3,
         GVA = V34) %>% 
  slice(11:133) %>% 
  mutate(across(4:41, as.double)) %>%
  mutate(GVA_sum = rowSums(across(c(4:6, 21:33)))) %>% 
  # slight difference between GVA and GVA_sum :(
  select(Year, GVA, Agriculture, Finance, Mining, Manufacturing, 
         GVA_sum) %>% 
  # Data is quarterly, averaging over quarters
  group_by(Year) %>%
  summarise(across(c(GVA, Agriculture, Finance, Mining, Manufacturing, GVA_sum),
                   mean, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(finance_share_VA = Finance / GVA,
         mining_share_VA = Mining / GVA,
         manufacturing_share_VA = Manufacturing / GVA,
         agric_share_VA = Agriculture / GVA,
         Source = "Japan National Statistics",
         ISO = "JPN") %>% 
  mutate(Year = as.integer(Year))

# Taiwan -----------------------------------------------------------------------
# source: https://nstatdb.dgbas.gov.tw/dgbasall/webMain.aspx?k=engmain
# in Gross Domestic Product_Current Price(Million N.T.$) 

TWN_raw <- fread(here("data/raw/value added data/taiwan_va.csv"))

TWN <- TWN_raw %>% 
  filter(V2 == "K. Financial and Insurance Activities" |
           V2 == "Total ( by production approach )" |
           V2 == "C. Manufacturing" |
           V2 == "B. Mining and Quarrying" |
           V2 == "A. Agriculture, Forestry, Fishing and Animal Husbandry") %>% 
  rename("Year" = V1) %>% 
  pivot_wider(names_from = V2,
              values_from = V3) %>% 
  mutate(Year = as.integer(Year)) %>% 
  mutate(across(2:6, as.double)) %>% 
  mutate(finance_share_VA = `K. Financial and Insurance Activities`/
           `Total ( by production approach )`,
         manufacturing_share_VA = `C. Manufacturing`/
           `Total ( by production approach )`,
         mining_share_VA = `B. Mining and Quarrying`/
           `Total ( by production approach )`,
         agric_share_VA = `A. Agriculture, Forestry, Fishing and Animal Husbandry`/
           `Total ( by production approach )`) %>% 
  mutate(ISO = "TWN") %>% 
  mutate(Country = "Taiwan") %>% 
  mutate(Source = "Taiwan National Statistics")

# Brunei -----------------------------------------------------------------------
# source: https://deps.mofe.gov.bn/SitePages/eData%20library.aspx
# Share in Gross Value Added by Kind of Economic Activity at Current Prices

BRN_raw <- read_excel(here("data/raw/value added data/brunei_va.xlsx"))

BRN <- BRN_raw %>% 
  mutate("Country" = "Brunei") %>% 
  mutate(finance_share_VA = finance_share_VA / 100,
         manufacturing_share_VA = manufacturing_share_VA / 100,
         mining_share_VA = mining_share_VA / 100,
         agric_share_VA = agric_share_VA / 100) %>% 
  mutate(Source = "Brunei National Statistics")

# Macau SAR --------------------------------------------------------------------
# source: https://www.dsec.gov.mo/ts/#!/step1/en-US
# Gross value added of principal economic activities at current producers' prices, by production approach
# Banking, Insurance and pension funding

MAC_raw <- read_excel(here("data/raw/value added data/mac_va.xlsx"))

colnamesMAC <- c("Year", "GVA", "mining_and_quarrying", "manufacturing",
                 "banking", "insurance_pension")
names(MAC_raw) <- colnamesMAC
MAC_raw <- MAC_raw[6:38,]

MAC <- MAC_raw %>% 
  mutate(Year = as.integer(Year)) %>% 
  mutate(across(2:6, as.double)) %>%
  mutate(finance_share_VA = (banking + insurance_pension) / GVA,
         manufacturing_share_VA = manufacturing / GVA,
         mining_share_VA = mining_and_quarrying / GVA) %>% 
  mutate("ISO" = "MAC") %>% 
  mutate("Country" = "Macau SAR") %>% 
  mutate("Source" = "Macau Statistics")

# United Arab Emirates --------------------------------------------------------- 
# source: https://fcsc.gov.ae/en-us/Pages/Statistics/Statistics-by-Subject.aspx#/%3Ffolder=Economy/National%20Account/Annual%20National%20Accounts

ARE <- read_excel(here("data/raw/value added data/ARE_va.xlsx"))

# Saudi Arabia -----------------------------------------------------------------
# source: https://www.stats.gov.sa/en/823 
# Gross Domestic Product By Kind of Economic Activity at Current Prices

SAU <- read_excel(here("data/raw/value added data/sau_va.xlsx"))

# Merge all datasets into one dataframe ----------------------------------------
value_added_shares <- bind_rows(
  estat %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, mining_share_VA, agric_share_VA, Source),
  adb_updated %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, mining_share_VA, agric_share_VA, Source),
  adb_wb %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, mining_share_VA, agric_share_VA, Source),
  oecd %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, agric_share_VA, Source),
  afdb %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, mining_share_VA, agric_share_VA, Source),
  cepal %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, mining_share_VA, agric_share_VA, Source),
  oecd_tiva %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, mining_share_VA, agric_share_VA, Source),
  TWN %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, mining_share_VA, agric_share_VA, Source),
  BRN %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, mining_share_VA, agric_share_VA, Source),
  MAC %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, mining_share_VA, Source),
  ARE %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, mining_share_VA, Source),
  SAU %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, mining_share_VA, Source),
  CHN %>% select(ISO, Year, finance_share_VA, agric_share_VA, Source),
  un_combined %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, mining_share_VA, agric_share_VA, Source),
  un2 %>% select(ISO, Year, agric_share_VA, manufacturing_share_VA, Source),
  SGP %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, Source),
  #JPN %>% select(ISO, Year, finance_share_VA, manufacturing_share_VA, mining_share_VA, agric_share_VA, Source)
)

# Find countries in macro_world but not in value_added_shares

#load(here("data/macro_data.RData"))

#missing_countries <- setdiff(macro_data$ISO, value_added_shares$ISO)
#print(missing_countries)

# Test countries

#ifelse("chn" %in% missing_countries, "missing", "not missing")

# Conflicts --------------------------------------------------------------------

# Identify countries and years with multiple observations
#conflicts <- value_added_shares %>%
 # group_by(ISO, Year) %>%
  #filter(n() > 1) %>%
  #arrange(ISO, Year)

# Pivot wider (only EA)

ISO_list_EA <- c("BRN", "MAC", "TWN", "CHN", "HKG", "IDN", "JPN", "KOR", "MYS",
                 "PHL", "SGP", "THA", "VNM", "MNG", "MMR", "KHM", "LAO", "TLS")

VA_EA_wide <- value_added_shares %>%
  filter(ISO %in% ISO_list_EA) %>%
  pivot_wider(names_from = Source,
              values_from = c(finance_share_VA, 
                              manufacturing_share_VA, 
                              mining_share_VA, 
                              agric_share_VA))

# Visualize all sources for finance_share_VA

finance_long <- VA_EA_wide %>% 
  pivot_longer(
    cols = starts_with("finance_share_VA"),
    names_to = "Source",
    values_to = "Finance_Share_VA"
  )

p_finance <- ggplot(finance_long, 
                    aes(x = Year, 
                        y = Finance_Share_VA, 
                        color = Source)) +
  geom_line() +
  facet_wrap(~ ISO, scales = "free_y") +
  labs(title = "Finance Share VA by Source",
       x = "Year", y = "Finance Share VA") +
  theme_minimal()

#ggplotly(p_finance) %>% 
#  layout(legend = list(title = list(text = "Source")))
# Comments: NAAG, UN, ADB overlap 
# more sources needed for MMR, MYS, VNM, IDN (more years!)
# KHM use only ADB data!!!
# 2000-2009 data for MYS questionable (see footnotes adb_raw)
# "For 2000–2009: Includes real estate activities; professional, scientific, and technical activities; and administrative and support service activities."
# cannot use ADB data for Malaysia 

# Visualize all sources for manufacturing_share_VA

manufacturing_long <- VA_EA_wide %>% 
  pivot_longer(
    cols = starts_with("manufacturing_share_VA"),
    names_to = "Source",
    values_to = "Manufacturing_Share_VA"
  )

p_manufacturing <- ggplot(manufacturing_long, 
                          aes(x = Year, 
                              y = Manufacturing_Share_VA, 
                              color = Source)) +
  geom_line() +
  facet_wrap(~ ISO, scales = "free_y") +
  labs(title = "Manufacturing Share VA by Source",
       x = "Year", y = "Manufacturing Share VA") +
  theme_minimal()

#ggplotly(p_manufacturing) %>%
#  layout(legend = list(title = list(text = "Source")))
# Comments: MMR missing, more years required for VNM, IDN, MYS
# for the rest: UN, NAAG, ADB, national statistics mostly overlap
# CHN: ADB data is higher because it also includes mining!!!

# Visualize all sources for mining_share_VA

mining_long <- VA_EA_wide %>% 
  pivot_longer(
    cols = starts_with("mining_share_VA"),
    names_to = "Source",
    values_to = "Mining_Share_VA"
  )

p_mining <- ggplot(mining_long, 
                   aes(x = Year, 
                       y = Mining_Share_VA, 
                       color = Source)) +
  geom_line() +
  facet_wrap(~ ISO, scales = "free_y") +
  labs(title = "Mining Share VA by Source",
       x = "Year", y = "Mining Share VA") +
  theme_minimal()

#ggplotly(p_mining) %>% 
#  layout(legend = list(title = list(text = "Source")))
# Comments: No NAAG data, CHN, SGP missing
# UN, ADB, national statistics mostly overlap

# Visualize all sources for agric_share_VA

agric_long <- VA_EA_wide %>% 
  pivot_longer(
    cols = starts_with("agric_share_VA"),
    names_to = "Source",
    values_to = "Agric_Share_VA"
  )

p_agric <- ggplot(agric_long, 
                  aes(x = Year, 
                      y = Agric_Share_VA,
                      color = Source)) +
  geom_line() +
  facet_wrap(~ ISO, scales = "free_y") +
  labs(title = "Agriculture Share VA by Source",
       x = "Year", y = "Agriculture Share VA") +
  theme_minimal()

#ggplotly(p_agric) %>%
#  layout(legend = list(title = list(text = "Source")))
# Comments: Needs more years for VNM, IDN, MYS ...

# Decide which source for which variable and which country to use --------------

rules_list <- list(
  "BRN" = list(
    "finance_share_VA" = "adb",
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "UN NA Estimates"
  ),
  "CHN" = list(
    "finance_share_VA" = "oecd_naag",
    "manufacturing_share_VA" = "oecd_tiva", 
    # Use oecd_tiva for manufacturing, it fits with UN estimates, but encompasses
    # more years, ADB data includes mining in manufacturing!
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "China National Statistics" 
  ),
  "HKG" = list(
    "finance_share_VA" = "adb",
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "UN NA Estimates" 
  ),
  "IDN" = list(
    "finance_share_VA" = "oecd_tiva",
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "UN NA Estimates" 
  ),
  "JPN" = list(
    "finance_share_VA" = "oecd_naag",
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "UN NA Estimates" 
  ),
  "KHM" = list(
    "finance_share_VA" = "adb",
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "UN NA Estimates" 
  ),
  "KOR" = list(
    "finance_share_VA" = "UN",
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "UN NA Estimates" 
  ),
  "LAO" = list(
    "finance_share_VA" = "adb",
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "UN NA Estimates" 
  ),
  "MMR" = list(
    "finance_share_VA" = "oecd_tiva",
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "UN NA Estimates" 
  ),
  "MNG" = list(
    "finance_share_VA" = "UN",
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "UN",
    "agric_share_VA" = "UN NA Estimates" 
  ),
  "MYS" = list(
    "finance_share_VA" = "oecd_tiva", # ADB data includes Includes real estate activities etc. 
    # ...and is therefore higher
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "UN NA Estimates" 
  ),
  "PHL" = list(
    "finance_share_VA" = "UN",
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "UN NA Estimates" 
  ),
  "SGP" = list(
    "finance_share_VA" = "UN",
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "UN NA Estimates" 
  ),
  "THA" = list(
    "finance_share_VA" = "UN",
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "UN NA Estimates" 
  ),
  "TWN" = list(
    "finance_share_VA" = "adb",
    "manufacturing_share_VA" = "Taiwan National Statistics",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "Taiwan National Statistics" 
  ),
  "VNM" = list(
    "finance_share_VA" = "oecd_tiva",
    "manufacturing_share_VA" = "UN NA Estimates",
    "mining_share_VA" = "oecd_tiva",
    "agric_share_VA" = "UN NA Estimates" 
  )
)

value_added_shares_EA_sources <- value_added_shares %>%
  filter(ISO %in% ISO_list_EA)

# Create final data by processing each country from the rules list
value_added_shares_EA <- lapply(names(rules_list), function(country) {
  
  # get the rules for the current country: variable -> preferred source
  country_rules <- rules_list[[country]]
  
  # start with the set of years available for this country
  df_country <- value_added_shares_EA_sources %>%
    filter(ISO == country) %>%
    distinct(Year) %>%
    mutate(ISO = country)
  
  # For each variable, filter rows to the ones with the correct Source and join by Year.
  for(var in names(country_rules)) {
    preferred_source <- country_rules[[var]]
    
    # Filter the rows for this variable & preferred source, select only Year and the variable column.
    df_var <- value_added_shares_EA_sources %>%
      filter(ISO == country, Source == preferred_source) %>%
      select(Year, !!sym(var))
    
    # Left join so that each country–year gets the chosen value.
    df_country <- left_join(df_country, df_var, by = "Year")
  }
  
  return(df_country)
})

# Combine results for all countries
value_added_shares_EA <- bind_rows(value_added_shares_EA)

# Ignore below
# Comments on Conflicts: 
# Chile - big differences in finance_share between OECD and CEPAL - choose OECD
# Replace CEPAL data for Chile for column finance_share_VA with NA

#value_added_shares_alpha <- value_added_shares %>% 
#  mutate(finance_share_VA = ifelse(ISO == "CHL" & Source == "cepal", NA, finance_share_VA))

# Now: construct averages for the conflicts

#value_added_shares <- value_added_shares_alpha %>%
#  group_by(ISO, Year) %>%
#  summarise(across(
#    c(finance_share_VA, manufacturing_share_VA, mining_share_VA, agric_share_VA), 
#    ~ mean(.x, na.rm = TRUE)
#  )) %>%
#  ungroup() %>% 
#  mutate(across(
#    c(finance_share_VA, manufacturing_share_VA, mining_share_VA, agric_share_VA),
#    ~ ifelse(is.nan(.x), NA, .x)
#  ))

# Save data --------------------------------------------------------------------

save(value_added_shares_EA, file = here("data/raw/value_added_shares.RData"))
