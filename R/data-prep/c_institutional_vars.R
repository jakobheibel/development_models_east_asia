rm(list = ls())
library(here)
source(here("packages.R"))

# Load main data (macro vars)

load(here("data/macro_world.RData"))

ISO_list_EA_final <- c(
    "TWN", "CHN", "HKG", "IDN", "JPN", "KOR", "MYS", "PHL", "SGP", "THA", "VNM", 
    "MNG", "MMR", "KHM", "LAO")

# State capacity/social capability variables -----------------------------------

## WGI: Political stability and absence of violence, government effectiveness, 
# rule of law, control of corruption inspired by Besley and Persson 2011)

wgi_raw <- read_dta(here("data/raw/wgidataset_with_sourcedata-2025.dta"))

wgi <- wgi_raw %>%
    # filter(econ_code %in% ISO_list_EA_final) %>%
    select(econ_code, wgi_year, dimension, score) %>%
    pivot_wider(names_from = dimension, values_from = score) %>%
    rename(
        Year = wgi_year,
        ISO = econ_code)

## VDEM: Liberal democracy index (already included in macro_world)

## Human capital index 

pennRaw <- read_excel(here("data/raw/pwt1001.xlsx"), sheet = "Data")

penn_hc <- pennRaw %>% 
  rename(Year = year,
         ISO = countrycode,
         human_capital_index = hc) %>%
  select(ISO, Year, human_capital_index)

# Technological capability variables (inspired by Fagerberg/Srholec) -----------

## ECI: Economic Complexity Index (already included in macro_world)

## SJR: Number of scientific articles

sjr_data_raw <- sjr_countries # (install via pak::pak("ikashnitsky/sjrdata")

sjr_data <- sjr_data_raw %>%
  filter(year >= 2000, year <= 2019) %>%
  mutate(ISO = countrycode(country, "country.name", "iso3c")) %>%
  rename(Year = year) %>% 
  select(ISO, Year, documents)

penn_pop <- pennRaw %>%  #pop data
  rename(Year = year,
         ISO = countrycode) %>%
  select(ISO, Year, pop) # pop = Population in millions

sjr_data_pc <- sjr_data %>%
  left_join(penn_pop, by = c("ISO", "Year")) %>%
  mutate(sjr_per_million = documents / pop) %>%
  select(ISO, Year, sjr_per_million) # academic journals per millions

## Patent applications (per million)

# Our World in Data (from World Bank)

patent_data_raw <- fread(here(
  "data/raw/patent-applications-per-million/patent-applications-per-million.csv"))

# Data for Taiwan from Taiwan Intellectual Property Office (TIPO)
# https://www.tipo.gov.tw/en/

patent_data_twn <- fread(here(
  "data/raw/patent-applications-per-million/twn/data_taiwan.csv"))

patent_per_million_twn <- patent_data_twn %>%
  left_join(penn_pop, by = c("ISO", "Year")) %>% 
  mutate(patent_applications_per_million = patent_applications/pop) %>% 
  select(ISO, Year, patent_applications_per_million)

patent_data <- patent_data_raw %>%
  filter(Year >= 2000, Year <= 2019) %>%
  rename(ISO = Code,
         patent_applications_per_million = 
           `Patent applications per million people`) %>%
  select(ISO, Year, patent_applications_per_million) %>% 
  bind_rows(patent_per_million_twn) # Data for Myanmar is missing

## R&D Expenditure as a share of GDP

# Our World in Data (from World Bank)

research_spending_raw <- fread(here(
  "data/raw/research-spending-gdp/research-spending-gdp.csv"))

# Data for Taiwan from Ministry of Science and Technology Statistics Database

research_spending_twn <- fread(here(
  "data/raw/research-spending-gdp/twn/research_spending_gdp_twn.csv"))

research_spending <- research_spending_raw %>%
  filter(Year >= 2000, Year <= 2019) %>%
  rename(ISO = Code,
         research_spending_gdp =
           `Research and development expenditure (% of GDP)`) %>%
  select(ISO, Year, research_spending_gdp) %>% 
  bind_rows(research_spending_twn)

# Combine institutional vars with macro vars from macro_world data -------------

institutional_datasets <- list(
  penn_hc,
  wgi,
  sjr_data_pc,
  patent_data,
  research_spending)

macro_inst_world <- Reduce(
  function(x, y) left_join(x, y, by = c("ISO", "Year")),
  institutional_datasets,
  init = macro_world)

# Save data

save(macro_inst_world, file = here("data/macro_inst_world.RData"))