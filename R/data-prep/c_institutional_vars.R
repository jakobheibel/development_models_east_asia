rm(list = ls())
library(here)
source(here("packages.R"))

# Load main data (macro vars)

load(here("data/macro_world.RData"))

ISO_list_EA_final <- c(
    "TWN", "CHN", "HKG", "IDN", "JPN", "KOR", "MYS", "PHL", "SGP", "THA", "VNM", 
    "MNG", "MMR", "KHM", "LAO")

# State capacity variables (inspired by Besley and Persson 2011) ---------------
## WGI: Political stability and absence of violence, government effectiveness, 
# rule of law, control of corruption
## WDI: Tax revenue (% of GDP), as a prxy for fiscal capacity 
## VDEM: Liberal democracy index (already included in macro_world)

wgi_raw <- read_dta(here("data/raw/wgidataset_with_sourcedata-2025.dta"))

wgi <- wgi_raw %>%
    # filter(econ_code %in% ISO_list_EA_final) %>%
    select(econ_code, wgi_year, dimension, score) %>%
    pivot_wider(names_from = dimension, values_from = score) %>%
    rename(
        Year = wgi_year,
        ISO = econ_code
    )

# Combine institutional vars with macro vars from macro_world data

macro_inst_world <- macro_world %>%
    left_join(wgi, by = c("ISO", "Year"))

# Save data

save(macro_inst_world, file = here("data/macro_inst_world.RData"))