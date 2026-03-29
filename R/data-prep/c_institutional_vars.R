rm(list = ls())
library(here)
source(here("packages.R"))

ISO_list_EA <- c("BRN", "MAC", "TWN", "CHN", "HKG", "IDN", "JPN", "KOR", "MYS",
                 "PHL", "SGP", "THA", "VNM", "MNG", "MMR", "KHM", "LAO", "TLS")

# State capacity variables (inspired by Besley and Persson 2011) ---------------
## WGI: Political stability and absence of violence, government effectiveness, rule of law, control of corruption
## WDI: Tax revenue (% of GDP))
## VDEM: Liberal democracy index (already included)

wgi <- read_dta(here("data/raw/wgidataset_with_sourcedata-2025.dta"))

wgi_ea <- wgi %>% 