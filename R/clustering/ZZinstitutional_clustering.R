rm(list = ls())
library(here)
source(here("packages.R"))
source(here("R/clustering/utils/clusters_functions.R"))

# Institutional extension of the period clustering from Figures_A1-A4 ----------
# repeats the East Asia clustering with macro + WGI variables

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
	"ECI",
	# World Governance Indicators (WGI)
	"cc", #"Control of Corruption",
	"ge", #"Government Effectiveness",
	"pv", #"Political Stability and Absence of Violence/Terrorism",
	"rq", #"Regulatory Quality",
	"rl", #"Rule of Law",
	"va"  #"Voice and Accountability"
	#"LibDem" # Changes Indonesia, Vietnam and China when included
)

# Time periods to consider

time_periods <- c(
	2000, 2002,
	2002, 2004,
	2004, 2006,
	2006, 2008,
	2008, 2010,
	2010, 2012,
	2012, 2014,
	2014, 2017,
	2017, 2019,
	2000, 2019
)

# Number of country groupings

k <- 4

# Color palette for dendograms and factor map

cluster_colors <- c(
	"1" = "#925E9F",
	"2" = "#ED0000",
	"4" = "#42B540",
	"3" = "#0099B4"
)

# Load and prepare macro + institutional data ----------------------------------

load(here("data/macro_inst_world.RData"))

macro_inst_world <- macro_inst_world %>%
	select(-ISO) %>%
	pivot_longer(cols = -c(Country, Year),
							 names_to = "variable_name", values_to = "value")

macro_EA <- macro_inst_world %>%
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

	assign(paste0("results_inst_", start_year, "_", end_year), results)

	save(results,
			 file = here(paste0("output/cluster_results/FE_Clust_EA_inst_",
													start_year, "_", end_year, ".RData")))

	# Create a list of dendograms

	dendo <- get(
		paste0(
			"results_inst_", time_periods[i], "_", time_periods[i + 1]
		)
	)$dendo

	assign(paste0("dendo_inst_", start_year, "_", end_year), dendo)

	ggsave(here(paste0("output/dendograms/FE_Clust_EA_inst_",
										 start_year, "_",
										 end_year, ".png")),
				 dendo,
				 width = 8, height = 6, dpi = 300)

	dendo_all[[paste0("dendo_inst_", start_year, "_", end_year)]] <- dendo

	# Create a list of country groupings for the Sankey diagram

	groupings <- get(
		paste0(
			"results_inst_", start_year, "_", end_year
		)
	)$groupings

	group_name <- paste0(start_year, "-", end_year)

	country_groupings[[group_name]] <- groupings
}

# Plot all dendograms

all_dendos <- gridExtra::grid.arrange(grobs = dendo_all, ncol = 2)

# Create Sankey diagram --------------------------------------------------------

sankey_institutional <- create_sankey(country_groupings, time_periods, cluster_colors)

ggsave(here(paste0("output/FE_Sankey_EA_inst_",
									 time_periods[1], "_",
									 time_periods[length(time_periods)], ".pdf")),
			 sankey_institutional,
			 width = 11, height = 6, dpi = 300, bg = "white")

ggsave(here(paste0("output/FE_Sankey_EA_inst_",
									 time_periods[1], "_",
									 time_periods[length(time_periods)], ".svg")),
			 sankey_institutional,
			 width = 11, height = 6, dpi = 300, bg = "white")