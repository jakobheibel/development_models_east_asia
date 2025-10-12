library(here)
source(here("packages.R"))

# Load the data ----------------------------------------------------------------

load(here("data/macro_world.RData"))

EA <- c(
  "China", 
  "Hong Kong", 
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
  "Taiwan")

var_filter <- c(
  "Unemp",
  "CapAccGross",
  "XinPercGDP",
  "GDPpcPPPDivFromMean", 
  "CAinPercGDP",
  "DebtPercGDP", 
  "FinanceShareVA",
  "ManufacturingShareVA",
  "AgricultureShareVA",
  "MiningShareVA",
  "GiniMkt",
  "FDInetinflow",
  "ECI",
  "GDPgrowth",
  "KOFEcGI"
)

macro_data <- macro_world %>% 
  filter(Country %in% EA,
         Year >= 2000,
         Year <= 2019) %>%
  select(Country, Year, all_of(var_filter))

# Code based on Gräbner et al. 2022 
# (https://github.com/graebnerc/structural-change)

# Regressions (Impulse response functions)  ------------------------------------

# Specifications

var_list <- c(
  "Unemp",
  "XinPercGDP",
  #"GDPgrowth",
  "GDPpcPPPDivFromMean", 
  "CAinPercGDP",
  "DebtPercGDP", 
  "FinanceShareVA",
  "ManufacturingShareVA",
  "AgricultureShareVA",
  "MiningShareVA",
  "GiniMkt",
  "FDInetinflow",
  "ECI"
)

shock_var <- "KOFEcGI"

reg_equation <- list()

reg_equation[["Unemp"]] <- paste0(
  "~", shock_var, "+GDPgrowth+CapAccGross+Unemp_DLAG+GDPgrowth_DLAG+CapAccGross_DLAG+Unemp_LAG")

#reg_equation[["GDPgrowth"]] <- paste0(
 # "~", shock_var, "+GDPgrowth_DLAG")

reg_equation[["CAinPercGDP"]] <- paste0(
  "~", shock_var, "+Unemp+CAinPercGDP_DLAG+Unemp_DLAG+CAinPercGDP_LAG")

reg_equation[["XinPercGDP"]] <- paste0(
  "~", shock_var, "+Unemp+XinPercGDP_DLAG+Unemp_DLAG+XinPercGDP_LAG")

reg_equation[["DebtPercGDP"]] <- paste0(
  "~", shock_var, "+CapAccGross+DebtPercGDP_DLAG+CapAccGross_DLAG+DebtPercGDP_LAG")

reg_equation[["GDPpcPPPDivFromMean"]] <- paste0(
  "~", shock_var, "+Unemp+GDPgrowth_DLAG+Unemp_DLAG")

reg_equation[["FinanceShareVA"]] <- paste0(
  "~", shock_var, "+GDPgrowth_DLAG+CapAccGross+FinanceShareVA_DLAG+CapAccGross_DLAG")

reg_equation[["ManufacturingShareVA"]] <- paste0(
  "~", shock_var, "+GDPgrowth_DLAG")

reg_equation[["AgricultureShareVA"]] <- paste0(
  "~", shock_var, "+GDPgrowth_DLAG")

reg_equation[["MiningShareVA"]] <- paste0(
  "~", shock_var, "+GDPgrowth_DLAG")

reg_equation[["GiniMkt"]] <- paste0(
  "~", shock_var, "+GDPgrowth_DLAG")

reg_equation[["FDInetinflow"]] <- paste0(
  "~", shock_var, "+GDPgrowth_DLAG")

reg_equation[["ECI"]] <- paste0(
  "~", shock_var, "+GDPgrowth_DLAG")

# Make aggreagted projections

impulse_responses_aggregated_fig_width <- 8
impulse_responses_aggregated_fig_height <- 10
impulse_responses_disaggregated_fig_width <- 9
impulse_responses_disaggregated_fig_height <- 11

list_of_fe_estimates <- list()
list_of_projection_plots <- list()
list_of_projection_objects <- list()
list_of_shock_data <- list()

for (v in var_list) {
  print(v)
  y_label <- "change in perc. p."
  regression_equation <- paste0(v, reg_equation[[v]])
  current_projections <- get_projections(data_obj = macro_data,
                                         regression_formula = regression_equation,
                                         projection_horizon = 8,
                                         return_intermediate_data = T)
  # requires package liloprojections from:
  # https://github.com/graebnerc/liloprojections
  print("...finished")
  list_of_fe_estimates[[v]] <- current_projections$fe_estimates
  list_of_projection_plots[[v]] <- current_projections$impulse_plot
  list_of_projection_objects[[v]] <- current_projections$projections
  list_of_shock_data[[v]] <- current_projections$estimation_data
}

fe_estimates <- rbindlist(list_of_fe_estimates)

write_feather(x = fe_estimates, 
              path = here("output/impulse_response/fe_estimates.feather"))

# Clustering functions ---------------------------------------------------------

make_dendo <- function(init_data,
                       # first_k=FALSE,
                       # k_cons = "all", # either 'all', 'first' or 'last' 
                       k_considered,
                       kof_case = FALSE,
                       n_groups = 4,
                       restrict_variables=F) {
  # make_dendo: takes total data of fixed effect estimates and clusters countries;
  # returns both output name and dendo plot in a list
  # stopifnot(k_cons %in% c("all", "first", "last"))
  
  #init_data <- dplyr::mutate(init_data, 
  #                          country = countrycode(country, "iso3c", "country.name"))
  
  # Select the adequate variables ----------------------------------------------
  if (!TRUE %in% (restrict_variables==FALSE)){
    init_data <- init_data %>%
      dplyr::filter(var %in% restrict_variables)
  }
  
  # Select the adequate projection horizon to consider -------------------------
  if (is.character(k_considered)){
    if (!k_considered %in% c("first", "last", "all")){
      stop("Admissible keywords for k values are: 'first', 'last', and 'all'. 
           Otherwise, only integers allowed to specify which k to consider.")
    } else{
      if (k_considered=="first"){
        k_used <- "k_1"
      } else if (k_considered=="last"){
        k_used <- paste0("k_", length(unique(init_data$k_val)))
      } else {
        stopifnot(k_considered=="all")
        k_used <- paste0("k_", 1:length(unique(init_data$k_val)))
      } 
    }
  } else if (is.integer(k_considered)){
    k_used <- paste0("k_", k_considered)
  } else if (is.numeric(k_considered)){
    warning("The k horizon was given as double, not integer; 
            transformed into integer to proceed!")
    k_used <- paste0("k_", as.integer(k_considered))
  } else {
    stop("The k horizon to be considered must be provided as vector of 
         integers or a keyword mentioned in function help!")
  }
  
  work_data_v1 <- init_data %>%
    dplyr::filter(k_val %in% k_used) %>%
    tidyr::unite("var_k", c("var", "k_val"), sep = "_") %>%
    tidyr::spread(var_k, fe_val)#  %>%     dplyr::select(-k_val)
  work_data_v1 <- as.data.frame(work_data_v1)
  rownames(work_data_v1) <- work_data_v1$country
  
  # Prepare output nomenclature depending on EMU or KOF case -------------------
  
  case_var <- "kof"
  
  output_name <- paste0(case_var, "_k", min(k_considered), "-", 
                        max(k_considered), "_cluster.pdf"
  )
  title_addendum <- paste0("(", case_var, " with k: ", min(k_considered), "-", 
                           max(k_considered),")"
  )
  dendo_title <- paste0("Hierachical clustering of FE estimates ", 
                        title_addendum
  )
  factor_title <- paste0("Factor map for FE estimates ", 
                         title_addendum
  )
  
  if (!TRUE %in% (restrict_variables==FALSE)){
    output_name <- paste0("only_", paste0(restrict_variables, collapse = "-"), 
                          "_" , output_name)
    dendo_title <- paste0(restrict_variables, collapse = "+")
  } 
  
  
  # Conduct the clustering and create the plot object --------------------------
  
  get_diff_matrix <- function(init_data, raw_dat = FALSE, m = "euclidean") {
    # Takes data, omits missing values, scales data and returns distance matrix
    work_data_1 <- na.omit(init_data)
    work_data_2 <- scale(work_data_1)
    if (raw_dat == TRUE) {
      return(work_data_2)
    } else {
      diss_ma <- dist(work_data_2, method = m)
      return(diss_ma)
    }
  }
  
  work_data_v1 <- select(work_data_v1, -country)
  clustering_object <- work_data_v1 %>%
    get_diff_matrix(raw_dat = FALSE) %>% # Scale the data
    agnes(method = "ward") # Compute hierachical clustering
  
  dendo_plot <- fviz_dend(clustering_object,
                          main = dendo_title,
                          xlab = "Countries", ylab = "Height",
                          k = n_groups, # Cut in groups
                          cex = 0.75, # label size
                          rect = TRUE, # Add rectangle around groups
                          rect_fill = TRUE,
                          palette = "lancet",
                          horiz = TRUE
  )
  sub_grp <- cutree(as.hclust(clustering_object), k = n_groups)
  
  # Create the factor map ------------------------------------------------------
  
  cluster_colors <- c(
    "1" = "#00468B", 
    "2" = "#ED0000", 
    "4" = "#42B540", 
    "5" = "#0099B4", 
    "3" = "#925E9F"
  )
  
  factor_map <- fviz_cluster(list(
    data = get_diff_matrix(work_data_v1, raw_dat = T),
    cluster = sub_grp
  ),
  repel = TRUE, # Avoid label overlapping
  show.clust.cent = TRUE, # Show cluster centers
  palette = cluster_colors, # Color palette see ?ggpubr::ggpar
  ggtheme = theme_minimal(),
  main = factor_title
  ) +
    labs(color = "Clusters") +
    guides(color = "none") +
    theme(legend.title = element_blank()
    )
  
  # Create the final return object ---------------------------------------------
  list_to_return <- list(
    plot_obj = dendo_plot,
    plot_name = output_name,
    factor_plot = factor_map,
    clust_obj = clustering_object
  ) # ,two_d=dendo_2d_plot
  return(list_to_return)
}

get_diff_matrix <- function(init_data, raw_dat = FALSE, m = "euclidean") {
  # Takes data, omits missing values, scales data and returns distance matrix
  work_data_1 <- na.omit(init_data)
  work_data_2 <- scale(work_data_1)
  if (raw_dat == TRUE) {
    return(work_data_2)
  } else {
    diss_ma <- dist(work_data_2, method = m)
    return(diss_ma)
  }
}

compare_clustering_types <- function(raw_dat,
                                     k_num = "all") {
  # Compares the different aggregative/divisive coefficients
  # of the standard clustering algorithms
  if (k_num == "all") { # all k are considered
    int_dat <- raw_dat %>%
      unite("var_k", c("var", "k_val"), sep = "_") %>%
      spread(var_k, fe_val)
  } else if (k_num == "first") { # only first k is considered
    int_dat <- fe_total %>%
      filter(k_val == "k_1") %>%
      spread(var, fe_val) %>%
      select(-k_val)
  } else if (k_num == "last") { # only last k is considered
    int_dat <- fe_total %>%
      filter(k_val == "k_8") %>%
      spread(var, fe_val) %>%
      select(-k_val)
  } else {
    stop("Wrong value for 'k_num' given. Allowed: 'first', 'last', or 'all'!")
  }
  rownames(int_dat) <- int_dat$country
  int_dat <- select(int_dat, -country)
  int_dat <- get_diff_matrix(int_dat, raw_dat = TRUE)
  diss_matrix <- get_diff_matrix(int_dat, raw_dat = FALSE)
  
  hc_agnes_complete_linkage <- agnes(diss_matrix, method = "complete") # Hierarchical clustering using Complete Linkage
  hc_agnes_average_linkage <- agnes(diss_matrix, method = "average") # Hierarchical clustering using Average Linkage
  hc_agnes_single_linkage <- agnes(diss_matrix, method = "single") # Hierarchical clustering using single Linkage
  hc_agnes_ward <- agnes(diss_matrix, method = "ward") # Hierarchical clustering using Ward's method
  divisive_cluster <- diana(int_dat) # divisive hierarchical clustering
  cluster_type <- c("agnes_complete", "agnes_average", "agnes_single", "agnes_ward", "diana_divisive")
  fit_coefs <- c(hc_agnes_complete_linkage$ac, hc_agnes_average_linkage$ac, hc_agnes_single_linkage$ac, hc_agnes_ward$ac, divisive_cluster$dc)
  info_frame <- data.frame(type_clustering = cluster_type, dif_coef = fit_coefs) %>%
    arrange(desc(dif_coef)) %>%
    rename(Algorithm=type_clustering,
           `Clust. coef.`=dif_coef)
  return(info_frame)
}

# Make clustering --------------------------------------------------------------

fe_k_plot_height <- 8
fe_k_plot_width <- 12

data_new <- read_feather(here("output/impulse_response/fe_estimates.feather"))  

# Data preparation

# dictionary with variable names

var_name_dict <- list()
var_name_dict[["Unemp"]] <- "unemp"
#var_name_dict[["GDPgrowth"]] <- "gdp_growth"
var_name_dict[["CAinPercGDP"]] <- "current_accout"
var_name_dict[["DebtPercGDP"]] <- "debt_public"
var_name_dict[["GiniMkt"]] <- "gini_market"
var_name_dict[["XinPercGDP"]] <- "exp_to_gdp"
var_name_dict[["GDPpcPPPDivFromMean"]] <- "gdp_pc_div"
var_name_dict[["FinanceShareVA"]] <- "fin_va"
var_name_dict[["ManufacturingShareVA"]] <- "manu_va"
var_name_dict[["AgricultureShareVA"]] <- "agri_va"
var_name_dict[["MiningShareVA"]] <- "mining_va"
var_name_dict[["FDInetinflow"]] <- "fdi_inflow_to_gdp"
var_name_dict[["ECI"]] <- "eci"
var_name_dict <- data.frame(feather_name = names(unlist(var_name_dict)), 
                            cluster_name = unlist(var_name_dict), row.names = NULL)
var_name_dict

fe_total <- data_new %>%
  rename(country=csu) %>%
  mutate(var = countrycode::countrycode(var, "feather_name", "cluster_name", 
                                        custom_dict = var_name_dict)) %>%
  gather(k_val, fe_val, -country, -var) %>%
  mutate(k_val = gsub("\\.", "_", k_val))

length(unique(fe_total$var))
fe_total <- as.data.table(fe_total)

# plot dynamics of FE estimates

fe_total_plot <- fe_total %>%
  filter(var %in% c("unemp", #"gdp_growth", 
                    "current_accout", 
                    "exp_to_gdp", "debt_public", "gdp_pc_div", 
                    "fin_va", "manu_va", "agri_va", "mining_va", "gini_market",
                    "fdi_inflow_to_gdp", "eci"))

fe_total_plot <- as.data.table(fe_total_plot)
fe_total_plot[var == "unemp", var := "Unemployment"]
fe_total_plot[var == "current_accout", var := "Current account balance"]
fe_total_plot[var == "exp_to_gdp", var := "Export to GDP"]
fe_total_plot[var == "debt_public", var := "Public debt"]
fe_total_plot[var == "gdp_pc_div", var := "GDP per capita (div from mean)"]
fe_total_plot[var == "fin_va", var := "Share of finance (VA)"]
fe_total_plot[var == "manu_va", var := "Share of manufacturing (VA)"]
fe_total_plot[var == "agri_va", var := "Share of agriculture (VA)"]
fe_total_plot[var == "mining_va", var := "Share of mining (VA)"]
fe_total_plot[var == "gini_market", var := "Gini coefficient on market income"]
fe_total_plot[var == "fdi_inflow_to_gdp", var := "FDI inflows to GDP"]
fe_total_plot[var == "eci", var := "Economic complexity index"]
fe_total_plot[, k_val := substr(k_val, 3, 3)]
fe_total_plot[, var_val := var(fe_val, na.rm = T), by = .(var, k_val)]

head(fe_total_plot)

fe_k_plot <- ggplot(data = fe_total_plot) +
  geom_line(aes(x = k_val, y = fe_val, color = country, group = country),
            color = "grey", alpha = 0.25
  ) +
  geom_line(
    data = fe_total_plot[country == "AUT"],
    aes(x = k_val, y = var_val, group = country),
    color = "red", alpha = 0.95
  ) +
  xlab("k") +
  scale_x_discrete(expand = c(0, 0)) +
  ylab("FE estimate") +
  facet_wrap(~var, scales = "free") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    strip.background = element_rect(fill = "white", linetype = "blank"),
    strip.text = element_text(size=14),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title = element_text(size=12)
  )
fe_k_plot

file_name <- "output/impulse_response/fe-estimates_k_dependence.pdf"

ggsave(fe_k_plot,
       filename = here(file_name),
       height = fe_k_plot_height,
       width = fe_k_plot_width
)

# Clustering of FE estimates

variable_selection_clustering <- c(
  "unemp", "current_account", "debt_public", "gini_market", "exp_to_gdp", 
  "gdp_pc_div", "fin_va", "manu_va", "agri_va", "mining_va", "fdi_inflow_to_gdp", 
  "eci")

nb_groups <- 5

impulse_reponse_results <- make_dendo(fe_total,
                         k_considered = "all",
                         #k_considered = 2:6,
                         kof_case = FALSE,
                         n_groups = nb_groups,
                         restrict_variables = variable_selection_clustering
)

plot_file_name <- "output/impulse_response/impulse_response_results.pdf"

w <- 9 * 1.618
h <- 7
pdf(here(plot_file_name), width = w, height = h) # golden ratio would be 1.618
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 2, 
                                           widths = c(2*(w/3), w/3), 
                                           default.units = "in")))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(impulse_reponse_results$plot_obj + 
        ggtitle("Agglomerative Hierarchical Clustering of FE Estimates (Impulse Response Functions)"), 
      vp = vplayout(1, 1))
print(impulse_reponse_results$factor_plot + ggtitle("Factor Map"), 
      vp = vplayout(1, 2))
dev.off()
