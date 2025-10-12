rm(list = ls())
library(here)
source(here("packages.R"))

load(here("data/macro_world.RData"))
load(here("data/plots_all_Variables.RData")) # plotly plots to view the variables
plot_list[["FinanceShareVA"]] # access specific plotly plot

# Proposed country groupings ---------------------------------------------------

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

cluster_colors <- c( 
  "1" = "#925E9F", # periphery
  "2" = "#ED0000", # emerging
  "4" = "#42B540", # developmental states
  "3" = "#0099B4" # financial hubs
)
  

# Which variables seem especially important for which country group???

variables_data <- c(
  "Unemp",
  "XinPercGDP",
  "GDPgrowth",
  "GDPpcPPPDivFromMean", 
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
  "ECI",
  "LibDem" 
)

# Create summary data for every variable ---------------------------------------

pennRaw <- read_excel(here("data/raw/pwt1001.xlsx"),
                      sheet = "Data")

#population data

#penn_pop <- pennRaw %>% 
#  rename(`Population, in millions (2019)` = pop,
#         Country = country,
#         Year = year,
#         ISO = countrycode) %>% 
#  select(Country, ISO, Year, `Population, in millions (2019)`) %>% 
#  filter(Year == 2019,
 #        ISO %in% ISO_list_EA3) %>% 
#  mutate(Country = recode(Country,
#                          "China, Hong Kong SAR" = "Hong Kong",
#                          "Republic of Korea" = "South Korea",
#                          "Lao People's DR" = "Laos",
#                          "Viet Nam" = "Vietnam")) %>% 
#  select(-c(ISO, Year))

missing_iso <- c()
generate_summary_data <- function(varname, 
                                  data = macro_world, # macro_world
                                  group_colors = group_colors,
                                  missing_iso = c() # vector with ISOs of countries...
                                  # ...with missing data (to be dashed!!)
                                  ) {
  
  summary_data <- data %>% 
    filter(ISO %in% east_asia) %>% 
    select(Country, ISO, Year, {{ varname }}) %>% 
    filter(Year > 1998 & Year < 2021) %>%
    mutate(Cluster = case_when(
      ISO %in% periphery ~ "Periphery",
      ISO %in% financial_hubs ~ "Finance",
      ISO %in% developmental ~ "Developmental",
      ISO %in% emerging ~ "Emerging",
      TRUE ~ "Other"))
  
  if (length(missing_iso) > 0) {
    summary_data <- summary_data %>% 
      mutate(missing_data = ifelse(ISO %in% missing_iso, "Missing Years",
                                   "Complete Data"))
  }
  
  summary_mean_data <- summary_data %>% 
    group_by(Cluster, Year) %>% 
    summarise(
      mean = mean({{ varname }}, na.rm = TRUE),
      min = min({{ varname }}, na.rm = TRUE),
      max = max({{ varname }}, na.rm = TRUE),
      sd = sd({{ varname }}, na.rm = TRUE),
      n = sum(!is.na({{ varname }})),
      .groups = "drop"
    ) %>%
    mutate(
      se = sd / sqrt(n),
      ci_lower = mean - 1.96 * se,
      ci_upper = mean + 1.96 * se
    )
  
  return(list(summary_data = summary_data,
              summary_mean_data = summary_mean_data))
  
}

#for (variable in variables_data) {
#  #print(paste("Processing variable:", variable))
#  result <- generate_summary_data(varname = !!sym(variable))
#  assign(paste0(variable, "_data"), result)
#}

# Create GDP p.c. development for Japan, South Korea, and Taiwan ---------------

gdp_data_JST <- pennRaw %>% 
  mutate(GDPpcPPPpenn = rgdpe/pop) %>% 
  select(countrycode, country, year, GDPpcPPPpenn) %>% 
  rename(Country = country,
         Year = year,
         ISO = countrycode) %>% 
  filter(ISO %in% c("JPN", "KOR", "TWN")) %>% 
  filter(Year <= 2000)

gdp_plot_JST <- ggplot(gdp_data_JST, 
                   aes(x = Year, 
                       y = GDPpcPPPpenn, 
                       group = Country,
                       linetype = Country,
                       #shape = Country
                   )) +
  geom_line(size = 0.5) +
  #guides(linetype = "none") +
  #geom_point(size = 1.5) +
  theme_minimal() +
  theme(
    #panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.position = "bottom",
    axis.line = element_blank(),  # turn off default lines
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),
    axis.line.y.left = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),  # add ticks
    axis.ticks.length = unit(0.1, "cm"),  # size of ticks
    axis.text = element_text(color = "black", size = 12),
    panel.border = element_blank(),  # remove full border
    panel.grid.minor = element_blank()
  ) +
  labs(title = "GDP per capita, PPP",
       x = "Year",
       y = "constant 2017 international $") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.015)),
                     breaks = c(1950, 1960, 1970, 1980, 1990, 2000
                                #2000, 2010, 2015
                                )
                     ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)))

gdp_plot_JST

ggsave(here("output/gdp_plot_JST.png"),
       gdp_plot_JST,
       width = 11, height = 6, dpi = 300, units = "in", bg = "white")

# Create GDP pc PPP diversion from mean figure ---------------------------------

gdp_data <- generate_summary_data(GDPpcPPPDivFromMean, 
                                  missing_iso = c("JPN", "CHN"))
                                  # data for JPN and CHN is not missing, but
                                  # should be highlighted/dashed (the function 
                                  # just works like that)

# highlight specific countries in the plots

highlight_countries_gdp <- c("PHL", "CHN", "KOR", "HKG",
                             "SGP", "TWN", "JPN", "MYS", "THA")

label_data_gdp <- gdp_data$summary_data %>%
  filter(ISO %in% highlight_countries_gdp) %>%
  group_by(Country) %>%
  filter(Year == max(Year))  # last year in plot data (2020)

gdp_plot <- ggplot(gdp_data$summary_data, 
                   aes(x = Year, 
                       y = GDPpcPPPDivFromMean, 
                       color = Cluster, 
                       group = Country,
                       linetype = missing_data
                       #shape = Cluster
                       )) +
  geom_line(size = 0.5) +
  guides(linetype = "none") +
  #geom_point(size = 1.5) +
  geom_text_repel(data = label_data_gdp,
                  aes(label = Country),
                  size = 4,
                  nudge_x = 5,
                  direction = "y",
                  segment.color = "black",
                  segment.size = 0.4,
                  box.padding = 0.5,
                  show.legend = FALSE) +
  scale_color_manual(values = group_colors) +
  theme_minimal() +
  theme(
    #panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_blank(),  # turn off default lines
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),
    axis.line.y.left = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),  # add ticks
    axis.ticks.length = unit(0.1, "cm"),  # size of ticks
    axis.text = element_text(color = "black", size = 12),
    panel.border = element_blank(),  # remove full border
    panel.grid.minor = element_blank()
  ) +
  labs(title = "GDP per capita, PPP (deviation from sample mean)",
       x = "Year",
       y = "constant 2017 international $",
       color = "Cluster") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0)),
                     breaks = c(2000, 2005, 2010, 2015, 2020)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)))

# group of least developed countries and some emerging economies are hard to
# distinguish in the gdp_plot

gdp_data_low_income <- gdp_data$summary_data %>% 
  filter(ISO %in% emerging | ISO %in% periphery)

label_data_gdp_low_income <- gdp_data_low_income %>% 
  group_by(Country) %>%
  filter(Year == max(Year))

gdp_low_income_plot <- ggplot(gdp_data_low_income, 
                   aes(x = Year, 
                       y = GDPpcPPPDivFromMean, 
                       color = Cluster, 
                       group = Country,
                       linetype = missing_data
                       #shape = Cluster
                       )) +
  geom_line(size = 0.5) +
  guides(linetype = "none") +
  #geom_point(size = 1.5) +
  geom_text_repel(data = label_data_gdp_low_income,
                  aes(label = Country),
                  size = 4,
                  nudge_x = 5,
                  direction = "y",
                  segment.color = "black",
                  segment.size = 0.4,
                  box.padding = 0.5,
                  show.legend = FALSE) +
  scale_color_manual(values = group_colors) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),  # turn off default lines
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),
    axis.line.y.left = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),  # add ticks
    axis.ticks.length = unit(0.1, "cm"),  # size of ticks
    axis.text = element_text(color = "black", size = 12),
    panel.border = element_blank(),  # remove full border
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(#title = "GDP per capita, PPP (deviation from sample mean)",
       x = "Year",
       y = "constant 2017 international $",
       color = "Cluster") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0)),
                     breaks = c(2000, 2005, 2010, 2015, 2020)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)))

gdp_plot_tgt <- ggpubr::ggarrange(
  gdp_plot, 
  gdp_low_income_plot,
  ncol = 2, nrow = 1,
  common.legend = TRUE,
  legend = "bottom"
)

ggsave(here("output/gdp_plot_tgt.png"),
       gdp_plot_tgt,
       width = 11, height = 6, dpi = 300, units = "in", bg = "white")

# gdp mean per group

gdp_mean_plot <- ggplot(gdp_data$summary_mean_data, 
                   aes(x = Year, y = mean, color = Cluster)) +
  geom_ribbon(aes(ymin = min, ymax = max, fill = Cluster),
              alpha = 0.2, color = NA) +
  geom_line(size = 0.5) +
  scale_color_manual(values = group_colors) +
  scale_fill_manual(values = group_colors) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.line = element_blank(),  # turn off default lines
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),
    axis.line.y.left = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),  # add ticks
    axis.ticks.length = unit(0.1, "cm"),  # size of ticks
    axis.text = element_text(color = "black", size = 12),
    panel.border = element_blank(),  # remove full border
    panel.grid.minor = element_blank()
  ) +
  labs(title = "GDP per capita, PPP (deviation from sample mean)",
       x = "Year",
       y = "constant 2017 international $",
       color = "Cluster") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.025)),
                     breaks = c(2000, 2005, 2010, 2015, 2020)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)))

gdp_mean_plot

# Plot for ECI data ------------------------------------------------------------

eci_data <- generate_summary_data(ECI#, 
                                  #missing_iso = c("JPN", "PHL", "VNM", "CHN",
                                  #                "TWN", "MNG")
                                  )

highlight_countries_eci <- east_asia

label_data_eci <- eci_data$summary_data %>%
  filter(ISO %in% highlight_countries_eci) %>%
  group_by(Country) %>%
  filter(Year == max(Year))  # last year in plot data (2020)

eci_plot <- ggplot(eci_data$summary_data, 
                    aes(x = Year, 
                        y = ECI, 
                        color = Cluster, 
                        group = Country,
                        )) +
  geom_line(size = 0.5) +
  #geom_ribbon(aes(ymin = min, ymax = max, fill = Cluster),
   #           alpha = 0.2, color = NA) +
  geom_text_repel(data = label_data_eci,
                  aes(label = Country),
                  size = 4,
                  nudge_x = 4,
                  direction = "y",
                  segment.color = "black",
                  segment.size = 0.4,
                  box.padding = 0.5,
                  show.legend = FALSE) +
  scale_color_manual(values = group_colors) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.line = element_blank(),  # turn off default lines
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),
    axis.line.y.left = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),  # add ticks
    axis.ticks.length = unit(0.1, "cm"),  # size of ticks
    axis.text = element_text(color = "black", size = 12),
    panel.border = element_blank(),  # remove full border
    panel.grid.minor = element_blank()
  ) +
  labs(title = "Economic Complexity Index",
       x = "Year",
       y = "ECI",
       color = "Cluster") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0)),
                     breaks = c(2000, 2005, 2010, 2015, 2020)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)))

ggsave(here("output/eci_plot.png"),
       eci_plot,
       width = 11, height = 6, dpi = 300, units = "in", bg = "white")

# plot for cluster means

eci_mean_plot <- ggplot(eci_data$summary_mean_data, 
                        aes(x = Year, 
                            y = mean, 
                            color = Cluster)) +
  geom_ribbon(aes(ymin = min, ymax = max, fill = Cluster),
              alpha = 0.2, color = NA) +
  geom_line(size = 0.5, show.legend = FALSE) +
  scale_color_manual(values = group_colors) +
  scale_fill_manual(values = group_colors) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.line = element_blank(),  # turn off default lines
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),
    axis.line.y.left = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),  # add ticks
    axis.ticks.length = unit(0.1, "cm"),  # size of ticks
    axis.text = element_text(color = "black", size = 12),
    panel.border = element_blank(),  # remove full border
    panel.grid.minor = element_blank()
  ) +
  labs(title = "Economic Complexity Index",
       x = "Year",
       y = "ECI",
       color = "Cluster") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.025)),
                     breaks = c(2000, 2005, 2010, 2015, 2020)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)))

# Add specific countries to the mean plot

#highlight_ISOs <- c("JPN", "PHL", "VNM", "CHN", "TWN", "MNG", "SGP", "HKG")
#highlight_colors <- c("JPN" = "#42B540", "PHL" = "#ED0000", 
#                      "VNM" = "#925E9F", "CHN" = "#ED0000", 
#                      "TWN" = "#42B540", "MNG" = "#925E9F",
#                      "SGP" = "#0099B4", "HKG" = "#0099B4")
highlight_ISOs <- c("PHL", "MNG", "VNM", "IDN", "CHN", "JPN")
highlight_colors <- c("PHL" = "#ED0000", "MNG" = "#925E9F",
                      "VNM" = "#925E9F", "IDN" = "#925E9F",
                      "CHN" = "#ED0000", "JPN" = "#42B540")
highlight_data <- filter(eci_data$summary_data, ISO %in% highlight_ISOs)
highlight_labels <- highlight_data %>%
  group_by(ISO) %>%
  filter(Year == max(Year)) %>%
  ungroup()

eci_mean_plot_highlights <- eci_mean_plot +
  geom_line(data = highlight_data, 
            aes(x = Year, y = ECI, group = ISO, color = ISO), 
            linewidth = 0.5, show.legend = FALSE, linetype = "dashed") +
  #Add country labels
  geom_text_repel(data = highlight_labels,
                  aes(x = Year, y = ECI, label = Country),
                  size = 4,
                  nudge_x = 1,
                  direction = "y",
                  segment.color = "black",
                  segment.size = 0.4,
                  box.padding = 0.5,
                  show.legend = FALSE) +
  
  scale_color_manual(
    values = c(group_colors, highlight_colors),
    breaks = c(names(group_colors), names(highlight_colors))
  )

eci_mean_plot_highlights

ggsave(here("output/eci_mean_plot.png"),
       eci_mean_plot_highlights,
       width = 11, height = 6, dpi = 300, units = "in", bg = "white")

# Plot for Gini data -----------------------------------------------------------

gini_data <- generate_summary_data(GiniMkt, missing_iso = c("KHM", "MMR", "LAO"))

label_data_gini <- gini_data$summary_data %>%
  #filter(ISO %in% highlight_countries_gini) %>%
  group_by(Country) %>%
  filter(Year == max(Year))  # last year in plot data (2020)

gini_plot <- ggplot(gini_data$summary_data, 
                    aes(x = Year, 
                        y = GiniMkt, 
                        color = Cluster, 
                        group = Country,
                        linetype = missing_data)) +
  geom_line(size = 0.5) +
  geom_text_repel(data = label_data_gini,
                  aes(label = Country),
                  size = 4,
                  nudge_x = 4,
                  direction = "y",
                  segment.color = "black",
                  segment.size = 0.4,
                  box.padding = 0.5,
                  show.legend = FALSE) +
  scale_color_manual(values = group_colors) +
  guides(linetype = "none") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.line = element_blank(),  # turn off default lines
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),
    axis.line.y.left = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),  # add ticks
    axis.ticks.length = unit(0.1, "cm"),  # size of ticks
    axis.text = element_text(color = "black", size = 12),
    panel.border = element_blank(),  # remove full border
    panel.grid.minor = element_blank()
  ) +
  labs(title = "Gini Index (Market Income)",
       x = "Year",
       y = "Gini Index",
       color = "Cluster",
       #linetype = "Missing data"
       ) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0)),
                     breaks = c(2000, 2005, 2010, 2015, 2020)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)))

gini_plot

ggsave(here("output/gini_plot.png"),
       gini_plot,
       width = 11, height = 6, dpi = 300, units = "in", bg = "white")

# Make boxplots to compare country groupings -----------------------------------

plot_cluster_boxplot <- function(data,
                                 varname,
                                 cluster_name,
                                 label = NULL, #y-axis label (default: varname)
                                 title = NULL, # plot title (default: varname)
                                 subtitle = NULL,
                                 color = group_colors,
                                 log_scale = FALSE,
                                 exclude_finance = FALSE,
                                 exclude_emerging = FALSE,
                                 exclude_developmental = FALSE,
                                 # Mongolia has really big negative FDI net 
                                 # inflow in 2016 of -37% of GDP
                                 exclude_mongolia_outlier = FALSE
                                 ) {
  
  var_sym <- sym(varname)
  
  # Prepare data
  data_filtered <- data %>%
    filter(ISO %in% east_asia, Year >= 2000 & Year <= 2019) %>% 
           #, Year >= 2014 & Year <= 2019) %>% #last six years
    mutate(Cluster = case_when(
      ISO %in% periphery ~ "Periphery",
      ISO %in% financial_hubs ~ "Finance",
      ISO %in% developmental ~ "Developmental",
      ISO %in% emerging ~ "Emerging",
      TRUE ~ "Other"
    )) %>%
    #mutate(group = ifelse(Cluster == cluster_name, cluster_name, "Other")) %>%
    mutate(group = factor(
      ifelse(Cluster == cluster_name, cluster_name, "Other"),
      levels = c("Developmental", "Finance", "Emerging", "Periphery", "Other")
    )) %>% 
    { if (exclude_finance) filter(., Cluster != "Finance") else . } %>%
    { if (exclude_emerging) filter(., Cluster != "Emerging") else . } %>%
    { if (exclude_developmental) filter(., Cluster != "Developmental") else . } %>%
    { if (exclude_mongolia_outlier)
      mutate(., FDInetinflow = case_when(
      Year == 2016 & ISO == "MNG" ~ NA_real_,  # Replace specific value with NA
      TRUE ~ FDInetinflow  # Keep all other values as they are
    ) ) else . } %>%
    select(Country, ISO, Year, Cluster, group, !!var_sym) %>%
    drop_na(!!var_sym)
  
  # Compute outliers manually for labeling (not used)
  outliers <- data_filtered %>%
    group_by(group) %>%
    mutate(
      Q1 = quantile(!!var_sym, 0.25, na.rm = TRUE),
      Q3 = quantile(!!var_sym, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      lower = Q1 - 1.5 * IQR,
      upper = Q3 + 1.5 * IQR,
      is_outlier = (!!var_sym < lower | !!var_sym > upper)
    ) %>%
    filter(is_outlier)
  
  # Axis label
  y_label <- if (!is.null(label)) label else varname
  
  #Boxplot title
  plot_title <- if (!is.null(title)) title else varname
  
  p <- ggplot(data_filtered, aes(x = group, y = !!var_sym, fill = group)) +
    geom_boxplot(alpha = 0.8, outlier.color = "black", outlier.size = 1.2) +
    #geom_boxplot(alpha = 0.8, outlier.shape = NA) +  # hide default outliers
    #geom_jitter(width = 0.2, alpha = 0.3) +          # show all points
    #geom_text(data = outliers,
    #          aes(x = group, y = !!var_sym, label = Country),
    #          #aes(x = group, y = !!var_sym, label = ISO),
    #          vjust = -0.5,
    #          #check_overlap = TRUE,
    #          size = 1,
    #          position = position_jitter(width = 0.2),
    #          inherit.aes = FALSE) +
    scale_fill_manual(values = c(setNames(color, cluster_name), Other = "gray70")) +
    labs(title = plot_title,
         subtitle = subtitle,
         x = NULL, 
         y = y_label) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "none",
          axis.line = element_blank(),  # turn off default lines
          axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),
          axis.line.y.left = element_line(color = "black", linewidth = 0.5),
          axis.ticks = element_line(color = "black", linewidth = 0.5),  # add ticks
          axis.ticks.length = unit(0.1, "cm"),  # size of ticks
          axis.text = element_text(color = "black", size = 12),
          panel.border = element_blank(),  # remove full border
          panel.grid.minor = element_blank()
          )
  
  if (log_scale) {
    p <- p + scale_y_log10()
  }
  
  return(p)
}

# Developmental States Group

p3 <- plot_cluster_boxplot(macro_world, "CAinPercGDP", "Developmental", 
                           title = "Current Account",
                           subtitle = "(Excl. HKG & SGP)",
                           label = "% of GDP",
                           color = "#42B540",
                           exclude_finance = TRUE)

p4 <- plot_cluster_boxplot(macro_world, "LibDem", "Developmental", 
                           title = "Liberal Democracy",
                           label = "Liberal Democracy Index",
                           color = "#42B540")

p5 <- plot_cluster_boxplot(macro_world, "ManufacturingShareVA", "Developmental",
                           title = "Manufacturing Sector",
                           subtitle = "(Excl. Emerging)",
                           label = "% of Total Value Added",
                           color = "#42B540",
                           exclude_emerging = TRUE)

Developmental_plot <- (p3 | p4 | p5) + 
  plot_annotation(title = "a) Developmental States", 
                  theme = theme(
                    plot.title = element_text(color = "black", size = 12,
                                              face = "bold")))
Developmental_plot

# Financial Hubs Group

p1 <- plot_cluster_boxplot(macro_world, "FinanceShareVA", "Finance",
                           title = "Finance & Insurance Sector",
                           label = "% of Total Value Added",
                           color = "#0099B4")

p2 <- plot_cluster_boxplot(macro_world, "FDInetoutflow", "Finance",
                           title = "FDI Outflows",
                           label = "% of GDP",
                           color = "#0099B4")

p3 <- plot_cluster_boxplot(macro_world, "CAinPercGDP", "Finance",
                           title = "Current Account",
                           subtitle = "(Excl. Developmental)",
                           label = "% of GDP",
                           color = "#0099B4",
                           exclude_developmental = TRUE)

Finance_plot <- (p1| p2 | p3) + 
  plot_annotation(title = "b) Financial Hubs", 
                  theme = theme(
                    plot.title = element_text(color = "black", size = 12,
                                              face = "bold")))
Finance_plot

# Emerging Economies Group

p1 <- plot_cluster_boxplot(macro_world, "ManufacturingShareVA", "Emerging",
                           title = "Manufacturing Sector",
                           subtitle = "(Excl. Developmental)",
                           label = "% of Total Value Added",
                           color = "#ED0000",
                           exclude_developmental = TRUE)

p2 <- plot_cluster_boxplot(macro_world, "FinanceShareVA", "Emerging",
                           title = "Finance & Insurace Sector",
                           subtitle = "(Excl. HKG & SGP)",
                           label = "% of Total Value Added",
                           color = "#ED0000",
                           exclude_finance = TRUE)

p3 <- plot_cluster_boxplot(macro_world, "GiniMkt", "Emerging",
                           title = "Market Income Inequality",
                           label = "Gini Index",
                           color = "#ED0000")

Emerging_plot <- (p1 | p2 | p3) + 
  plot_annotation(title = "c) Emerging Economies", 
                  theme = theme(
                    plot.title = element_text(color = "black", size = 12,
                                              face = "bold")))
Emerging_plot

# Periphery Group

p1 <- plot_cluster_boxplot(macro_world, "AgricultureShareVA", "Periphery",
                           title = "Agriculture, Forestry, Fishing",
                           label = "% of Total Value Added",
                           color = "#925E9F")

p2 <- plot_cluster_boxplot(macro_world, "MiningShareVA", "Periphery",
                           title = "Mining & Quarrying",
                           label = "% of Total Value Added",
                           color = "#925E9F")

p3 <- plot_cluster_boxplot(macro_world, "FDInetinflow", "Periphery",
                           title = "FDI Inflows",
                           subtitle = "(Excl. HKG & SGP, MNG 2016)",
                           label = "% of GDP",
                           color = "#925E9F",
                           exclude_finance = TRUE,
                           exclude_mongolia_outlier = TRUE
                           )

Periphery_plot <- (p1 | p2 | p3) + 
  plot_annotation(title = "d) Periphery", 
                  theme = theme(
                    plot.title = element_text(color = "black", size = 12,
                                              face = "bold")))
Periphery_plot

boxplots <- ggpubr::ggarrange(
  Developmental_plot, 
  Finance_plot, 
  Emerging_plot, 
  Periphery_plot,
  ncol = 1, nrow = 4#,
  #common.legend = TRUE,
  #legend = "bottom"
)
boxplots

ggsave(here("output/boxplots_complete_period.png"),
       boxplots,
       #width = 7.5, height = 9.23, dpi = 300, units = "in", bg = "white")
       width = 11, height = 13.53734, dpi = 300, units = "in", bg = "white")
