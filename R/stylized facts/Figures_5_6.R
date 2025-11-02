rm(list = ls())
library(here)
source(here("packages.R"))

load(here("data/macro_world.RData"))

# Proposed country groupings ---------------------------------------------------

periphery <- c("VNM", "MNG", "MMR", "KHM", "LAO", "IDN")

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

# Figure 5: Create GDP pc PPP diversion from mean figure -----------------------

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

ggsave(here("output/gdp_plot_tgt.pdf"),
       gdp_plot_tgt,
       width = 11, height = 6, dpi = 300, units = "in", bg = "white")

ggsave(here("output/gdp_plot_tgt.svg"),
       gdp_plot_tgt,
       width = 11, height = 6, dpi = 300, units = "in", bg = "white")

# Figure 6: Plot for ECI data --------------------------------------------------

eci_data <- generate_summary_data(ECI#, 
                                  #missing_iso = c("JPN", "PHL", "VNM", "CHN",
                                  #                "TWN", "MNG")
)

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

ggsave(here("output/eci_mean_plot.pdf"),
       eci_mean_plot_highlights,
       width = 11, height = 6, dpi = 300, units = "in", bg = "white")

ggsave(here("output/eci_mean_plot.svg"),
       eci_mean_plot_highlights,
       width = 11, height = 6, dpi = 300, units = "in", bg = "white")