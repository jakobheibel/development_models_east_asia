rm(list = ls())
library(here)
source(here("packages.R"))

load(here("data/macro_world.RData"))

ISO_list_EA3 <- c("TWN", "CHN", "HKG", "IDN", "JPN", "KOR", "MYS", "PHL", "SGP",
                  "THA", "VNM", "MNG", "MMR", "KHM", "LAO")

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
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.07))) +
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