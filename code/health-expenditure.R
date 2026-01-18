######################
# Health Expenditure #
######################

#### Setup ####
# Load libraries
library(tidyverse)
library(showtext)
library(ggtext)

# Load additional fonts
font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 300)

#### Load Data ####
# Read data and prep data
health_expenditure_raw <- read_tsv("https://datawrapper.dwcdn.net/Bxhol/9/dataset.csv")

health_expenditure_clean <- health_expenditure_raw %>%
  select(country, year, spend, life_expectancy = le) %>%
  arrange(country, year) %>%
  mutate(
    current_year = year == max(health_expenditure_raw$year),
    plot_size = year %% 2000,
    plot_size = if_else(
      year == max(health_expenditure_raw$year),
      40,
      plot_size + 1
    ),
    country = factor(
      country,
      levels = c("Canada", "France", "Germany", "Japan", "Italy", "UK", "US")
    )
  )

# Create country_labels for plot
country_labels <- tribble(
  ~country  , ~year , ~spend , ~life_expectancy , ~xnudge , ~ynudge ,
  "Canada"  ,  2023 ,   5307 , 82.6             ,     550 , 0       ,
  "France"  ,  2023 ,   5014 , 83.3             ,     500 , 0       ,
  "Germany" ,  2023 ,   5971 , 81.4             ,     600 , 0       ,
  "Italy"   ,  2023 ,   3249 , 83.7             ,       0 , 0.55    ,
  "Japan"   ,  2023 ,   4874 , 84.7             ,     450 , 0       ,
  "UK"      ,  2023 ,   4444 , 81.3             ,     350 , 0       ,
  "US"      ,  2023 ,  10827 , 79.3             ,       0 , 0.5     ,
)

#### Plot Data ####
health_expenditure_clean %>%
  ggplot(aes(
    x = spend,
    y = life_expectancy,
    fill = country,
    color = current_year,
    size = plot_size
  )) +
  geom_point(shape = 21, stroke = 0.25) +
  annotate(
    geom = "rect",
    xmin = 2090,
    xmax = 2500,
    ymin = 86.44,
    ymax = 86.97,
    fill = "#E94F55"
  ) +
  annotate(
    geom = "text",
    x = c(2200, 11300),
    y = c(85.8, 77.5),
    label = c("Life Expectancy", "Per-capital\nspend"),
    hjust = c(0, 1),
    size = 9,
    size.unit = "pt",
    color = "gray50",
    lineheight = 0.9,
    fontface = "bold",
    family = "roboto"
  ) +
  annotate(
    geom = "text",
    x = c(4300, 6500),
    y = c(78, 81),
    label = c(2000, 2023),
    color = "#F5C55E",
    size = 8,
    size.unit = "pt",
    family = "roboto"
  ) +
  geom_text(
    data = country_labels,
    inherit.aes = FALSE,
    color = "gray50",
    family = "roboto",
    size = 9,
    size.unit = "pt",
    nudge_x = country_labels$xnudge,
    nudge_y = country_labels$ynudge,
    aes(x = spend, y = life_expectancy, label = country)
  ) +
  scale_color_manual(breaks = c(FALSE, TRUE), values = c("white", "black")) +
  scale_fill_manual(
    breaks = c("Canada", "France", "Germany", "Japan", "Italy", "UK", "US"),
    values = c(
      "#FFAEA9",
      "#80B1E2",
      "#F5C55E",
      "#DACFC0",
      "#61A961",
      "#E94F55",
      "#4076A4"
    )
  ) +
  scale_size_continuous(range = c(1, 6)) +
  scale_x_continuous(
    breaks = seq(3000, 11000, 1000),
    labels = c(
      format(seq(3000, 10000, 1000), big.mark = ",", trim = TRUE),
      "$11,000"
    )
  ) +
  coord_cartesian(
    xlim = c(2200, 11300),
    ylim = c(77, 86),
    expand = FALSE,
    clip = "off"
  ) +
  theme(
    text = element_text(family = "roboto"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(
      face = "bold",
      size = 13,
      margin = margin(l = 8, b = 5)
    ),
    plot.subtitle = element_markdown(
      lineheight = 1.3,
      margin = margin(t = 3, l = 10, b = 11)
    ),
    plot.caption = element_markdown(
      hjust = 0,
      size = 7,
      lineheight = 1.3,
      margin = margin(t = 12, l = 8)
    ),
    axis.title = element_blank(),
    axis.text = element_text(color = "black"),
    axis.ticks = element_blank(),
    axis.line = element_line(linewidth = 0.25),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, color = "gray"),
    panel.background = element_rect(fill = "white"),
    legend.position = "none"
  ) +
  labs(
    title = "Value for money",
    subtitle = "How life expectancy and per-capita healthcare spend have changed since 2000.<br><span style='color:white'>**UK**</span> spending is rising, but life expectancy has stalled",
    caption = "In US Dollars, adjusted for purchasing power and inflation.<br>Excludes 2020-22. <span style='color:gray60'>**Chart: Tom Calver | The Times and The Sunday Times**</span>"
  )

#### Save Image ####
ggsave("figures/health-expenditure.png", width = 6, height = 4.5)

# End
