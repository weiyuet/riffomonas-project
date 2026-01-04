##########################
# NCAA Basketball Finals #
##########################

#### Setup ####
# Load Libraries
library(tidyverse)
library(showtext)
library(rvest)
library(glue)

# Load Additional Fonts
font_add_google("Libre Franklin",
                "franklin")
font_add_google("Domine",
                "domine")
showtext_opts(dpi = 300)
showtext_auto()

#### Load Data ####
# Women's Viewership
womens_data <- "https://www.nielsen.com/news-center/2024/womens-college-basketball-championship-draws-record-breaking-18-9-million-viewers/"

# Scraping Data from html
womens_viewership <- read_html(womens_data) %>% 
  html_element("tbody") %>% 
  html_table() %>% 
  select(year = X1, 
         viewership = X6) %>% 
  mutate(viewership = str_replace_all(viewership,
                                      ",",
                                      "") %>% as.numeric(),
         women = viewership / 1000000) %>% 
  select(-viewership)

# Men's Viewership
mens_data <- "https://www.sportsmediawatch.com/ncaa-final-four-ratings-history-most-watched-games-cbs-tbs-nbc/"

# Scraping Data from html
mens_years <- read_html(mens_data) %>% 
  html_elements("h4") %>% 
  html_text() %>% 
  str_subset("^\\d{4}$")

mens_viewership <- read_html(mens_data) %>% 
  html_nodes("table") %>% 
  map_dfr(~html_table(.x)) %>% 
  filter(Window == "Champ.") %>% 
  mutate(year = mens_years) %>% 
  select(year, viewership = Vwrs) %>% 
  mutate(viewership = str_replace(viewership,
                                      "M\n?.*",
                                      ""),
         viewership = na_if(viewership,
                            "n.a."),
         men = as.numeric(viewership),
         year = as.numeric(year)) %>% 
  select(-viewership)

# Combine Women's and Men's Viewership Data
combined_viewership <- full_join(womens_viewership,
          mens_viewership,
          by = "year") %>% 
  pivot_longer(-year,names_to = "tournament",
               values_to = "viewership_millions") %>% 
  bind_rows(tibble(year = 2020,
                   tournament = c("women",
                                  "men"),
                   viewership_millions = c(NA,
                                           NA))) %>% 
  filter(year >= 1995) %>% 
  arrange(-year)

#### Visualize ####
# Create right side labels
label_data <- combined_viewership %>%
  filter(year == 2024) %>% 
  mutate(label = if_else(tournament == "women",
                         glue("Women's:\n{round(viewership_millions, 1)} million"),
                         glue("Men's:\n{round(viewership_millions, 1)} million")))

# Plot Data
combined_viewership %>% 
  ggplot(aes(x = year,
             y = viewership_millions,
             colour = tournament)) +
  geom_line(linewidth = 0.8,
            show.legend = FALSE) +
  geom_text(data = label_data,
            show.legend = FALSE,
            aes(x = year + 1,
                y = viewership_millions,
                label = label),
            hjust = 0,
            vjust = c(0.8, 0.9),
            family = "franklin",
            fontface = "bold",
            size = 9,
            size.unit = "pt",
            lineheight = 1) +
  geom_point(data = label_data,
             show.legend = FALSE,
             aes(x = year,
                 y = viewership_millions)) +
  annotate(geom = "text",
           hjust = 0,
           family = "franklin",
           colour = "gray40",
           size = 8.5,
           size.unit = "pt",
           x = 1992,
           y = c(seq(5, 20, 5) + 0.6, 25),
           label = c(seq(5, 20, 5), "25 million\nviewers")) +
  annotate(geom = "text",
           hjust = 0,
           vjust = 0,
           family = "franklin",
           colour = "black",
           size = 9,
           size.unit = "pt",
           x = 2025,
           y = 20,
           label = "2024 Finals"
           ) +
  labs(x = NULL,
       y = NULL,
       title = "N.C.A.A Basketball Championship Viewers, 1995-2024",
       caption = "Source: Nielsen . By The New York Times") +
  scale_y_continuous(limits = c(0, NA),
                     breaks = seq(0, 30, 5)) +
  scale_x_continuous(limits = c(1992,
                     2025),
                     breaks = seq(1995, 2025, 5)) +
  scale_colour_manual(breaks = c("men", "women"),
                      values = c("#999999", "#E57D01")) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  theme(text = element_text(family = "franklin",),
        plot.title = element_text(family = "domine",
                                  face = "bold",
                                  size = 12.5,
                                  margin = margin(t = 3,
                                                  b = 14)),
        plot.caption = element_text(hjust = 0,
                                    colour = "gray40",
                                    size = 8,
                                    margin = margin(t = 12)),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(4, "pt"),
        axis.ticks.x = element_line(linewidth = 0.2,
                                    colour = "gray70"),
        axis.text.x = element_text(colour = "gray40",
                                   margin = margin(t = 4)),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray70",
                                          linewidth = 0.2),
        plot.margin = margin(t = 5, r = 75, b = 5, l = 5))

#### Save Image ####
ggsave("figures/ncaa-basketball-finals.png", 
       width = 8, 
       height = 5)