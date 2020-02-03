library(dplyr)
library(ggplot2)
library(maps)

cod_county_2017 <- read.delim("cod_county_2017.txt")

by_county <- cod_county_2017 %>% 
  select(fips = County.Code, Deaths) %>% 
  filter(!is.na(fips)) %>% 
  group_by(fips) %>% 
  summarize(tot_deaths = sum(Deaths)) %>% 
  as.data.frame()

p1 <- usmap::plot_usmap(regions = "counties", data = by_county, values = "tot_deaths", color = NA) +
  scale_fill_continuous(
    low = "navy", high = "orange", name = "Total Deaths"
  ) +
  labs(title = "Total deaths in the United States due to mental and behavioral disorders (1999 - 2017)",
       subtitle = "Mortality figures less than 10 persons are suppressed (shown in gray)",
       caption = "CDC WONDER Underlying Cause of Death ") +
  theme(
    text = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    plot.margin=grid::unit(c(0,0,0,0), "mm")
  )

ggsave("county_plot.png", p1)

p2 <- usmap::plot_usmap(regions = "counties", data = by_county, values = "tot_deaths", color = NA, include = "IL") +
  scale_fill_continuous(
    low = "navy", high = "orange", name = "Total Deaths"
  ) +
  labs(title = "Deaths due to mental and behavioral disorders in Illinois (1999 - 2017)",
       subtitle = "Mortality figures less than 10 persons are suppressed (shown in gray)",
       caption = "CDC WONDER Underlying Cause of Death ") +
  theme(
    text = element_text(color = "white"),
    legend.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black", color = NA),
    aspect.ratio = 1
  )

ggsave("il_plot.png", p2, width = 10, height = 10)
