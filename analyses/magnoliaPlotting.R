# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# 
# Magnolia modelling graphing of DOYs
# 
# Refurbished by Justin
# 17 October 2024
# 
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(tidyverse)
library(ggplot2)
library(ggthemes)

# getting data
plotd <- read_csv("analyses/input/magnoliaPlotting.csv")

# Plotting the beautiful magnolia phenological events by species over time

unique(plotd$spName)

plotd  %>%
  filter(grepl("campbellii",spName))%>%
  ggplot(aes(x = year,
             y = gdd,
             colour = event))+
  geom_line(linewidth = 1)+
  geom_point() +
  theme_clean()+
  labs(x = "Year",
       y = "Julian day of year of phenological event occurrence")+
  facet_grid(`Ref..No.` ~ .) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

plotd  %>%
  filter(grepl("sprengeri",spName))%>%
  ggplot(aes(x = year,
             y = DOY,
             colour = event))+
  geom_line(linewidth = 1)+
  geom_point() +
  theme_clean()+
  labs(x = "Year",
       y = "Julian day of year of phenological event occurrence")+
  facet_grid(`Ref..No.` ~ .) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

plotd  %>%
  filter(grepl("sargentiana",spName))%>%
  ggplot(aes(x = year,
             y = DOY,
             colour = event))+
  geom_line(linewidth = 1)+
  geom_point() +
  theme_clean()+
  labs(x = "Year",
       y = "Julian day of year of phenological event occurrence")+
  facet_grid(`Ref..No.` ~ .) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

plotd  %>%
  filter(grepl("dawsoniana",spName))%>%
  ggplot(aes(x = year,
             y = DOY,
             colour = event))+
  geom_line(linewidth = 1)+
  geom_point() +
  theme_clean()+
  labs(x = "Year",
       y = "Julian day of year of phenological event occurrence")+
  facet_grid(`Ref..No.` ~ .) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

plotd  %>%
  filter(grepl("stellata",spName))%>%
  ggplot(aes(x = year,
             y = DOY,
             colour = event))+
  geom_line(linewidth = 1)+
  geom_point() +
  theme_clean()+
  labs(x = "Year",
       y = "Julian day of year of phenological event occurrence")+
  facet_grid(`Ref..No.` ~ .) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

plotd  %>%
  filter(grepl("denudata",spName))%>%
  ggplot(aes(x = year,
             y = DOY,
             colour = event))+
  geom_line(linewidth = 1)+
  geom_point() +
  theme_clean()+
  labs(x = "Year",
       y = "Julian day of year of phenological event occurrence")+
  facet_grid(`Ref..No.` ~ .) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

plotd  %>%
  filter(grepl("cylindrica",spName))%>%
  ggplot(aes(x = year,
             y = DOY,
             colour = event))+
  geom_line(linewidth = 1)+
  geom_point() +
  theme_clean()+
  labs(x = "Year",
       y = "Julian day of year of phenological event occurrence")+
  facet_grid(`Ref..No.` ~ .) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

plotd  %>%
  filter(grepl("zenii",spName))%>%
  ggplot(aes(x = year,
             y = gdd,
             colour = event))+
  geom_line(linewidth = 1)+
  geom_point() +
  theme_clean()+
  labs(x = "Year",
       y = "Julian day of year of phenological event occurrence")+
  facet_grid(`Ref..No.` ~ .) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

  # Not pictured: amoena, conifera, sapaensis, biondii, maudiae, cavaleriei, chevalieri, laevifolia








