# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# 
# Magnolia modelling combining temperature and data
# 
# Started by Justin
# 16 July 2024
# 
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list=ls())

library(rstanarm)
library(tidyverse)

# Loading in the temperature and floral phenology data
source("analyses/magnoliaTemperature.R")
dmag <- read.csv("analyses/output/magnoliaClean.csv")
dtemp <- d #keeping it consistent format

# Just a little bit of adjustments to the data
dmag <- dmag %>%
  select(-1)

# Trying to merge the dtemp into dmag and hope they match by date
dmag$value <- as.Date(dmag$value)
dmag <- dmag %>%
  mutate(ID1 = value) %>%
  group_by(value) %>%
  mutate(ID2 = row_number()) %>%
  ungroup()
dtemp <- dtemp %>%
  mutate(ID1 = date) %>%
  group_by(date) %>%
  mutate(ID2 = row_number()) %>%
  ungroup()

dmerge <- full_join(dmag, dtemp, by = c("ID1","ID2"))
# Ya idk 
# If I don't go group_by() the two datasets multiply by each other
# If I do use group_by() then dtemp doesn't replicate every time a date appears twice
