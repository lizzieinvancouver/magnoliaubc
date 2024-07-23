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
dmag$gdd <- NA

# for(i in c(1:nrow(dmag))) { #i = 1
#   gddhere <- dtemp$gddAccumulate[which(dtemp$year == dmag$year[i] & dtemp$doy == dmag$DOY[i])]
#   dmag$gdd[i] <- gddhere
# }

for(i in c(1:nrow(dmag))) {
  match_row <- which(dtemp$year == dmag$year[i] & dtemp$doy == dmag$DOY[i])
  
  if (length(match_row) > 0) {
    dmag$gdd[i] <- dtemp$gddAccumulate[match_row]
  } else {
    dmag$gdd[i] <- NA
  }
}

getwd()
# write.csv(dmag, "analyses/output/magnoliaAll.csv")
# write.csv(dmag, "analyses/input/magnoliaAll.csv")


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Now to get onto the actual modelling parts?

magd <- read_csv("analyses/output/magnoliaAll.csv")

mod1 <- stan_lmer(gdd ~ event | Name, data = magd) #This gives us the average gdd for each event but also try to partition by the different Names

