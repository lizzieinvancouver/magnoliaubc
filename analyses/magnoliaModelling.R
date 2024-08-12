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
# install.packages("shinystan")
library(shinystan)

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

# mod1 <- stan_lmer(gdd ~ event | Name, data = magd) 
# This gives us the average gdd for each event but also try to partition by the different Names
# this model took 22 minutes to run, or 1326.273 seconds for all 4 chains with an average of approximately 331 seconds per chain

# Tell Lizzie: how many rows of data there are for very event | Name, what are the unique combinations of that? Remove some data that's super sparse, like the cultivars only have a couple years of data
unique(magd$Name) #53 unique cultivars or species
unique(magd$event) # 6 event types, but we can remove first green seen because it has very few years of data
# Therefore approximately 265 unique combinations of event | Name

# subsetting data by the event type
dbud <- filter(magd, event == "First bud colour")
danthesis <- filter(magd, event == "First flower fully open")
dpeak <- filter(magd, event == "Peak bloom")
dftepal <- filter(magd, event == "First tepal drop")
dltepal <- filter(magd, event == "Last tepal drop")
dgreen <- filter(magd, event == "First green seen")

# This is the gdd ~ (1|Name) model structure
modbud <- stan_lmer(gdd ~ (1|Name), data = dbud)
modanthesis <- stan_lmer(gdd ~ (1|Name), data = danthesis)
modpeak <- stan_lmer(gdd ~ (1|Name), data = dpeak)
modftepal <- stan_lmer(gdd ~ (1|Name), data = dftepal)
modltepal <- stan_lmer(gdd ~ (1|Name), data = dltepal)
modgreen <- stan_lmer(gdd ~ (1|Name), data = dgreen)

# This is the gdd ~ 1|(event) + 1|(Name)
modbud2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dbud)
modanthesis2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = danthesis)
modpeak2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dpeak)
modftepal2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dftepal)
modltepal2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dltepal)
modgreen2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dgreen)
# Error in model.matrix.default(eval(substitute(~foo, list(foo = x[[2]]))),  : 
# model frame and formula mismatch in model.matrix()

# launch_shinystan(mod1)
# Just look up how to do this:
  # we have an rstanarm object, go online figure out how to get summary of parameter values for rstanarm objects, rstanarm has a way to do this elegantly
  # Print out the output of the parameters for this model

modbudsum <- summary(modbud, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modanthesissum <- summary(modanthesis, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modpeaksum <- summary(modpeak, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modftepalsum <- summary(modftepal, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modltepalsum <- summary(modltepal, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)

modbudsum
modanthesissum
modpeaksum
modftepalsum
modltepalsum
