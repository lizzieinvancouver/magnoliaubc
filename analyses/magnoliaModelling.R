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
# install.packages("bayesplot")
library(bayesplot)
library(ggplot2)

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

# getwd()
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
# modbud <- stan_lmer(gdd ~ (1|Name), data = dbud)
# modanthesis <- stan_lmer(gdd ~ (1|Name), data = danthesis)
# modpeak <- stan_lmer(gdd ~ (1|Name), data = dpeak)
# modftepal <- stan_lmer(gdd ~ (1|Name), data = dftepal)
# modltepal <- stan_lmer(gdd ~ (1|Name), data = dltepal)
# modgreen <- stan_lmer(gdd ~ (1|Name), data = dgreen)

# # This is the gdd ~ 1|(event) + 1|(Name)
# modbud2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dbud)
# modanthesis2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = danthesis)
# modpeak2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dpeak)
# modftepal2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dftepal)
# modltepal2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dltepal)
# modgreen2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dgreen)
# # Error in model.matrix.default(eval(substitute(~foo, list(foo = x[[2]]))),  : 
# # model frame and formula mismatch in model.matrix()
# 
# # launch_shinystan(mod1)
# # Just look up how to do this:
#   # we have an rstanarm object, go online figure out how to get summary of parameter values for rstanarm objects, rstanarm has a way to do this elegantly
#   # Print out the output of the parameters for this model
# 
# modbudsum <- summary(modbud, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# modanthesissum <- summary(modanthesis, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# modpeaksum <- summary(modpeak, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# modftepalsum <- summary(modftepal, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# modltepalsum <- summary(modltepal, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# 
# modbudsum
# modanthesissum
# modpeaksum
# modftepalsum
# modltepalsum

# Making a new column to specify species and cultivar name only for the nested structure model
magd$spName <- magd$Name
magd$spName <- sub("\\'.*","",magd$spName)
# Fixing any of the 'Barbara Cook' entries in which no specific epithet is given, and should be M. dawsoniana and 'Forrest's Pink' to be M. denudata
for(i in 1:nrow(magd)){
  if(magd$Name[i] == "M. 'Barbara Cook'"){
    magd$spName[i] <- "M. dawsoniana"
  }
}

for(i in 1:nrow(magd)){
  if(magd$Name[i] == "M. 'Forrest's Pink'"){
    magd$spName[i] <- "M. denudata"
  }
}

magd$cultivarName <- magd$Name
magd$cultivarName <- sub("^[^\\']*","",magd$cultivarName)

# Now giving the empty values (true species) their own identifier
for(i in 1:nrow(magd)){
  if(magd$cultivarName[i] == ""){
    magd$cultivarName[i] <- "botanical species"
  }
}

# subsetting data by the event type
dbud <- filter(magd, event == "First bud colour")
danthesis <- filter(magd, event == "First flower fully open")
dpeak <- filter(magd, event == "Peak bloom")
dftepal <- filter(magd, event == "First tepal drop")
dltepal <- filter(magd, event == "Last tepal drop")
dgreen <- filter(magd, event == "First green seen")

# New model that now uses nesting by species gdd ~ (1 | spName/cultivarName)
modbud3 <- stan_lmer(gdd ~ (1 | spName/cultivarName), data = dbud)
modanthesis3 <- stan_lmer(gdd ~ (1 | spName/cultivarName), data = danthesis)
modpeak3 <- stan_lmer(gdd ~ (1 | spName/cultivarName), data = dpeak)
modftepal3 <- stan_lmer(gdd ~ (1 | spName/cultivarName), data = dftepal)
modltepal3 <- stan_lmer(gdd ~ (1 | spName/cultivarName), data = dltepal)
modgreen3 <- stan_lmer(gdd ~ (1 | spName/cultivarName), data = dgreen)

# Seeing if gdd changes and looking at the species only gdd ~ (1 | spName) + year
modbud4 <- stan_lmer(gdd ~ (1 | spName) + year, data = dbud)
modanthesis4 <- stan_lmer(gdd ~ (1 | spName) + year, data = danthesis)
modpeak4 <- stan_lmer(gdd ~ (1 | spName) + year, data = dpeak)
modftepal4 <- stan_lmer(gdd ~ (1 | spName) + year, data = dftepal)
modltepal4 <- stan_lmer(gdd ~ (1 | spName) + year, data = dltepal)
modgreen4 <- stan_lmer(gdd ~ (1 | spName) + year, data = dgreen)

# Summaries for these models
modbudsum3 <- summary(modbud3, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modanthesissum3 <- summary(modanthesis3, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modpeaksum3 <- summary(modpeak3, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modftepalsum3 <- summary(modftepal3, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modltepalsum3 <- summary(modltepal3, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modgreen3 <- summary(modgreen3, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)

modbudsum4 <- summary(modbud4, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modanthesissum4 <- summary(modanthesis4, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modpeaksum4 <- summary(modpeak4, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modftepalsum4 <- summary(modftepal4, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modltepalsum4 <- summary(modltepal4, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modgreen4 <- summary(modgreen4, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)

# sink("modelOutputs/modbudsum3.txt")
# modbudsum3
# sink()
# 
# sink("modelOutputs/modanthesissum3.txt")
# modanthesissum3
# sink()
# 
# sink("modelOutputs/modpeaksum3.txt")
# modpeaksum3
# sink()
# 
# sink("modelOutputs/modftepalsum3.txt")
# modftepalsum3
# sink()
# 
# sink("modelOutputs/modltepalsum3.txt")
# modltepalsum3
# sink()
# 
# sink("modelOutputs/modgreen3.txt")
# modgreen3
# sink()
# 
# sink("modelOutputs/modbudsum4.txt")
# modbudsum4
# sink()
# 
# sink("modelOutputs/modanthesissum4.txt")
# modanthesissum4
# sink()
# 
# sink("modelOutputs/modpeaksum4.txt")
# modpeaksum4
# sink()
# 
# sink("modelOutputs/modftepalsum4.txt")
# modftepalsum4
# sink()
# 
# sink("modelOutputs/modltepalsum4.txt")
# modltepalsum4
# sink()
# 
# sink("modelOutputs/modgreen4.txt")
# modgreen4
# sink()

# Visualizing the summaries
modbud3.post <- as.array(modbud3)
dimnames(modbud3.post)
mcmc_intervals(modbud3.post, pars = vars(1:89))


