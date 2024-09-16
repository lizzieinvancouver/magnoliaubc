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
library(ggthemes)

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
write.csv(dmag, "analyses/output/magnoliaAll.csv")
write.csv(dmag, "analyses/input/magnoliaAll.csv")


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

# # subsetting data by the event type
# dbud <- filter(magd, event == "First bud colour")
# danthesis <- filter(magd, event == "First flower fully open")
# dpeak <- filter(magd, event == "Peak bloom")
# dftepal <- filter(magd, event == "First tepal drop")
# dltepal <- filter(magd, event == "Last tepal drop")
# dgreen <- filter(magd, event == "First green seen")
# 
# # This is the gdd ~ (1|Name) model structure
# # modbud <- stan_lmer(gdd ~ (1|Name), data = dbud)
# # modanthesis <- stan_lmer(gdd ~ (1|Name), data = danthesis)
# # modpeak <- stan_lmer(gdd ~ (1|Name), data = dpeak)
# # modftepal <- stan_lmer(gdd ~ (1|Name), data = dftepal)
# # modltepal <- stan_lmer(gdd ~ (1|Name), data = dltepal)
# # modgreen <- stan_lmer(gdd ~ (1|Name), data = dgreen)
# 
# # # This is the gdd ~ 1|(event) + 1|(Name)
# # modbud2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dbud)
# # modanthesis2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = danthesis)
# # modpeak2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dpeak)
# # modftepal2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dftepal)
# # modltepal2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dltepal)
# # modgreen2 <- stan_lmer(gdd ~ 1|(event) + 1|(Name), data = dgreen)
# # # Error in model.matrix.default(eval(substitute(~foo, list(foo = x[[2]]))),  : 
# # # model frame and formula mismatch in model.matrix()
# # 
# # # launch_shinystan(mod1)
# # # Just look up how to do this:
# #   # we have an rstanarm object, go online figure out how to get summary of parameter values for rstanarm objects, rstanarm has a way to do this elegantly
# #   # Print out the output of the parameters for this model
# # 
# # modbudsum <- summary(modbud, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# # modanthesissum <- summary(modanthesis, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# # modpeaksum <- summary(modpeak, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# # modftepalsum <- summary(modftepal, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# # modltepalsum <- summary(modltepal, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# # 
# # modbudsum
# # modanthesissum
# # modpeaksum
# # modftepalsum
# # modltepalsum

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

unique(magd$spName)

magd$cultivarName <- magd$Name
magd$cultivarName <- sub("^[^\\']*","",magd$cultivarName)
magd$spName[which(magd$spName == "M  sprengeri ")] <- "M. sprengeri"
magd$spName[which(magd$spName == "M. sprengeri ")] <- "M. sprengeri"
magd$spName[which(magd$spName == "M. campbellii ")] <- "M. campbellii"
magd$spName[which(magd$spName == "M. campbellii (Alba Group) ")] <- "M. campbellii (Alba Group)"
magd$spName[which(magd$spName == "M. sargentiana (pale flower selection ex. ")] <- "M. sargentiana (pale flower selection ex.)"
magd$spName[which(magd$spName == "M. campbellii subsp. mollicomata ")] <- "M. campbellii subsp. mollicomata"
magd$spName[which(magd$spName == "M. campbellii subsp. campbellii ")] <- "M. campbellii subsp. campbellii"
magd$spName[which(magd$spName == "M. denudata ")] <- "M. denudata"
magd$spName[which(magd$spName == "M. sargentiana ")] <- "M. sargentiana"
magd$spName[which(magd$spName == "M. dawsoniana ")] <- "M. dawsoniana"

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

# # Seeing if gdd changes and looking at the species only gdd ~ (1 | spName) + year
# modbud4 <- stan_lmer(gdd ~ (1 | spName) + year, data = dbud)
# modanthesis4 <- stan_lmer(gdd ~ (1 | spName) + year, data = danthesis)
# modpeak4 <- stan_lmer(gdd ~ (1 | spName) + year, data = dpeak)
# modftepal4 <- stan_lmer(gdd ~ (1 | spName) + year, data = dftepal)
# modltepal4 <- stan_lmer(gdd ~ (1 | spName) + year, data = dltepal)
# modgreen4 <- stan_lmer(gdd ~ (1 | spName) + year, data = dgreen)

# Summaries for these models
modbudsum3 <- summary(modbud3, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modanthesissum3 <- summary(modanthesis3, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modpeaksum3 <- summary(modpeak3, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modftepalsum3 <- summary(modftepal3, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modltepalsum3 <- summary(modltepal3, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
modgreen3 <- summary(modgreen3, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)

# modbudsum4 <- summary(modbud4, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# modanthesissum4 <- summary(modanthesis4, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# modpeaksum4 <- summary(modpeak4, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# modftepalsum4 <- summary(modftepal4, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# modltepalsum4 <- summary(modltepal4, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)
# modgreen4 <- summary(modgreen4, pars = NULL, regex_pars = NULL, probs = c(0.1, 0.5, 0.9), digits = 1)

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
mcmc_intervals(modbud3.post, pars = vars(1:79))

# dimnames(modbud3.post)[[1]] <- "(Intercept)"
#                                "b[(Intercept) cultivarName:spName:Alba):M._sargentiana_(pale_flower_selection_ex.)]",
#                                  "b[(Intercept) cultivarName:spName:Barbara_Cook:M._dawsoniana]",
#                                  "b[(Intercept) cultivarName:spName:Betty_Jessel:M._campbellii_subsp._campbellii]",
#                                  "b[(Intercept) cultivarName:spName:Blood_Moon:M._sargentiana]",
#                                  "b[(Intercept) cultivarName:spName:Blood_Moon_on_M._sprengeri:M._sargentiana]",
#                                  "b[(Intercept) cultivarName:spName:Borde_Hill:M._campbellii]",
#                                  "b[(Intercept) cultivarName:spName:Borde_Hill:M._campbellii_subsp._mollicomata]",
#                                  "b[(Intercept) cultivarName:spName:Chyverton_Dark:M._sargentiana]",
#                                  "b[(Intercept) cultivarName:spName:Chyverton_Red:M._dawsoniana]",
#                                  "b[(Intercept) cultivarName:spName:Chyverton_Red__(ex._Chyverton):M._dawsoniana]",
#                                  "b[(Intercept) cultivarName:spName:Claret_Cup:M._sprengeri]",
#                                  "b[(Intercept) cultivarName:spName:Clark:M._dawsoniana]",
#                                  "b[(Intercept) cultivarName:spName:Clarke:M._dawsoniana]",
#                                  "b[(Intercept) cultivarName:spName:Copeland_Court:M._sprengeri]",
#                                  "b[(Intercept) cultivarName:spName:Diva:M._sprengeri]",
#                                  "b[(Intercept) cultivarName:spName:Eric_Savill:M._sprengeri]",
#                                  "b[(Intercept) cultivarName:spName:Ethel_Hillier:M._campbellii]",
#                                  "b[(Intercept) cultivarName:spName:Ethel_Hillier:M._campbellii_(Alba_Group)]",
#                                  "b[(Intercept) cultivarName:spName:Forrests_Pink:M._denudata]",
#                                  "b[(Intercept) cultivarName:spName:Japanese_Clone:M._denudata]",
#                                  "b[(Intercept) cultivarName:spName:Lanarth:M._campbellii]",
#                                  "b[(Intercept) cultivarName:spName:Lanarth:M._campbellii_subsp._mollicomata]",
#                                  "b[(Intercept) cultivarName:spName:Landicla:M._campbellii]",
#                                  "b[(Intercept) cultivarName:spName:Wadas_Japanese_Clone:M._denudata]",
#                                  "b[(Intercept) cultivarName:spName:Wakehurst:M._sprengeri]",
#                                  "b[(Intercept) cultivarName:spName:Wakehurst_A.M.:M._sprengeri]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._amoena]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._biondii]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._campbellii]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._campbellii_(deep_pink_form)]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._campbellii_(F.C.C._form)]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._campbellii_(hybrid)]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._campbellii_subsp._mollicomata]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._cavaleriei_var._platypetala]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._chevalieri]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._conifera]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._cylindrica]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._dawsoniana]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._denudata]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._laevifolia]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._maudiae]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._maudiae_var._platypetala]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._officinalis_biloba]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._sapaensis]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._sargentiana]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._sargentiana_(ex._var._robusta)]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._sprengeri]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._stellata]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._zenii]",
#                                  "b[(Intercept) cultivarName:spName:botanical_species:M._zenii_(clone_no._1)]",
#                                  "b[(Intercept) spName:M._amoena]",
#                                  "b[(Intercept) spName:M._biondii]",
#                                  "b[(Intercept) spName:M._campbellii]",
#                                  "b[(Intercept) spName:M._campbellii_(Alba_Group)]",
#                                  "b[(Intercept) spName:M._campbellii_(deep_pink_form)]",
#                                  "b[(Intercept) spName:M._campbellii_(F.C.C._form)]",
#                                  "b[(Intercept) spName:M._campbellii_(hybrid)]",
#                                  "b[(Intercept) spName:M._campbellii_subsp._campbellii]",
#                                  "b[(Intercept) spName:M._campbellii_subsp._mollicomata]",
#                                  "b[(Intercept) spName:M._cavaleriei_var._platypetala]",
#                                  "b[(Intercept) spName:M._chevalieri]",
#                                  "b[(Intercept) spName:M._conifera]",
#                                  "b[(Intercept) spName:M._cylindrica]",
#                                  "b[(Intercept) spName:M._dawsoniana]",
#                                  "b[(Intercept) spName:M._denudata]",
#                                  "b[(Intercept) spName:M._laevifolia]",
#                                  "b[(Intercept) spName:M._maudiae]",
#                                  "b[(Intercept) spName:M._maudiae_var._platypetala]",
#                                  "b[(Intercept) spName:M._officinalis_biloba]",
#                                  "b[(Intercept) spName:M._sapaensis]",
#                                  "b[(Intercept) spName:M._sargentiana]",
#                                  "b[(Intercept) spName:M._sargentiana_(ex._var._robusta)]",
#                                  "b[(Intercept) spName:M._sargentiana_(pale_flower_selection_ex.)]",
#                                  "b[(Intercept) spName:M._sprengeri]",
#                                  "b[(Intercept) spName:M._stellata]",
#                                  "b[(Intercept) spName:M._zenii]",
#                                  "b[(Intercept) spName:M._zenii_(clone_no._1)]",
#                                  "sigma",
#                                  "Sigma[cultivarName:spName:(Intercept),(Intercept)]",
#                                  "Sigma[spName:(Intercept),(Intercept)]")

modanthesis3.post <- as.array(modanthesis3)
dimnames(modanthesis3.post)
mcmc_intervals(modanthesis3.post, pars = vars(1:75))

modpeak3.post <- as.array(modpeak3)
dimnames(modpeak3.post)
mcmc_intervals(modpeak3.post, pars = vars(1:73))

modftepal3.post <- as.array(modftepal3)
dimnames(modftepal3.post)
mcmc_intervals(modftepal3.post, pars = vars(1:74))

modltepal3.post <- as.array(modltepal3)
dimnames(modltepal3.post)
mcmc_intervals(modltepal3.post, pars = vars(1:75))

# ONLY SPECIES
modbud3.post <- as.array(modbud3)
dimnames(modbud3.post)
mcmc_intervals(modbud3.post, pars = vars(52:78))

modanthesis3.post <- as.array(modanthesis3)
dimnames(modanthesis3.post)
mcmc_intervals(modanthesis3.post, pars = vars(50:74))

modpeak3.post <- as.array(modpeak3)
dimnames(modpeak3.post)
mcmc_intervals(modpeak3.post, pars = vars(49:72))

modftepal3.post <- as.array(modftepal3)
dimnames(modftepal3.post)
mcmc_intervals(modftepal3.post, pars = vars(50:73))

modltepal3.post <- as.array(modltepal3)
dimnames(modltepal3.post)
mcmc_intervals(modltepal3.post, pars = vars(50:74))

sort(unique(magd$Name))
# Make a plot where pars = vars() only for just the species
# Species vary with a variance of this much...that's the last parameter here
# Can divide it up into species, subspecies, and then cultivar, and if the subspecies are super similar leave them in as cultivars

# Write up
# First paragraph like what's interseting about this dataset, things we might wanna learn about climate and its effects on magnolias, variation in the GDD of the events, and then the two models, and then also show the climate data...we wouldn't expect change from this...briefly mention the linear regression of warming over time...since there's no changes in the temperatuer maybe we look at gdds, and then description of the data

# install.packages("tidybayes")
# install.packages("ggplot2")
library(tidybayes)
# budtable2 <- modbud3 %>%
#   spread_draws((Intercept),
#                b[(Intercept) cultivarName:spName:'Alba'):M._sargentiana_(pale_flower_selection_ex.)],
#                b[(Intercept) cultivarName:spName:'Barbara_Cook':M._dawsoniana],                       
#                b[(Intercept) cultivarName:spName:'Betty_Jessel':M._campbellii_subsp._campbellii],     
#                b[(Intercept) cultivarName:spName:'Blood_Moon':M._sargentiana],                        
#                b[(Intercept) cultivarName:spName:'Blood_Moon'_on_M._sprengeri:M._sargentiana],        
#                b[(Intercept) cultivarName:spName:'Borde_Hill':M._campbellii],                         
#                b[(Intercept) cultivarName:spName:'Borde_Hill':M._campbellii_subsp._mollicomata],      
#                b[(Intercept) cultivarName:spName:'Chyverton_Dark':M._sargentiana],                    
#                b[(Intercept) cultivarName:spName:'Chyverton_Red':M._dawsoniana],                      
#                b[(Intercept) cultivarName:spName:'Chyverton_Red'__(ex._'Chyverton'):M._dawsoniana],   
#                b[(Intercept) cultivarName:spName:'Claret_Cup':M._sprengeri],                          
#                b[(Intercept) cultivarName:spName:'Clark':M._dawsoniana],                              
#                b[(Intercept) cultivarName:spName:'Clarke':M._dawsoniana],                             
#                b[(Intercept) cultivarName:spName:'Copeland_Court':M._sprengeri],                      
#                b[(Intercept) cultivarName:spName:'Diva':M._sprengeri],                                
#                b[(Intercept) cultivarName:spName:'Eric_Savill':M._sprengeri],                         
#                b[(Intercept) cultivarName:spName:'Ethel_Hillier':M._campbellii],                      
#                b[(Intercept) cultivarName:spName:'Ethel_Hillier':M._campbellii_(Alba_Group)],         
#                b[(Intercept) cultivarName:spName:'Forrest's_Pink':M._denudata],                       
#                b[(Intercept) cultivarName:spName:'Japanese_Clone':M._denudata],                       
#                b[(Intercept) cultivarName:spName:'Lanarth':M._campbellii],                            
#                b[(Intercept) cultivarName:spName:'Lanarth':M._campbellii_subsp._mollicomata],         
#                b[(Intercept) cultivarName:spName:'Landicla':M._campbellii],                           
#                b[(Intercept) cultivarName:spName:'Wada's_Japanese_Clone':M._denudata],                
#                b[(Intercept) cultivarName:spName:'Wakehurst':M._sprengeri],                           
#                b[(Intercept) cultivarName:spName:'Wakehurst'_A.M.:M._sprengeri],                      
#                b[(Intercept) cultivarName:spName:botanical_species:M._amoena],                        
#                b[(Intercept) cultivarName:spName:botanical_species:M._biondii],                       
#                b[(Intercept) cultivarName:spName:botanical_species:M._campbellii],                    
#                b[(Intercept) cultivarName:spName:botanical_species:M._campbellii_(deep_pink_form)],   
#                b[(Intercept) cultivarName:spName:botanical_species:M._campbellii_(F.C.C._form)],      
#                b[(Intercept) cultivarName:spName:botanical_species:M._campbellii_(hybrid)],           
#                b[(Intercept) cultivarName:spName:botanical_species:M._campbellii_subsp._mollicomata], 
#                b[(Intercept) cultivarName:spName:botanical_species:M._cavaleriei_var._platypetala],  
#                b[(Intercept) cultivarName:spName:botanical_species:M._chevalieri],                    
#                b[(Intercept) cultivarName:spName:botanical_species:M._conifera],                      
#                b[(Intercept) cultivarName:spName:botanical_species:M._cylindrica],                    
#                b[(Intercept) cultivarName:spName:botanical_species:M._dawsoniana],                    
#                b[(Intercept) cultivarName:spName:botanical_species:M._denudata],                      
#                b[(Intercept) cultivarName:spName:botanical_species:M._laevifolia],                    
#                b[(Intercept) cultivarName:spName:botanical_species:M._maudiae],                       
#                b[(Intercept) cultivarName:spName:botanical_species:M._maudiae_var._platypetala],      
#                b[(Intercept) cultivarName:spName:botanical_species:M._officinalis_biloba],            
#                b[(Intercept) cultivarName:spName:botanical_species:M._sapaensis],                     
#                b[(Intercept) cultivarName:spName:botanical_species:M._sargentiana],                   
#                b[(Intercept) cultivarName:spName:botanical_species:M._sargentiana_(ex._var._robusta)],
#                b[(Intercept) cultivarName:spName:botanical_species:M._sprengeri],                     
#                b[(Intercept) cultivarName:spName:botanical_species:M._stellata],                      
#                b[(Intercept) cultivarName:spName:botanical_species:M._zenii],                         
#                b[(Intercept) cultivarName:spName:botanical_species:M._zenii_(clone_no._1)],           
#                b[(Intercept) spName:M._amoena],                                                       
#                b[(Intercept) spName:M._biondii],                                                      
#                b[(Intercept) spName:M._campbellii],                                                   
#                b[(Intercept) spName:M._campbellii_(Alba_Group)],                                      
#                b[(Intercept) spName:M._campbellii_(deep_pink_form)],                                  
#                b[(Intercept) spName:M._campbellii_(F.C.C._form)],                                     
#                b[(Intercept) spName:M._campbellii_(hybrid)],                                          
#                b[(Intercept) spName:M._campbellii_subsp._campbellii],                                 
#                b[(Intercept) spName:M._campbellii_subsp._mollicomata],                                
#                b[(Intercept) spName:M._cavaleriei_var._platypetala],                                  
#                b[(Intercept) spName:M._chevalieri],                                                   
#                b[(Intercept) spName:M._conifera],                                                     
#                b[(Intercept) spName:M._cylindrica],                                                   
#                b[(Intercept) spName:M._dawsoniana],                                                   
#                b[(Intercept) spName:M._denudata],                                                     
#                b[(Intercept) spName:M._laevifolia],                                                   
#                b[(Intercept) spName:M._maudiae],                                                      
#                b[(Intercept) spName:M._maudiae_var._platypetala],                                     
#                b[(Intercept) spName:M._officinalis_biloba],                                           
#                b[(Intercept) spName:M._sapaensis],                                                    
#                b[(Intercept) spName:M._sargentiana],                                                 
#                b[(Intercept) spName:M._sargentiana_(ex._var._robusta)],                               
#                b[(Intercept) spName:M._sargentiana_(pale_flower_selection_ex.)],                      
#                b[(Intercept) spName:M._sprengeri],                                                    
#                b[(Intercept) spName:M._stellata],                                                     
#                b[(Intercept) spName:M._zenii],                                                        
#                b[(Intercept) spName:M._zenii_(clone_no._1)],                                          
#                sigma)


# Adding intercept to the parameters
budtable <- as.data.frame.table(modbud3.post) %>%
  group_by(parameters) %>%
  summarize(value = mean(Freq))

budtable$intercept.val <- NA
budtable$gdd.val <- NA

for(i in 1:nrow(budtable)){
  budtable$intercept.val[i] <- budtable$value[1]
  budtable$gdd.val[i] <- budtable$value[i] + budtable$intercept.val[i]
}

# anthesis
anthesistable <- as.data.frame.table(modanthesis3.post) %>%
  group_by(parameters) %>%
  summarize(value = mean(Freq))

anthesistable$intercept.val <- NA
anthesistable$gdd.val <- NA

for(i in 1:nrow(anthesistable)){
  anthesistable$intercept.val[i] <- anthesistable$value[1]
  anthesistable$gdd.val[i] <- anthesistable$value[i] + anthesistable$intercept.val[i]
}

# peak bloom
peaktable <- as.data.frame.table(modpeak3.post) %>%
  group_by(parameters) %>%
  summarize(value = mean(Freq))

peaktable$intercept.val <- NA
peaktable$gdd.val <- NA

for(i in 1:nrow(peaktable)){
  peaktable$intercept.val[i] <- peaktable$value[1]
  peaktable$gdd.val[i] <- peaktable$value[i] + peaktable$intercept.val[i]
}

# first tepal
ftepaltable <- as.data.frame.table(modftepal3.post) %>%
  group_by(parameters) %>%
  summarize(value = mean(Freq))

ftepaltable$intercept.val <- NA
ftepaltable$gdd.val <- NA

for(i in 1:nrow(ftepaltable)){
  ftepaltable$intercept.val[i] <- ftepaltable$value[1]
  ftepaltable$gdd.val[i] <- ftepaltable$value[i] + ftepaltable$intercept.val[i]
}

# last tepal
ltepaltable <- as.data.frame.table(modltepal3.post) %>%
  group_by(parameters) %>%
  summarize(value = mean(Freq))

ltepaltable$intercept.val <- NA
ltepaltable$gdd.val <- NA

for(i in 1:nrow(ltepaltable)){
  ltepaltable$intercept.val[i] <- ltepaltable$value[1]
  ltepaltable$gdd.val[i] <- ltepaltable$value[i] + ltepaltable$intercept.val[i]
}

budtable$gdd.val[which(budtable$parameters == "(Intercept)")] <- budtable$value[1]
anthesistable$gdd.val[which(anthesistable$parameters == "(Intercept)")] <- anthesistable$value[1]
peaktable$gdd.val[which(peaktable$parameters == "(Intercept)")] <- peaktable$value[1]
ftepaltable$gdd.val[which(ftepaltable$parameters == "(Intercept)")] <- ftepaltable$value[1]
ltepaltable$gdd.val[which(ltepaltable$parameters == "(Intercept)")] <- ltepaltable$value[1]

budtable <- budtable[1:78,]
anthesistable <- anthesistable[1:74,]
peaktable <- peaktable[1:72,]
ftepaltable <- ftepaltable[1:73,]
ltepaltable <- ltepaltable[1:74,]

budtable$uniquecolour <- NA
for(i in 1:nrow(budtable)){
  if(budtable$parameters[i] == "(Intercept)" | budtable$parameters[i] == "sigma"){
    budtable$uniquecolour[i] <- "highlight"
  }
  else{
    budtable$uniquecolour[i] <- "non"
  }
}

anthesistable$uniquecolour <- NA
for(i in 1:nrow(anthesistable)){
  if(anthesistable$parameters[i] == "(Intercept)" | anthesistable$parameters[i] == "sigma"){
    anthesistable$uniquecolour[i] <- "highlight"
  }
  else{
    anthesistable$uniquecolour[i] <- "non"
  }
}

peaktable$uniquecolour <- NA
for(i in 1:nrow(peaktable)){
  if(peaktable$parameters[i] == "(Intercept)" | peaktable$parameters[i] == "sigma"){
    peaktable$uniquecolour[i] <- "highlight"
  }
  else{
    peaktable$uniquecolour[i] <- "non"
  }
}

ftepaltable$uniquecolour <- NA
for(i in 1:nrow(ftepaltable)){
  if(ftepaltable$parameters[i] == "(Intercept)" | ftepaltable$parameters[i] == "sigma"){
    ftepaltable$uniquecolour[i] <- "highlight"
  }
  else{
    ftepaltable$uniquecolour[i] <- "non"
  }
}

ltepaltable$uniquecolour <- NA
for(i in 1:nrow(ltepaltable)){
  if(ltepaltable$parameters[i] == "(Intercept)" | ltepaltable$parameters[i] == "sigma"){
    ltepaltable$uniquecolour[i] <- "highlight"
  }
  else{
    ltepaltable$uniquecolour[i] <- "non"
  }
}

# Reversing order of table so that the plot comes out in the same order as the mcmc intervals
budtable <- rev(budtable)

# visualizing these new tables with the gdd  values
budtable %>%
  ggplot(aes(x = gdd.val,
             y = parameters,
             colour = uniquecolour)) +
  geom_point() + 
  theme_few() +
  theme(legend.position = "none") +
  scale_y_discrete(limits=rev)

anthesistable %>%
  ggplot(aes(x = gdd.val,
             y = parameters,
             colour = uniquecolour)) +
  geom_point() + 
  theme_few() +
  theme(legend.position = "none") +
  scale_y_discrete(limits=rev)

peaktable %>%
  ggplot(aes(x = gdd.val,
             y = parameters,
             colour = uniquecolour)) +
  geom_point() + 
  theme_few() +
  theme(legend.position = "none") +
  scale_y_discrete(limits=rev)

ftepaltable %>%
  ggplot(aes(x = gdd.val,
             y = parameters,
             colour = uniquecolour)) +
  geom_point() + 
  theme_few() +
  theme(legend.position = "none") +
  scale_y_discrete(limits=rev)

ltepaltable %>%
  ggplot(aes(x = gdd.val,
             y = parameters,
             colour = uniquecolour)) +
  geom_point() + 
  theme_few() +
  theme(legend.position = "none") +
  scale_y_discrete(limits=rev)

# Just a linear reg for gdd to see if it changes
gddmodeltable <- magd %>%
  group_by(year) %>%
  summarize(gdd.annual = max(gdd,na.rm = TRUE))

gddmodel <- lm(gdd.annual~year, gddmodeltable)
summary(gddmodel)
paste('y =', coef(gddmodel)[[2]], '* x', '+', coef(gddmodel)[[1]])

# Just a linear reg for temperature to see if it changes
tempmodeltable <- dtemp %>%
  group_by(year) %>%
  summarize(temp.annual = mean(avg_temperature, na.rm = TRUE))

tempmodel <- lm(temp.annual~year, tempmodeltable)
summary(tempmodel)
paste('y =', coef(tempmodel)[[2]], '* x', '+', coef(tempmodel)[[1]])

# Visualizing these lm
gddmodeltable %>%
  ggplot(aes(x = year,
             y = gdd.annual))+
  geom_point() +
  geom_smooth(method = "lm") +
  theme_clean() +
  labs(x = "Year",
       y = "Annual maximum GDD value") +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023))+
  ylim(240,600)

tempmodeltable %>%
  ggplot(aes(x = year,
             y = temp.annual))+
  geom_point() +
  geom_smooth(method = "lm") +
  theme_clean() +
  labs(x = "Year",
       y = "Mean annual temperature") +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023))

# GDD accumulation plot
dtemp %>%
  ggplot(aes(x = date,
             y = gddAccumulate)) +
  geom_col(fill = "#a3152b") +
  labs(x = "Day of year",
       y = "GDD accumulation") +
  theme_clean()
