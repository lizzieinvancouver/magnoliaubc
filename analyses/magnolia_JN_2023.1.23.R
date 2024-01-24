## Started 18 Jan 2024 ##
## By Lizzie ##
## Looking at some Magnolia data ##

## From UBC botanical garden ## 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# setwd("~/Documents/git/projects/misc/miscmisc/magnoliaubc/analyses")

# install.packages("viridis")

## packages
library(ggplot2)
library(gridExtra)
library(viridis)

d  <- read.csv("analyses/input/Magnolia Phenology Study to 2013.csv")
# d$firstbudcolordate <- as.Date()
# d$firstbudcolordate <- as.Date()
d$firstbudcolordate <- as.Date(d$First.bud.colour, format="%d-%b-%Y")
d$firstbudcolordoy <- format(d$firstbudcolordate,"%j")
d$firstbudcoloryr <- format(d$firstbudcolordate,"%Y")
# Ugh ... I don't think the above is good, as we'd have to do every column with a date, we should try..
# MELTING (or reshaping from wide to long) so we get all the date columns in one columns and have...
# another column for 'event'

plot(d, aes(x=firstbudcoloryr, y=firstbudcolordoy))+
	geom_line(color=Name) # oy -- not working! Need to clean up the name column using grep




# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# 23/1/2023
# JUSTIN's  efforts

# trying to turn all the columns into one column for easier DOY conversion
library(tidyverse)
# install.packages("reshape")
library(reshape)
library(lubridate)
library(ggthemes)


magnolia <- read.csv("analyses/input/Magnolia Phenology Study to 2013.csv")
magselect <- magnolia %>%
  select(-X.1,-X.2,-X.3,-X.4,-X.5) %>%
  as.data.frame() #I just tossed this in here since it seemed to fix some date issues

# M. sprengeri 'Copeland Court' has wack date values so I'm going to filter it out and then change them separately before adding it back into the main dataframe since I'm not sure how to modify only one range of rows

magfilter <- magselect %>%
  filter(Name != "M. sprengeri 'Copeland Court'")
copeland <- magselect %>%
  filter(Name == "M. sprengeri 'Copeland Court'")

# working on the other species/cultivars first
maglong <- melt(magfilter, id.vars=c("X","Ref..No.","Location","Name","year","Accession.Number"),
                      var = "event")
magdate <- maglong %>%
  mutate(value=dmy(value))
magdate$value <- as.Date(magdate$value)
magdate$year <- year(magdate$value)
# magdoy <- magdate %>%
#   mutate(doy = yday(value)) 
# for some reason, applying yday() here to convert to doy messes up the format when doing pivot_wider

magwide <- magdate %>%
  group_by(event)%>%
  mutate(row=row_number()) %>%
  pivot_wider(names_from = event, values_from = value) %>%
  drop_na(Ref..No.)

# try doy conversion here? less elegant but avoids the format issue
magw_doy <- magwide %>%
  mutate(bud.colour = yday(First.bud.colour),
         anthesis = yday(First.flower.fully.open),
         peak.bloom = yday(Peak.bloom),
         tepal.drop.first = yday(First.tepal.drop),
         tepal.drop.last = yday(Last.tepal.drop))


# now adding in copeland court
copeland$First.bud.colour <- paste(copeland$year, copeland$First.bud.colour, sep = "/")
copeland$First.flower.fully.open <- paste(copeland$year, copeland$First.flower.fully.open, sep = "/")
copeland$Peak.bloom <- paste(copeland$year, copeland$Peak.bloom, sep = "/")
copeland$First.tepal.drop <- paste(copeland$year, copeland$First.tepal.drop, sep = "/")
copeland$Last.tepal.drop <- paste(copeland$year, copeland$Last.tepal.drop, sep = "/")

copelandlong <- copeland %>%
  melt(id.vars=c("X","Ref..No.","Location","Name","year","Accession.Number"),
                              var = "event")
copelanddate <- copelandlong %>%
  mutate(value=ydm(value))
# Copeland Court already has years entered in

copelandwide <- copelanddate %>%
  group_by(event)%>%
  mutate(row=row_number()) %>%
  pivot_wider(names_from = event, values_from = value) %>%
  drop_na(Ref..No.)

copelandw_doy <- copelandwide %>%
  mutate(bud.colour = yday(First.bud.colour),
         anthesis = yday(First.flower.fully.open),
         peak.bloom = yday(Peak.bloom),
         tepal.drop.first = yday(First.tepal.drop),
         tepal.drop.last = yday(Last.tepal.drop))

# Combining all the cultivars together now
as.data.frame(magw_doy)
as.data.frame(copelandw_doy)
magbind <- bind_rows(magw_doy,copelandw_doy)

bloomplot <- magbind %>%
  ggplot(aes(x = bud.colour,
             y = year,
             colour = Name)) +
  geom_line(linewidth = 1) +
  theme_clean() +
  theme(legend.position = "none")
bloomplot
# ummm....it looks crazy


