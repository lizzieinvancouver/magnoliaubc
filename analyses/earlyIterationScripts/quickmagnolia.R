## Started 18 Jan 2024 ##
## By Lizzie ##
## Looking at some Magnolia data ##

## From UBC botanical garden ## 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# setwd("~/Documents/git/projects/misc/miscmisc/magnoliaubc/analyses")

## packages
library(ggplot2)
library(gridExtra)
library(viridis)

d  <- read.csv("input/Magnolia Phenology Study to 2013.csv")
d$firstbudcolordate <- as.Date()
d$firstbudcolordate <- as.Date()
d$firstbudcolordate <- as.Date(d$First.bud.colour, format="%d-%b-%Y")
d$firstbudcolordoy <- format(d$firstbudcolordate,"%j")
d$firstbudcoloryr <- format(d$firstbudcolordate,"%Y")
# Ugh ... I don't think the above is good, as we'd have to do every column with a date, we should try..
# MELTING (or reshaping from wide to long) so we get all the date columns in one columns and have...
# another column for 'event'

plot(d, aes(x=firstbudcoloryr, y=firstbudcolordoy))+
	geom_line(color=Name) # oy -- not working! Need to clean up the name column using grep

