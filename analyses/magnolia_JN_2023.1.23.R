# ## Started 18 Jan 2024 ##
# ## By Lizzie ##
# ## Looking at some Magnolia data ##
# 
# ## From UBC botanical garden ## 
# 
# ## housekeeping
# rm(list=ls()) 
# options(stringsAsFactors = FALSE)
# 
# # setwd("~/Documents/git/projects/misc/miscmisc/magnoliaubc/analyses")
# 
# # install.packages("viridis")
# 
# ## packages
# library(ggplot2)
# library(gridExtra)
# library(viridis)
# 
# d  <- read.csv("analyses/input/Magnolia Phenology Study to 2013.csv")
# # d$firstbudcolordate <- as.Date()
# # d$firstbudcolordate <- as.Date()
# d$firstbudcolordate <- as.Date(d$First.bud.colour, format="%d-%b-%Y")
# d$firstbudcolordoy <- format(d$firstbudcolordate,"%j")
# d$firstbudcoloryr <- format(d$firstbudcolordate,"%Y")
# # Ugh ... I don't think the above is good, as we'd have to do every column with a date, we should try..
# # MELTING (or reshaping from wide to long) so we get all the date columns in one columns and have...
# # another column for 'event'
# 
# plot(d, aes(x=firstbudcoloryr, y=firstbudcolordoy))+
# 	geom_line(color=Name) # oy -- not working! Need to clean up the name column using grep




# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# 23/1/2023
# JUSTIN's  efforts

rm(list = ls())

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

budplot <- magbind %>%
  ggplot(aes(x = year,
             y = bud.colour,
             colour = Name)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "First colour of flower buds (DOY)")+
  theme_clean() +
  theme(legend.position = "none")
budplot
# ummm....it looks crazy, but there seem to be trends

# trying other blooming stages
anthesisplot <- magbind %>%
  ggplot(aes(x = year,
             y = anthesis,
             colour = Name)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "First flower fully open (DOY)")+
  theme_clean() +
  theme(legend.position = "none")
anthesisplot

# peak bloom
peakplot <- magbind %>%
  ggplot(aes(x = year,
             y = peak.bloom,
             colour = Name)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Peak bloom (DOY)")+
  theme_clean() +
  theme(legend.position = "none")
peakplot

# first tepal dropping
tepalplot <- magbind %>%
  ggplot(aes(x = year,
             y = tepal.drop.first,
             colour = Name)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "First tepal dehiscence (DOY)")+
  theme_clean() +
  theme(legend.position = "none")
tepalplot

# last tepal dropping
witherplot <- magbind %>%
  ggplot(aes(x = year,
             y = tepal.drop.last,
             colour = Name)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Last tepal dehiscence (DOY)")+
  theme_clean() +
  theme(legend.position = "none")
witherplot

# would it be useful to look at mean temperature from March-June in the years and see if they match the events? Just a thought that Adriana might like

# Can we do average DOY of phenological event to see if there's a strong pattern? Or divide it by species so its more digestible at least
# here I set any species with more than 3 cultivars to be split up into their own section
unique(magbind$Name)

campbellii <- magbind %>%
  filter(grepl("campbellii",Name))

dawsoniana <- magbind %>%
  filter(grepl("dawsoniana",Name))

sargentiana <- magbind %>%
  filter(grepl("sargentiana",Name))

sprengeri <- magbind %>%
  filter(grepl("sprengeri",Name))

misc <- magbind %>%
  filter(!grepl("campbellii",Name))%>%
  filter(!grepl("dawsoniana",Name))%>%
  filter(!grepl("sargentiana",Name))%>%
  filter(!grepl("sprengeri",Name))

# making averages for each phenological event
campbellavg <- campbellii %>%
  group_by(year) %>%
  summarize(bud.colour.avg = mean(bud.colour),
            anthesis.avg = mean(anthesis),
            peak.avg = mean(peak.bloom),
            tepalfirst.avg = mean(tepal.drop.first),
            tepallast.avg = mean(tepal.drop.last))
campbell.long <- campbellavg %>%
  reshape2::melt(id.vars = "year",
       var = "event")%>%
  mutate(species = "M. campbellii")

dawsonavg <- dawsoniana %>%
  group_by(year) %>%
  summarize(bud.colour.avg = mean(bud.colour),
            anthesis.avg = mean(anthesis),
            peak.avg = mean(peak.bloom),
            tepalfirst.avg = mean(tepal.drop.first),
            tepallast.avg = mean(tepal.drop.last))
dawson.long <- campbellavg %>%
  reshape2::melt(id.vars = "year",
                 var = "event") %>%
  mutate(species = "M. dawsoniana")

sargentavg <- sargentiana %>%
  group_by(year) %>%
  summarize(bud.colour.avg = mean(bud.colour),
            anthesis.avg = mean(anthesis),
            peak.avg = mean(peak.bloom),
            tepalfirst.avg = mean(tepal.drop.first),
            tepallast.avg = mean(tepal.drop.last))
sargent.long <- sargentavg %>%
  reshape2::melt(id.vars = "year",
                 var = "event") %>%
  mutate(species = "M. sargentiana")

sprengeravg <- sprengeri %>%
  group_by(year) %>%
  summarize(bud.colour.avg = mean(bud.colour),
            anthesis.avg = mean(anthesis),
            peak.avg = mean(peak.bloom),
            tepalfirst.avg = mean(tepal.drop.first),
            tepallast.avg = mean(tepal.drop.last))
sprenger.long <- sprengeravg %>%
  reshape2::melt(id.vars = "year",
                 var = "event") %>%
  mutate(species = "M. sprengeri")

miscselect <- misc %>%
  select(c(5,4,13,14,15,16,17))
misc.long <- miscselect %>%
  reshape2::melt(id.vars = c("year","Name"),
                 var = "event") %>%
  mutate(species = Name)%>%
  select(!Name) %>%
  drop_na(year)

# mag_group <- rbind(campbell.long,
#                    dawson.long,
#                    sargent.long,
#                    sprenger.long,
#                    misc.long)
# 
# mag_plot <- mag_group %>%
#   ggplot(aes(x = year,
#              y = value,
#              colour = event)) +
#   geom_line(linewidth = 1) +
#   labs(x = "Year",
#        y = "Day of phenological event occurring") +
#   theme_clean()
# mag_plot
# For now it's so confusing. Let me just look at the four grouped species first

mag_group2 <- rbind(campbell.long,
                   dawson.long,
                   sargent.long,
                   sprenger.long)
mag_group2 <- mag_group2 %>%
  drop_na(year)

# Plotting the four grouped species
mag_plot2 <- mag_group2 %>%
  ggplot(aes(x = year,
             y = value,
             colour = event)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Day of phenological event occurring") +
  theme_clean() +
  facet_wrap(species ~ ., ncol = 2) +
  scale_x_continuous(labels = c(1991:2009), breaks = c(1991:2009)) +
  scale_color_hue(name = "Phenological event",
                  labels = c("First bud colour",
                             "Anthesis",
                             "Peak bloom",
                             "First tepal drop",
                             "Last tepal drop")) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))
mag_plot2

# Now I can tackle the miscellaneous species
misc_plot <- misc.long %>%
  ggplot(aes(x = year,
             y = value,
             colour = event)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Day of phenological event occurring") +
  theme_clean() +
  facet_wrap(species ~ ., ncol = 3) +
  scale_x_continuous(labels = c(1991:2009), breaks = c(1991:2009)) +
  scale_color_hue(name = "Phenological event",
                  labels = c("First bud colour",
                                "Anthesis",
                                "Peak bloom",
                                "First tepal drop",
                                "Last tepal drop")) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))
misc_plot



