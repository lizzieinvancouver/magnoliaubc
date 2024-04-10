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

# 16 February 2024
# JUSTIN's  efforts

rm(list = ls())

# trying to turn all the columns into one column for easier DOY conversion
library(tidyverse)
library(reshape2)
library(lubridate)
library(ggthemes)
library(viridis)


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
  mutate(species = "M. campbellii") %>%
  drop_na(year)

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
  mutate(species = "M. dawsoniana")  %>%
  drop_na(year)

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
  mutate(species = "M. sargentiana")  %>%
  drop_na(year)

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
  mutate(species = "M. sprengeri")  %>%
  drop_na(year)

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

# Aligning with historic climate data?
# climdat <- read_csv("analyses/input/ubcClimateDaily_1991_1995.csv") #UBC totem field station only has up to 1995?
# 
# climdat_filter <- climdat %>% #only months Feb to May are needed
#   mutate(date = ymd(LOCAL_DATE)) %>%
#   filter(date >= make_date(year((date)), 2, 1),
#          date <= make_date(year((date)), 5, 31))

# Trying it out with 12000 rows of climate data from vancouver.weatherstats at Vancouver International Airport
vandat <- read_csv("analyses/input/vanDailyWeather.csv")

range <- interval(as.POSIXct("1991-01-31"),
                  as.POSIXct("2023-05-31"))

vandat_filt <- vandat %>% #only months Feb to May are needed
  mutate(date2 = ymd(date)) %>%
  filter(date2 >= make_date(year((date2)), 2, 1),
         date2 <= make_date(year((date2)), 5, 31)) %>%
  filter(date2 %within% range) %>%
  mutate(DOY = yday(date2))

vandat_sel <- vandat_filt %>%
  select(date2, DOY, max_temperature, avg_temperature, min_temperature, heatdegdays, growdegdays_5, growdegdays_7, growdegdays_10) %>%
  mutate(year = year(date2),
         month = month(date2))
# I wanted to keep sunrise sunset data but it's not complete

# Make mean average spring temperature according to the historic min and max phenology DOYs for each species?
# Luckily for me, DOY range for the four grouped cultivars (campbellii, etc.) are always within ~60 to ~140 to let's filter for DOY 59 to 141
vandat_maggroup_year <- vandat_sel %>%
  filter(DOY %in% (59:141)) %>%
  group_by(year) %>%
  summarise_all(mean) %>%
  select(-date2, -DOY, -month)
# Now we have average temperature between Feb and May for every year

# Worthwhile to do monthly temperature? Then we can overlay annual temperature patterns over each year to see if the phenological event lines up with the temeperature of the month it occurs in?
vandat_maggroup_month <- vandat_sel %>%
  filter(DOY %in% (59:141)) %>%
  group_by(year, month) %>%
  summarise_all(mean) %>%
  select(-date2, -DOY)

# Just to look; spring temps across the year
yeartemp_plot <- vandat_maggroup_year %>%
  ggplot(aes(x = year,
             y = avg_temperature))+
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Average temperature (degC)") +
  theme_clean()
yeartemp_plot

# monthly variation per year
monthtemp_plot <- vandat_maggroup_month %>%
  ggplot(aes(x = month,
             y = avg_temperature))+
  geom_line(linewidth = 1) +
  labs(x = "Month",
       y = "Average temperature (degC)") +
  theme_clean() +
  facet_wrap(year ~.)
monthtemp_plot

# Adding mean spring temperature to the magnolia plots on a second axis?
# Can add the yearly temp data to each of the species' data frames as a new column
vandat_sargent <- vandat_maggroup_year %>%
  filter(year %in% (1991:2009))

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Making floating bars to represent min and max temperatures in spring for each year
# vandat_maggroup_year is too long of a name so let me just shorten it
tempdat <- vandat_maggroup_year %>%
  select(1:4) %>%
  tibble::rownames_to_column(var = "var")

tempplot <- tempdat %>%
  ggplot(aes(x = as.factor(year),
             ymin = `min_temperature`,
             ymax = `max_temperature`,
             lower = `min_temperature`,
             upper = `max_temperature`,
             middle = `avg_temperature`))+
  geom_boxplot(stat = "identity",
               width = 0.5)+ 
  labs(x = "Year",
       y = "Spring Temperature (degC)")+
  theme_clean() +
  theme(axis.text.x = element_text(angle = 270))+
  ylim(0,20)
tempplot

sargplot <- sargent.long %>%
  ggplot(aes(x = year,
             y = value,
             colour = event))+
  geom_line(linewidth=1)+
  labs(x = "Year",
       y = "DOY of phenological event") +
  theme_clean()+
  scale_color_viridis()+
  scale_x_continuous(labels = c(1991:2009), breaks = c(1991:2009)) +
  scale_color_hue(name = "Phenological event",
                  labels = c("First bud colour",
                             "Anthesis",
                             "Peak bloom",
                             "First tepal drop",
                             "Last tepal drop")) +
  theme(axis.text.x = element_text(angle = 270),
        strip.text.x = element_text(face = "italic"))
sargplot  
  
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
## some f(x)s to help calculate GDD
## this f(x) makes a column which zeroes out all data
# below your threshold temperature

makethreshold.data <- function(dater, temp.col, thresh){ 
  ifelse(dater[[temp.col]]>thresh,
         (dater[[temp.col]]-thresh), 0)
}

## this f(x) adds up gdd
## requires data ordered by doy (I do this in the loop below)
## this f(x) returns the value while treating NA as zeroes
# needstartdate is when you require data to start that year, otherwise it returns NA
# for example, 5 means you need data that starts before 5 January to actually count
makegdd.data.skipNA <- function(dater, gdd.col, doy.col, startdate, needstartdate){
  saveme <- c()
  for(i in 1:nrow(dater)){
    # deal with cases where the data start after Jan 1
    if (dater[[doy.col]][1]>needstartdate) saveme[i] <- NA
    else
      # deal with cases where the entire column is NA
      if (sum(is.na(dater[[gdd.col]]))==length(dater[[gdd.col]])) saveme[i] <- NA
      else
        # okay, finally calculate the GDD
        if (dater[[doy.col]][i]==startdate) saveme[i] <- (dater[[gdd.col]][i])
        else
          # if a cell is NA, just add 0 instead of the cell
          if (is.na(dater[[gdd.col]][i])) saveme[i] <- (0+saveme[i-1])
          else
            saveme[i] <- (dater[[gdd.col]][i]+saveme[i-1])
  }
  return(saveme)
}


countNA.pergdd <- function(dater, gdd.col, doy.col, startdate, needstartdate){
  saveme <- c()
  for(i in 1:nrow(dater)){
    dater <- dater[order(as.numeric(dater[[doy.col]])),]
    if (dater[[doy.col]][1]>needstartdate) saveme[i] <- NA
    else 
      if  (sum(is.na(dater[[gdd.col]]))==length(dater[[gdd.col]])) saveme[i] <- NA
      else
        saveme[i] <- sum(is.na(dater[[gdd.col]][which(dater[[doy.col]]<(i+1))]))
  }
  return(saveme)
}



##
## f(x)s that I never got to run
## I tried to count NA and make cumulative GDD in one f(x)
##

makegdd.data.skipNA.arghh <- function(dater, gdd.col, doy.col, startdate, needstartdate){
  saveme <- c()
  nacount <- c()
  for(i in 1:nrow(dater)){
    if (dater[[doy.col]][1]>needstartdate) saveme[i] <- NA
    else 
      if (is.na(unique(dater[[gdd.col]][i]))==TRUE) saveme[i] <- NA
      else
        
        # subsetme <- dater[which(dater[[doy.col]]<(i+1)),]
        # nacount[i] <- sum(is.na(subsetme[[gdd.col]]))
        
        if (is.na(dater[[gdd.col]][i])==TRUE) saveme[i] <- (0+saveme[i-1])
        else
          if (dater[[doy.col]][i]==startdate) saveme[i] <- (dater[[gdd.col]][i])
          else
            saveme[i] <- (dater[[gdd.col]][i]+saveme[i-1])
          nacount[i] <- (sum(is.na(subsetme[[gdd.col]][i]))+nacount[i-1])
  }
  return(cbind(saveme, nacount))
}
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# 9 April 2024 now adding the new data from Adriana

d2010 <- read_csv("analyses/input/data2010.csv")
d2011 <- read_csv("analyses/input/data2011.csv")
d2012 <- read_csv("analyses/input/data2012.csv")
d2013 <- read_csv("analyses/input/data2013.csv")
d2014 <- read_csv("analyses/input/data2014.csv")
d2015 <- read_csv("analyses/input/data2015.csv")
d2016 <- read_csv("analyses/input/data2016.csv")
d2017 <- read_csv("analyses/input/data2017.csv")
d2018 <- read_csv("analyses/input/data2018.csv")
d2019 <- read_csv("analyses/input/data2019.csv")
# d2020 <- read_csv("analyses/input/data2020.csv")
d2021 <- read_csv("analyses/input/data2021.csv")
d2022 <- read_csv("analyses/input/data2022.csv")
d2023 <- read_csv("analyses/input/data2023.csv")

dto23 <- rbind(d2010, d2011, d2012, d2013, d2014,
               d2015, d2016, d2017, d2018, d2019,
               d2021, d2022, d2023)
dlong <- melt(dto23, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
                var = "event")
dlong$value <- as.Date(dlong$value, format = "%Y-%m-%d")
ddate <- dlong %>%
  mutate(DOY = yday(value)) %>%
  filter(!is.na(value)) %>%
  select(-5,-6)


