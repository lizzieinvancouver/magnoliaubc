# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#     Started by Justin
#     9 April 2024
# 
#     Magnolia phenology cleaner analysis
# 
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
rm(list = ls())
library(tidyverse)
library(reshape2)
library(lubridate)
library(ggthemes)
library(viridis)

# Starting with Adriana's new data from 2010 to 2023
# 2020 and 2024 not included
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

# bind_rows() indicates different data classes between each dataset, so make long format first and make all of the dates character class first.

# # I tried doing this with lapply but I'm so confused so I'll just do each one manually
# dlist <- list(d2010,d2011,d2012,d2013,d2014,d2015,d2016,d2017,d2018,d2019,d2021,d2022,d2023)
# clist <- c("d2010","d2011","d2012","d2013","d2014","d2015","d2016","d2017","d2018","d2019","d2021","d2022","d2023")
# 
# colnames <- c("Refno","Location","Name","FBD","FFFO","PB","FTD","LTD","FGS","AccessNo","Stat2024","Comments")
# dlist2 <- lapply(dlist, function(x){x<-setNames(x, colnames)})
# dlist3 <- lapply(dlist2, function(x) x %>% mutate_all(as.character))
# dlist4 <- lapply(dlist3, function(x){melt(dlist3[[x]], id.vars=c("Refno","Location","Name","AccessNo","Status2024","Comments"),var = "event")})

# as.character()
convertcols <- c("First bud colour",
                 "First flower fully open",
                 "Peak bloom",
                 "First tepal drop",
                 "Last tepal drop",
                 "First green seen")
d2010[convertcols] <- lapply(d2010[convertcols], as.character)
d2011[convertcols] <- lapply(d2011[convertcols], as.character)
d2012[convertcols] <- lapply(d2012[convertcols], as.character)
d2013[convertcols] <- lapply(d2013[convertcols], as.character)
d2014[convertcols] <- lapply(d2014[convertcols], as.character)
d2015[convertcols] <- lapply(d2015[convertcols], as.character)
d2016[convertcols] <- lapply(d2016[convertcols], as.character)
d2017[convertcols] <- lapply(d2017[convertcols], as.character)
d2018[convertcols] <- lapply(d2018[convertcols], as.character)
d2019[convertcols] <- lapply(d2019[convertcols], as.character)
d2021[convertcols] <- lapply(d2021[convertcols], as.character)
d2022[convertcols] <- lapply(d2022[convertcols], as.character)
d2023[convertcols] <- lapply(d2023[convertcols], as.character)

# melting
d2010 <- melt(d2010, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
              var = "event")
d2011 <- melt(d2011, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
              var = "event")
d2012 <- melt(d2012, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
              var = "event")
d2013 <- melt(d2013, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
              var = "event")
d2014 <- melt(d2014, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
              var = "event")
d2015 <- melt(d2015, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
              var = "event")
d2016 <- melt(d2016, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
              var = "event")
d2017 <- melt(d2017, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
              var = "event")
d2018 <- melt(d2018, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
              var = "event")
d2019 <- melt(d2019, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
              var = "event")
d2021 <- melt(d2021, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
              var = "event")
d2022 <- melt(d2022, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
              var = "event")
d2023 <- melt(d2023, id.vars=c("Ref. No.","Location","Name","Accession Number","Status in 2024","Comments"),
              var = "event")

dlong <- bind_rows(d2010, d2011, d2012, d2013, d2014,
               d2015, d2016, d2017, d2018, d2019,
               d2021, d2022, d2023)
dlong %>% filter(value == "XX")
dlong$value[which(dlong$value == "XX")] <- "NA"

dlong$value <- as.Date(dlong$value, format = "%Y-%m-%d")
ddate <- dlong %>%
  mutate(DOY = yday(value)) %>%
  filter(!is.na(value)) %>%
  select(-5,-6) %>%
  mutate(year = year(value))

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Applying the same treatment to the earlier Magnolia data from before
# OBJECTS:
#     ddate: 2010 to 2023 magnolia data cleaned

dinit <- read_csv("analyses/input/Magnolia Phenology Study to 2013.csv")

dinit2 <- dinit %>%
  select(-1, -c(12:16))

# Copeland court has weird stuff that needs to be sorted out separately
dinit3 <- dinit2 %>%
  filter(Name != "M. sprengeri 'Copeland Court'")
copeland <- dinit2 %>%
  filter(Name == "M. sprengeri 'Copeland Court'")
dinit3 <- dinit3 %>%
  mutate("First green seen" = NA)
dinit3[convertcols] <- lapply(dinit3[convertcols], as.character)

# Making long
dinitlong <- dinit3 %>%
  melt(id.vars=c("Ref. No.","Location","Name","Accession Number","year"),
   var = "event")

# Formatting to date
dinitdate <- dinitlong %>%
  mutate(value = dmy(value))
dinitdate$year <- year(dinitdate$value)

# Adding DOY and clearing NAs
dinitdate <- dinitdate %>%
  mutate(DOY = yday(value)) %>%
  filter(!is.na(value))

# Working on Copeland court now
copeland <- copeland %>%
  mutate("First green seen" = NA)
copeland[convertcols] <- lapply(copeland[convertcols], as.character)

# Adds years to the dates
copeland$"First bud colour" <- paste(copeland$year, copeland$"First bud colour", sep = "/")
copeland$"First flower fully open" <- paste(copeland$year, copeland$"First flower fully open", sep = "/")
copeland$"Peak bloom" <- paste(copeland$year, copeland$"Peak bloom", sep = "/")
copeland$"First tepal drop" <- paste(copeland$year, copeland$"First tepal drop", sep = "/")
copeland$"Last tepal drop" <- paste(copeland$year, copeland$"Last tepal drop", sep = "/")

# Making long
copelandlong <- copeland %>%
  melt(id.vars=c("Ref. No.","Location","Name","year","Accession Number"),
       var = "event")  

# Formatting to date
copelandlong <- copelandlong %>%
  filter(!grepl("/NA",value))
copelandlong$value <- as.Date(copelandlong$value, format = "%Y/%d/%m")

# Adding DOY and clearing NAs
copelanddate <- copelandlong %>%
  mutate(DOY = yday(value)) %>%
  filter(!is.na(value))

dinitbind <- bind_rows(dinitdate,copelanddate)

allbind <- bind_rows(ddate,dinitbind)

# Adriana says the plant in A2 which was once Magnolia sprengeri 'Copeland Court' is now called M. sprengeri 'Diva' with accession number 1982-0954.03
allbind$Name[which(allbind$Name == "M. sprengeri 'Copeland Court'" & allbind$Location == "A2")] <- "M. sprengeri 'Diva'"
# Seems like it didn't really affect anything actually...

# We also need to combine names that are the same, i.e. M. sprengeri = Magnolia sprengeri
unique(allbind$Name)
allbind$Name <- gsub("Magnolia", "M.", allbind$Name)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Time for some simple visualization

# diva <- allbind %>%
#   filter(grepl("1982-0954.03",`Accession Number`))
# diva2 <- allbind %>%
#   filter(grepl("Copeland",Name))

# Magnolia sprengeri and cultivars <><><><><><><><><><><><><><><><><><><><><><><
sprengeri <- allbind %>%
  filter(grepl("sprengeri",Name))

sprengeri_cult <- sprengeri %>%
  filter(grepl("'",Name))

sprengeri_spec <- sprengeri %>%
  filter(!grepl("'",Name))

# # Averaging by species?
# # Saving the stuff here in markdown for later
# sprengeravg <- sprengeri %>%
#   group_by(year) %>%
#   summarize(bud.colour.avg = mean("First bud colour"),
#             anthesis.avg = mean("First flower fully open"),
#             peak.avg = mean("Peak bloom"),
#             tepalfirst.avg = mean("First tepal drop"),
#             tepallast.avg = mean("Last tepal drop"))
# 
# sprenger.long <- sprengeravg %>%
#   reshape2::melt(id.vars = "year",
#                  var = "event") %>%
#   mutate(species = "M. sprengeri")  %>%
#   drop_na(year)

sprenger_spec_label <- c("22"="M. sprengeri")

sprengeri_spec %>% ggplot(aes(x = year,
                             y = DOY,
                             colour = event)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Day of occurrence") +
  theme_clean() +
  facet_wrap(`Ref. No.` ~ ., labeller = as_labeller(sprenger_spec_label)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

sprenger_cult_label <- c("2"="M. sprengeri 'Diva'",
                         "3"="M. sargentiana 'Blood Moon' on M. sprengeri",
                         "22"="M. sprengeri",
                         "34"="M. sprengeri 'Claret Cup'",
                         "53"="M. sprengeri 'Wakehurst'",
                         "78"="M. sprengeri 'Eric Savill'")

sprengeri_cult %>% ggplot(aes(x = year,
                              y = DOY,
                              colour = event)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Day of occurrence") +
  theme_clean() +
  facet_grid(`Ref. No.` ~ .,labeller = as_labeller(sprenger_cult_label))+
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Magnolia campbellii and cultivars <><><><><><><><><><><><><><><><><><><><><><>
campbellii <- allbind %>%
  filter(grepl("campbellii",Name))

campbellii_cult <- campbellii %>%
  filter(grepl("'",Name))

campbellii_spec <-campbellii %>%
  filter(!grepl("'",Name))

# All M. campbellii
campbellii %>% ggplot(aes(x = year,
                             y = DOY,
                             colour = event)) +
  geom_line(linewidth = 1) +
  geom_point()+
  labs(x = "Year",
       y = "Day of occurrence") +
  theme_clean() +
  facet_grid(`Ref. No.` ~ .) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

campbell_spec_label <- c("27"="M. campbellii",
                      "28"="M. campbellii",
                      "44"="M. campbellii",
                      "50"="M. campbellii",
                      "67"="M. campbellii",
                      "77"="M. campbellii")

# All campbellii species individuals
campbellii_spec %>% ggplot(aes(x = year,
                          y = DOY,
                          colour = event)) +
  geom_line(linewidth = 1) +
  geom_point()+
  labs(x = "Year",
       y = "Day of occurrence") +
  theme_clean() +
  facet_grid(`Ref. No.` ~ .,labeller = as_labeller(campbell_spec_label)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

campbell_cult_label <- c("6"="M. campbellii 'Borde Hill'",
                    "7"="M. campbellii 'Lanarth'",
                    "29"="M. campbellii 'Ethel Hillier'",
                    "58"="M. campbellii 'Landicla'",
                    "80"="M. campbellii 'Betty Jessel'")

# All campbellii cultivar individuals
campbellii_cult %>% ggplot(aes(x = year,
                          y = DOY,
                          colour = event)) +
  geom_line(linewidth = 1) +
  geom_point()+
  labs(x = "Year",
       y = "Day of occurrence") +
  theme_clean() +
  facet_grid(`Ref. No.` ~ .,labeller = as_labeller(campbell_cult_label)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Magnolia dawsoniana and cultivars <><><><><><><><><><><><><><><><><><><><><><>
dawsoniana <- allbind %>%
  filter(grepl("dawsoniana",Name))

dawsoniana_cult <- dawsoniana %>%
  filter(grepl("'",Name))

dawsoniana_spec <- dawsoniana %>%
  filter(!grepl("'",Name))

# All M. campbellii
dawsoniana %>% ggplot(aes(x = year,
                          y = DOY,
                          colour = event)) +
  geom_line(linewidth = 1) +
  geom_point()+
  labs(x = "Year",
       y = "Day of occurrence") +
  theme_clean() +
  facet_grid(`Ref. No.` ~ .) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

dawson_spec_label <- c("12"="M. dawsoniana")

# All dawsoniana species individuals
dawsoniana_spec %>% ggplot(aes(x = year,
                               y = DOY,
                               colour = event)) +
  geom_line(linewidth = 1) +
  geom_point()+
  labs(x = "Year",
       y = "Day of occurrence") +
  theme_clean() +
  facet_grid(`Ref. No.` ~ .,labeller = as_labeller(dawson_spec_label)) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

dawson_cult_label <- c("16"="M. dawsoniana 'Chyverton Red'",
                  "54"="M. dawsoniana 'Barbara Cook'",
                  "63"="M. dawsoniana 'Clarke'")

# All dawsoniana cultivar individuals
dawsoniana_cult %>% ggplot(aes(x = year,
                               y = DOY,
                               colour = event)) +
  geom_line(linewidth = 1) +
  geom_point()+
  labs(x = "Year",
       y = "Day of occurrence") +
  theme_clean() +
  facet_grid(`Ref. No.` ~ .,labeller = as_labeller(dawson_cult_label )) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Magnolia misc species and cultivars <><><><><><><><><><><><><><><><><><><><><>
misc <- allbind %>%
  filter(!grepl("campbellii",Name)) %>%
  filter(!grepl("sprengeri",Name)) %>%
  filter(!grepl("dawsoniana",Name))

misc_cult <- misc %>%
  filter(grepl("'",Name))

misc_spec <- misc %>%
  filter(!grepl("'",Name))

misc %>% ggplot(aes(x = year,
                         y = DOY,
                         colour = event)) +
  geom_line(linewidth = 1) +
  geom_point()+
  labs(x = "Year",
       y = "Day of occurrence") +
  theme_clean() +
  facet_grid(`Ref. No.` ~ .) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

misc_spec %>% ggplot(aes(x = year,
                               y = DOY,
                               colour = event)) +
  geom_line(linewidth = 1) +
  geom_point()+
  labs(x = "Year",
       y = "Day of occurrence") +
  theme_clean() +
  facet_grid(`Ref. No.` ~ .) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))

# ,labeller = as_labeller(dawson_spec_label)

misc_cult %>% ggplot(aes(x = year,
                         y = DOY,
                         colour = event)) +
  geom_line(linewidth = 1) +
  geom_point()+
  labs(x = "Year",
       y = "Day of occurrence") +
  theme_clean() +
  facet_grid(`Ref. No.` ~ .) +
  theme(strip.text.y.right = element_text(angle = 0)) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))


allbind %>% 
  filter(`Ref. No.` == "39")


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Temperature data <><><><><><><><><><><><><><><><><><><><><><>
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