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
copelandlong$value <- as.Date(copelandlong$value, format = "%Y/%m/%d")

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

sprengericult <- allbind %>%
  filter(grepl("sprengeri",Name))
sprengeri <- allbind %>%
  filter(Name == "M. sprengeri")


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

sprengericult %>% ggplot(aes(x = year,
                         y = DOY,
                         colour = event)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Day of occurrence") +
  theme_clean() +
  facet_wrap(Name ~ .)

sprengeri %>% ggplot(aes(x = year,
                             y = DOY,
                             colour = event)) +
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Day of occurrence") +
  theme_clean() +
  facet_wrap(Name ~ .) +
  scale_x_continuous(labels = c(1991:2023), breaks = c(1991:2023)) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(face = "italic"))
