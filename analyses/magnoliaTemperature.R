# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Checking weather data consistency
# 
# Justin Ngo
# Updated 6 June 2024
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
library(tidyverse)
library(ggthemes)
library(rstanarm)

# tsawassen.avg <- read.table("data/GHCN_TSAWASSEN_Tavg.txt")%>%
#   mutate(avg_temperature.tsawassen = V4)
# tsawassen.max <- read.table("data/GHCN_TSAWASSEN_Tmax.txt")%>%
#   mutate(max_temperature.tsawassen = V4)
# tsawassen.min <- read.table("data/GHCN_TSAWASSEN_Tmin.txt")%>%
#   mutate(min_temperature.tsawassen = V4)
# 
# tsawassen.avg$V1 <- paste(tsawassen.avg$V1, tsawassen.avg$V2, tsawassen.avg$V3, sep = "/")
# tsawassen.avg <- tsawassen.avg %>%
#   mutate(date = ymd(V1)) %>%
#   mutate(DOY = yday(date))
# 
# tsawassen.max$V1 <- paste(tsawassen.max$V1, tsawassen.max$V2, tsawassen.max$V3, sep = "/")
# tsawassen.max <- tsawassen.max %>%
#   mutate(date = ymd(V1)) %>%
#   mutate(DOY = yday(date))
# 
# tsawassen.min$V1 <- paste(tsawassen.min$V1, tsawassen.min$V2, tsawassen.min$V3, sep = "/")
# tsawassen.min <- tsawassen.min %>%
#   mutate(date = ymd(V1)) %>%
#   mutate(DOY = yday(date))
# 
# tsawassen.avg <- select(tsawassen.avg,5,6,7)
# tsawassen.min <- select(tsawassen.min,5,6,7)
# tsawassen.max <- select(tsawassen.max,5,6,7)
# 
# tavg.airport <- airport %>%
#   select(1,3) %>%
#   mutate(source = "envcanada")
# merged.data <- merge(tavg.airport,tsawassen.avg) 
# # It seems that Tsawwassen is warmer than the airport, and it's also farther away from the garden so maybe not the best comparison? But it's one of the only ones with a record that's about as long as the airport

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Comparing the GHCN and environment canada datasets directly as a plot
airport <- read_csv("analyses/input/vanDailyWeather.csv") %>%
  select(1,2,4,5)
airport.ghcn <- read.table("data/GHCN_AIRPORT_Tavg.txt")

airport.ghcn$V1 <- paste(airport.ghcn$V1, airport.ghcn$V2, airport.ghcn$V3, sep = "/")
airport.ghcn <- airport.ghcn %>%
  mutate(avg_temperature = V4) %>%
  mutate(date = ymd(V1)) %>%
  mutate(source = "ghcn")
airport.ghcn.sel <- airport.ghcn %>%
  select(5:7)

min.ghcn <- read.table("data/GHCN_AIRPORT_Tmin.txt")
min.ghcn$V1 <- paste(min.ghcn$V1, min.ghcn$V2, min.ghcn$V3, sep = "/")
min.ghcn <- min.ghcn %>%
  mutate(min_temperature = V4) %>%
  mutate(date = ymd(V1)) %>%
  mutate(source = "ghcn")
min.ghcn.sel <- min.ghcn %>%
  select(5:7)

max.ghcn <- read.table("data/GHCN_AIRPORT_Tmax.txt")
max.ghcn$V1 <- paste(max.ghcn$V1, max.ghcn$V2, max.ghcn$V3, sep = "/")
max.ghcn <- max.ghcn %>%
  mutate(max_temperature = V4) %>%
  mutate(date = ymd(V1)) %>%
  mutate(source = "ghcn")
max.ghcn.sel <- max.ghcn %>%
  select(5:7)

min.envcan <- airport %>%
  select(date, min_temperature)%>%
  mutate(source = "envcanada") %>%
  mutate(min.temp.envcan = min_temperature)
max.envcan <- airport %>%
  select(date, max_temperature)%>%
  mutate(source = "envcanada") %>%
  mutate(max.temp.envcan = max_temperature)

max.ghcn.fin <- max.ghcn.sel %>%
  mutate(max.temp.ghcn = max_temperature) %>%
  select(2,4)
min.ghcn.fin <- min.ghcn.sel %>%
  mutate(min.temp.ghcn = min_temperature) %>%
  select(2,4)

min.envcan.fin <- min.envcan %>%
  select(1,4)
max.envcan.fin <- max.envcan %>%
  select(1,4)


range <- interval(as.POSIXct("1991-01-31"),
                  as.POSIXct("2023-05-31"))

tavg.airport <- airport %>%
  select(1,3) %>%
  mutate(source = "envcanada")

tavg.airport <- tavg.airport  %>%
  filter(date %within% range)
airport.ghcn.sel <- airport.ghcn.sel %>%
  filter(date %within% range)
min.ghcn.sel <- min.ghcn.sel %>%
  filter(date %within% range)
max.ghcn.sel <- max.ghcn.sel %>%
  filter(date %within% range)
min.ghcn.fin <- min.ghcn.fin %>%
  filter(date %within% range)
max.ghcn.fin <- max.ghcn.fin %>%
  filter(date %within% range)
min.envcan.fin <- min.envcan.fin %>%
  filter(date %within% range)
max.envcan.fin <- max.envcan.fin %>%
  filter(date %within% range)

# airport.bind <- rbind(airport.ghcn.sel,tavg.airport)
# 
# min.bind <- rbind(min.envcan,min.ghcn.sel)
# max.bind <- rbind(max.envcan,max.ghcn.sel)
# 
# min.merge <- merge(min.envcan.fin,min.ghcn.fin)
# max.merge <- merge(max.envcan.fin,max.ghcn.fin)
# 
# airport.bind %>%
#   ggplot(aes(x = date,
#              y = avg_temperature,
#              colour = source))+
#   geom_line(linewidth = 1)+
#   labs(x = "Date",
#        y = "Average temperature (degC)")+
#   theme_clean()
# 
# min.merge %>%
#   ggplot(aes(x = min.temp.envcan,
#              y = min.temp.ghcn))+
#   geom_point()+
#   geom_abline(slope=1,intercept=0,colour = "#a3152b",linewidth=1)+
#   labs(x = "Minimum temperature from EnvCanada (degC)",
#        y = "Minimum temperature from GHCN (degC)")+
#   theme_clean()
# 
# max.merge %>%
#   ggplot(aes(x = max.temp.envcan,
#              y = max.temp.ghcn))+
#   geom_point()+
#   geom_abline(slope=1,intercept=0,colour = "#a3152b",linewidth=1)+
#   labs(x = "Maximum temperature from EnvCanada (degC)",
#        y = "Maximum temperature from GHCN (degC)")+
#   theme_clean()
# 
# # <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# 
# # <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# # Comparing the GHCN and environment canada datasets by difference
# 
# airport.ghcn.2 <- airport.ghcn.sel %>%
#   mutate(avg_temperature.ghcn = avg_temperature) %>%
#   select(2,4)
# tavg.airport.2 <- tavg.airport %>%
#   mutate(avg_temperature.airport = avg_temperature) %>%
#   select(1,4)
# 
# airport.merge <- merge(airport.ghcn.2,tavg.airport.2)
# airport.merge <- mutate(airport.merge, difference = avg_temperature.ghcn - avg_temperature.airport)
# 
# airport.merge %>%
#   ggplot(aes(x = date,
#              y = difference))+
#   geom_line(linewidth=1,colour="#a3152b")+
#   labs(x = "Date",
#        y = "Difference in Tavg value between GHCN and EnvCanada")+
#   theme_clean()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# ## some f(x)s to help calculate GDD
# ## this f(x) makes a column which zeroes out all data
# # below your threshold temperature
# 
# makethreshold.data <- function(dater, temp.col, thresh){ 
#   ifelse(dater[[temp.col]]>thresh,
#          (dater[[temp.col]]-thresh), 0)
# }
# 
# ## this f(x) adds up gdd
# ## requires data ordered by doy (I do this in the loop below)
# ## this f(x) returns the value while treating NA as zeroes
# # needstartdate is when you require data to start that year, otherwise it returns NA
# # for example, 5 means you need data that starts before 5 January to actually count
# makegdd.data.skipNA <- function(dater, gdd.col, doy.col, startdate, needstartdate){
#   saveme <- c()
#   for(i in 1:nrow(dater)){
#     # deal with cases where the data start after Jan 1
#     if (dater[[doy.col]][1]>needstartdate) saveme[i] <- NA
#     else
#       # deal with cases where the entire column is NA
#       if (sum(is.na(dater[[gdd.col]]))==length(dater[[gdd.col]])) saveme[i] <- NA
#       else
#         # okay, finally calculate the GDD
#         if (dater[[doy.col]][i]==startdate) saveme[i] <- (dater[[gdd.col]][i])
#         else
#           # if a cell is NA, just add 0 instead of the cell
#           if (is.na(dater[[gdd.col]][i])) saveme[i] <- (0+saveme[i-1])
#           else
#             saveme[i] <- (dater[[gdd.col]][i]+saveme[i-1])
#   }
#   return(saveme)
# }
# 
# 
# countNA.pergdd <- function(dater, gdd.col, doy.col, startdate, needstartdate){
#   saveme <- c()
#   for(i in 1:nrow(dater)){
#     dater <- dater[order(as.numeric(dater[[doy.col]])),]
#     if (dater[[doy.col]][1]>needstartdate) saveme[i] <- NA
#     else 
#       if  (sum(is.na(dater[[gdd.col]]))==length(dater[[gdd.col]])) saveme[i] <- NA
#       else
#         saveme[i] <- sum(is.na(dater[[gdd.col]][which(dater[[doy.col]]<(i+1))]))
#   }
#   return(saveme)
# }
# 
# ##
# ## f(x)s that I never got to run
# ## I tried to count NA and make cumulative GDD in one f(x)
# ##

# makegdd.data.skipNA.arghh <- function(dater, gdd.col, doy.col, startdate, needstartdate){
#   saveme <- c()
#   nacount <- c()
#   for(i in 1:nrow(dater)){
#     if (dater[[doy.col]][1]>needstartdate) saveme[i] <- NA
#     else 
#       if (is.na(unique(dater[[gdd.col]][i]))==TRUE) saveme[i] <- NA
#       else
#         
#         # subsetme <- dater[which(dater[[doy.col]]<(i+1)),]
#         # nacount[i] <- sum(is.na(subsetme[[gdd.col]]))
#         
#         if (is.na(dater[[gdd.col]][i])==TRUE) saveme[i] <- (0+saveme[i-1])
#         else
#           if (dater[[doy.col]][i]==startdate) saveme[i] <- (dater[[gdd.col]][i])
#           else
#             saveme[i] <- (dater[[gdd.col]][i]+saveme[i-1])
#           nacount[i] <- (sum(is.na(subsetme[[gdd.col]][i]))+nacount[i-1])
#   }
#   return(cbind(saveme, nacount))
# }

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
vandat <- read_csv("analyses/input/vanDailyWeather.csv")

range <- interval(as.POSIXct("1991-01-31"),
                  as.POSIXct("2023-05-31"))

vandat_febmay <- vandat %>% #only months Feb to May are needed
  mutate(date2 = ymd(date)) %>%
  filter(date2 >= make_date(year((date2)), 2, 1),
         date2 <= make_date(year((date2)), 5, 31)) %>%
  filter(date2 %within% range) %>%
  mutate(DOY = yday(date2))

vandat_sel <- vandat_febmay %>%
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

# Is spring weather just not changing across the years?
# Making four plots to show the temperature in yearly quarters
vandat_month <- vandat %>%
  mutate(month = month(date))%>%
  mutate(year = year(date))%>%
  select(1:5,73,74)
szn.winter <- vandat_month %>%
  filter(month == "12" | month == "1" | month == "2") %>%
  group_by(year)%>%
  summarize_all(mean)%>%
  mutate(season = "winter")
szn.spring <- vandat_month %>%
  filter(month == "3" | month == "4" | month == "5") %>%
  group_by(year)%>%
  summarize_all(mean)%>%
  mutate(season = "spring")
szn.summer <- vandat_month %>%
  filter(month == "6" | month == "7" | month == "8") %>%
  group_by(year)%>%
  summarize_all(mean)%>%
  mutate(season = "summer")
szn.autumn <- vandat_month %>%
  filter(month == "9" | month == "10" | month == "11") %>%
  group_by(year)%>%
  summarize_all(mean)%>%
  mutate(season = "autumn")

seasons <- rbind(szn.winter,szn.spring,szn.summer,szn.autumn)
seasons %>%
  ggplot(aes(x = year,
             y = avg_temperature,
             colour = season))+
  geom_point() +
  geom_smooth(method = "lm") +
  theme_clean() +
  labs(x = "Year",
       y = "Mean Temperature (degC)") +
  ylim(0,25)

# winter temp trend
wintermodel <- lm(avg_temperature~year, szn.winter)
summary(wintermodel)
paste('y =', coef(wintermodel)[[2]], '* x', '+', coef(wintermodel)[[1]])

# spring temp trend
springmodel <- lm(avg_temperature~year, szn.spring)
summary(springmodel)
paste('y =', coef(springmodel)[[2]], '* x', '+', coef(springmodel)[[1]])

# summer temp trend
summermodel <- lm(avg_temperature~year, szn.summer)
summary(summermodel)
paste('y =', coef(summermodel)[[2]], '* x', '+', coef(summermodel)[[1]])

# autumn temp trend
autumnmodel <- lm(avg_temperature~year, szn.autumn)
summary(autumnmodel)
paste('y =', coef(autumnmodel)[[2]], '* x', '+', coef(autumnmodel)[[1]])

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Seasonal lm() for min and max temperatures

# winter temp trend
summary(lm(min_temperature~year, szn.winter))
summary(lm(max_temperature~year, szn.winter))

# spring temp trend
summary(lm(min_temperature~year, szn.spring))
summary(lm(max_temperature~year, szn.spring))

# summer temp trend
summary(lm(min_temperature~year, szn.summer))
summary(lm(max_temperature~year, szn.summer))

# autumn temp trend
summary(lm(min_temperature~year, szn.autumn))
summary(lm(max_temperature~year, szn.autumn))

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Since there's a gap in the GHCN data, let's just stick with the airport. I don't think that the garden has their own rigorous and continuous set of temperature data, and the totem field ones are also disjointed and/or discontinued
# To make GDD we just need the average, which we already have
tavg.envcan <- tavg.airport %>%
  filter(source == "envcanada")

# setting our date range as February to June
range <- interval(as.POSIXct("1991-01-31"),
                  as.POSIXct("2023-05-31"))
tavg.envcan <- tavg.envcan %>%
  filter(date >= make_date(year((date)), 1, 1),
         date <= make_date(year((date)), 5, 31)) %>%
  filter(date %within% range)

# The GDD formula is like this: GDD = (Tmax + Tmin) / 2 – T where T is T_base
# We already have (Tmax + Tmin)/2, so I just need to make a new column where I set Tbase and then subtract it from the average data

# Set T_base as 5 degC?
d <- tavg.envcan %>%
  mutate(Tbase = 5) %>%
  mutate(GDD = avg_temperature - Tbase)

# If anything is below zero, set it as 0?
d$GDD <- sub("\\-.*","0",d$GDD)
d$GDD <- as.numeric(d$GDD)

# isolating out years and months so we can group_by
d <- d %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date))

# Ordering the data by date so it ascends
d$avg_temperature <- as.numeric(d$avg_temperature)
d <- d %>%
  mutate(rowID = row_number())
d <- d %>%
  arrange(desc(rowID))

# cumulative sum
d <- d %>%
  group_by(year) %>%
  mutate(gddAccumulate = cumsum(GDD))

# Adding doy
d <- d %>%
  mutate(doy = yday(date))

d %>%
  ggplot(aes(x = date,
             y = gddAccumulate)) +
  geom_point(colour = "#a3152b") +
  labs(x = "Day of year",
       y = "GDD accumulation") +
  theme_clean()
  
