
#####LOADING LIBRARIES#####

library(RMySQL)
library(DBI)
library(dplyr)
library(lubridate)
library(ggplot2)
library(lubridate)
library(anytime)
library(tidyverse)
library(arules)
library(base)
library(scales)
library(tidyr)
library(grid)
library(gridExtra)
library(forecast)
library(plotly)
library(readxl)
library(imputeTS)

##Create a database connection

con = dbConnect(
  MySQL(),
  user = 'deepAnalytics',
  password = 'Sqltask1234!',
  dbname = 'dataanalytics2018',
  host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

#tables of data base listed 

dbListTables(con)
dbListFields(con,"yr_2006")


#request relevant data base tables


yr_2006 <-
  dbGetQuery(con,
             "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
     Sub_metering_3, Global_active_power,Voltage,Global_intensity FROM yr_2006")

yr_2007 <-
  dbGetQuery(con,
             "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
   Sub_metering_3, Global_active_power,Voltage,Global_intensity FROM yr_2007")

yr_2008 <- 
  dbGetQuery(con,
             "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
     Sub_metering_3, Global_active_power,Voltage,Global_intensity FROM yr_2008")

yr_2009 <-
  dbGetQuery(con,
             "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
     Sub_metering_3, Global_active_power,Voltage,Global_intensity FROM yr_2009")

yr_2010 <-
  dbGetQuery(con,
             "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
     Sub_metering_3, Global_active_power,Voltage,Global_intensity FROM yr_2010")



#weather data for Paris
WeatherParis <- read.csv("WeatherParis.csv",sep=';')

#yr_2006 and yr_2010 excluded, no data for complete year

SmartSub <- bind_rows(yr_2007,yr_2008,yr_2009,yr_2010)

#Initial Exploration of Data Set

head(SmartSub)
str(SmartSub)
summary(SmartSub)

#set local time on english version

Sys.setlocale("LC_TIME", "C")

#combine Date and Time to DateTime

SmartSub <-cbind(SmartSub,paste(SmartSub$Date,SmartSub$Time), stringsAsFactors=FALSE)

head(SmartSub)
#rename new feature

colnames(SmartSub)[9] <-"DateTime"

#move column to first place in data frame

SmartSub <- SmartSub[,c(ncol(SmartSub), 1:(ncol(SmartSub)-1))]

#remove redundant "Date" and "Time" features

SmartSub <- SmartSub[,-c(2,3)]

#Convert DateTime into time format

SmartSub$DateTime <- as.POSIXct(SmartSub$DateTime, "%Y/%m/%d %H:%M:%S")

#Add time zone

attr(SmartSub$DateTime, "tzone") <- "Europe/Paris"

#create features 'year','quarter','month','week','weekday,'day','hour','minute'

SmartSub$year <- year(SmartSub$DateTime)

SmartSub$quarter <- quarter(SmartSub$DateTime)

SmartSub$month <- month(SmartSub$DateTime)

SmartSub$week <- week(SmartSub$DateTime)         

SmartSub$weekday <- weekdays(SmartSub$DateTime)

SmartSub$day <- day(SmartSub$DateTime)

SmartSub$hour <- hour(SmartSub$DateTime)

SmartSub$minute <- minute (SmartSub$DateTime)

#rename submeters 

SmartSub <- rename(SmartSub, submeter_kitchen = Sub_metering_1)

SmartSub <- rename(SmartSub, submeter_laundry = Sub_metering_2)

SmartSub <- rename(SmartSub, submeter_heat_AC = Sub_metering_3)

#new feature: measuring total power consumption

#convert kilo-watt into watt-minutes

SmartSub <- mutate(SmartSub, total_power = Global_active_power*1000 /60)

#implement new feature 'unknown' energy consumption(not represented in SmartSubmeters) into data set

SmartSub <-
  mutate(SmartSub,
         unknown = total_power - submeter_kitchen - submeter_laundry - submeter_heat_AC)

#Visualising 'Voltage' to check relevance of feature 

SmartSub1 <- SmartSub %>%
  group_by(DateTime = floor_date(DateTime, "1 hour")) %>%
  summarize(
    submeter_kitchen = sum(submeter_kitchen),
    submeter_laundry = sum(submeter_laundry),
    submeter_heat_AC = sum(submeter_heat_AC),
    unknown = sum(unknown),
    total_power = sum(total_power),
    Global_active_power =sum(Global_active_power),
    Voltage= mean(Voltage),
    Global_intensity=mean(Global_intensity)
  )

pl1_voltage <- ggplot(data = SmartSub1,aes(x = DateTime, y = Voltage)) +
  geom_line(color='blue',
            alpha = 0.6,
            size = 1) +
  labs(x = "3 years periode", 
       y = "Voltage") +
  ggtitle('VOLTAGE FREQUENCY') +
  theme_minimal()+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        text = element_text(size = 15))


pl2_voltage <-  pl1_voltage + scale_x_datetime(
  breaks = seq(
    as.POSIXct("2007-01-01 00:00:00"),
    as.POSIXct("2010-03-01 00:00:00"),
    "1 year"),
  labels = date_format("%Y"),
  expand = c(0, 0),
  limits = c(
    as.POSIXct("2007-01-01 00:00:00"),
    as.POSIXct("2010-03-01 00:00:00")
  )
)  

pl2_voltage

#Plotting Examples for energy consumption: 3 submeters combined 1 day


SmartSub1 <- SmartSub %>%
  group_by(DateTime = floor_date(DateTime, "15 minutes")) %>%
  summarize(
    submeter_kitchen = sum(submeter_kitchen),
    submeter_laundry = sum(submeter_laundry),
    submeter_heat_AC = sum(submeter_heat_AC),
    unknown = sum(unknown),
    total_power = sum(total_power),
    Global_active_power =sum(Global_active_power),
    Voltage= mean(Voltage),
    Global_intensity=mean(Global_intensity)
  )


SmartSub_mul <- SmartSub1 %>% 
  select (DateTime, submeter_heat_AC, submeter_kitchen, submeter_laundry, unknown) %>%
  gather(key = "variable", value = "value", -DateTime)


myplot <- ggplot(SmartSub_mul, aes(x = DateTime, y = value)) + 
  geom_line(aes(color = variable), alpha = 0.8,
            size = 1) +
  scale_color_manual(values = c( "red","blue", "orange","grey")) +
  labs(x = "2008-02-06 (Tuesday)", 
       y = "Energy in Watt-Hour") +
  ggtitle('Water Heater & AC - 1 DAY Power Consumption') +
  theme_minimal()+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        text = element_text(size = 15))+
  
  scale_x_datetime(
    breaks = seq(
      as.POSIXct("2008-05-06 00:00:00"),
      as.POSIXct("2008-05-07 00:00:00"),
      "2 hours"),
    labels = date_format("%H"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2008-05-06 00:00:00"),
      as.POSIXct("2008-05-07 00:00:00")
    )
  )  +
  scale_y_continuous(labels = comma)

myplot

#plotting 3 submeters combined  1 week


SmartSub1 <- SmartSub %>%
  group_by(DateTime = floor_date(DateTime, "1 hour")) %>%
  summarize(
    submeter_kitchen = sum(submeter_kitchen),
    submeter_laundry = sum(submeter_laundry),
    submeter_heat_AC = sum(submeter_heat_AC),
    unknown = sum(unknown),
    total_power = sum(total_power),
    Global_active_power =sum(Global_active_power),
    Voltage= mean(Voltage),
    Global_intensity=mean(Global_intensity)
  )


SmartSub_mul <- SmartSub1 %>% 
  select (DateTime, submeter_heat_AC, submeter_kitchen, submeter_laundry, unknown) %>%
  gather(key = "variable", value = "value", -DateTime)


myplot <- ggplot(SmartSub_mul, aes(x = DateTime, y = value)) + 
  geom_line(aes(color = variable), alpha = 0.8,
            size = 1) +
  scale_color_manual(values = c( "red","blue", "orange","grey")) +
  labs(x = "1st week February 2008", 
       y = "Energy in Watt-Hour") +
  ggtitle('Water Heater & AC - 1 day Power Consumption') +
  theme_minimal()+
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        text = element_text(size = 15))+
   scale_x_datetime(
    breaks = seq(
      as.POSIXct("2008-01-07 00:00:00"),
      as.POSIXct("2008-01-14 00:00:00"),
      "2 days"),
    labels = date_format("%A"),
    expand = c(0, 0),
    limits = c(
      as.POSIXct("2008-01-07 00:00:00"),
      as.POSIXct("2008-01-14 00:00:00")
    )
  )  +
  scale_y_continuous(labels = comma)

myplot

head(WeatherParis)
str(WeatherParis)

# preprocessing weather data for Paris

#Convert DateTime into time format

WeatherParis$DateTime <- as.POSIXct(SmartSub$DateTime, "%Y/%m/%d %H:%M:%S")

#Add time zone

attr(WeatherParis$DateTime, "tzone") <- "Europe/Paris"

#merging (left outer join) weather data with Data Frame for Submeters

SmartSub_T <- merge(SmartSub, WeatherParis, by = "DateTime",all.x = TRUE)

#Replacing missing values by interpolating temperature

SmartSub_T$temperature <- na.interpolation(SmartSub_T$temperature)

#function to include 2-tariff energy prices

SmartSub_T$energy_price <- 1

myfunction <- function (x) {
  
  if (x>=0 & x <=7)
  { return(0.0001244)
  }    
  else{
    return(0.0001593)
  }
}

SmartSub_T$energy_price <- sapply(SmartSub_T$hour,myfunction)

SmartSub1 <- SmartSub_T %>%
  group_by(DateTime = floor_date(DateTime, "15 minutes")) %>%
  summarize(
    submeter_kitchen = sum(submeter_kitchen),
    submeter_laundry = sum(submeter_laundry),
    submeter_heat_AC = sum(submeter_heat_AC),
    unknown = sum(unknown),
    total_power = sum(total_power),
    Global_active_power =sum(Global_active_power),
    Voltage= mean(Voltage),
    Global_intensity=sum(Global_intensity),
    energy_price = mean(energy_price)
    
  )

#creating column for total energy costs

SmartSub2 <- as.data.frame(SmartSub1)

SmartSub2 <- mutate(SmartSub2, energy_costs=energy_price * total_power)

#saving it in separate file
write.csv(SmartSub2, file="SmartSub21.csv")

#preprocess Data of year 2010 for Energy Mix analysis

SmartSub_10 <- yr_2010

SmartSub_10 <- cbind(SmartSub_10,paste(SmartSub_10$Date, SmartSub_10$Time),
        stringsAsFactors = FALSE)

#rename new feature

colnames(SmartSub_10)[9] <-"DateTime"

#move column to first place in data frame

SmartSub_10 <- SmartSub_10[,c(ncol(SmartSub_10), 1:(ncol(SmartSub_10)-1))]

#remove redundant "Date" and "Time" features

SmartSub_10 <- SmartSub_10[,-c(2,3)]

#Convert DateTime into time format

SmartSub_10$DateTime <- as.POSIXct(SmartSub_10$DateTime, "%Y/%m/%d %H:%M:%S")

#Add time zone

attr(SmartSub_10$DateTime, "tzone") <- "Europe/Paris"

#create features 'year','quarter','month','week','weekday,'day','hour','minute'

SmartSub_10$year <- year(SmartSub_10$DateTime)

SmartSub_10$quarter <- quarter(SmartSub_10$DateTime)

SmartSub_10$month <- month(SmartSub_10$DateTime)

SmartSub_10$week <- week(SmartSub_10$DateTime)         

SmartSub_10$weekday <- weekdays(SmartSub_10$DateTime)

SmartSub_10$day <- day(SmartSub_10$DateTime)

SmartSub_10$hour <- hour(SmartSub_10$DateTime)

SmartSub_10$minute <- minute (SmartSub_10$DateTime)

#rename submeters 

SmartSub_10 <- rename(SmartSub_10, submeter_kitchen = Sub_metering_1)
SmartSub_10 <- rename(SmartSub_10, submeter_laundry = Sub_metering_2)
SmartSub_10 <- rename(SmartSub_10, submeter_heat_AC = Sub_metering_3)

#convert kilo-watt into watt-minutes

SmartSub_10 <- mutate(SmartSub_10, total_power = Global_active_power*1000 /60)

SmartSub_10 <-
  mutate(SmartSub_10,
         unknown = total_power - submeter_kitchen - submeter_laundry - submeter_heat_AC)

#Energy Mix Data for France

EnergyMix <- read_csv("EnergyMix.csv")

EnergyMix <- as.data.frame(EnergyMix)

#change data format from character to numerical

EnergyMix[2:11] <- lapply(EnergyMix[2:11], as.numeric)

#transform DateTime into time format

EnergyMix$DateTime <- gsub("T"," ",EnergyMix$DateTime)

EnergyMix$DateTime <- as.POSIXct(strptime(EnergyMix$DateTime, "%Y-%m-%d %H:%M:%S"))

#join Submeter Data with Energy Mix

SmartSub_10 <- merge(SmartSub_10, EnergyMix, by = "DateTime")

SmartSub_10[18:27]  <- lapply(SmartSub_10[18:27],na.interpolation)

SmartSub_10 <- mutate(SmartSub_10,clean_index=`total consumption`/Co2)

#example for time series, random week in 2008

houseWeek <- filter(SmartSub_T, year == 2008 & week==4)

SmartSub1 <- houseWeek %>%
  group_by(DateTime = floor_date(DateTime, "1 hour")) %>%
  summarize(
    submeter_kitchen = sum(submeter_kitchen),
    submeter_laundry = sum(submeter_laundry),
    submeter_heat_AC = sum(submeter_heat_AC),
    unknown = sum(unknown),
    total_power = sum(total_power),
    Global_active_power =sum(Global_active_power),
    Voltage= mean(Voltage),
    Global_intensity=mean(Global_intensity),
    temperature=mean(temperature)
  )

#transform data set into time series

submeter_AC_weekly_TS <-
  ts(SmartSub1$submeter_kitchen,
     frequency = 24)

monthplot(submeter_AC_weekly_TS)

#initial exploration of time series

class(submeter_AC_weekly_TS)

start(submeter_AC_weekly_TS)

end(submeter_AC_weekly_TS)

frequency(submeter_AC_weekly_TS)

summary(submeter_AC_weekly_TS)

#apply time series linear regression

fitSM3 <- tslm(submeter_AC_weekly_TS ~ trend + season) 

summary(fitSM3)

#making and plotting forecast

forecastfitSM3 <- forecast(fitSM3, h=10)

plot(forecastfitSM3,ylab= "Watt-Hours", xlab="Time")

#decomposing time series into trend, seasonal, random parts

components070809SM3weekly <- decompose(submeter_AC_weekly_TS)

plot(components070809SM3weekly)

summary(components070809SM3weekly)
