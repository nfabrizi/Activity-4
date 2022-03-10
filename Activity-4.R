### Tutorial 4 ###

#install.packages(c("dplyr","lubridate", "ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A")
metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")
sensorLog <- read.csv("/cloud/project/activity04/Sensor log.csv",
                      na.strings = "#N/A")

# Cleaning Data

weather$dateF <- mdy_hm(weather$Date)
weather$doy <- yday(weather$dateF)
weather$year <- year(weather$dateF)
ggplot(data=weather[weather$doy > 121 & weather$doy < 274 ,],
       aes(x=dateF,
           y=Precip))+
  geom_col(color="royalblue4")+
  theme_classic()

weather$precip.QC <- ifelse(weather$doy >= 121 & weather$doy <= 188 & weather$year == 2021, 
                            NA, weather$Precip)

# Creating flags

weather$FreezeFlag <- ifelse(weather$AirTemp <= 0,
                             1, 0)

# Creating functions to check data

weather$dateF[1] %--% weather$dateF[2]
int_length(weather$dateF[1] %--% weather$dateF[2])

intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
interval_times <- int_length(intervals)
intervals[interval_times != 900]

timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}

timeCheck900(weather$dateF)



### Activity 4 ###

average <- function(x){
  x.no = na.omit(x)
  sum(x.no)/length(x.no)
}

average(weather$AirTemp)


# Prompt 1

ggplot(data=weather[weather$doy >= 121 & weather$doy <= 151 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_line()

ggplot(data=weather[weather$doy >= 182 & weather$doy <= 212 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_line()



# Prompt 2

weather$dateF[1] %--% weather$dateF[2]
int_length(weather$dateF[1] %--% weather$dateF[2])
intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]

interval_times <- int_length(intervals)
intervals[interval_times != 900]





