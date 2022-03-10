### Reading data ###
weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A")
metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")
sensorLog <- read.csv("/cloud/project/activity04/Sensor log.csv",
                      na.strings = "#N/A")
#install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(lubridate)
weather$dateF <- mdy_hm(weather$Date)
weather$doy <- yday(weather$dateF)
weather$year <- year(weather$dateF)
weather$precip.QC <- ifelse(weather$doy >= 121 & weather$doy <= 188 & weather$year == 2021, 
                            NA,
                            weather$Precip) 



### Question 1 ###

weather$Total.QC <- ifelse(weather$XLevel > 2 | 
                          weather$XLevel < -2 | 
                          weather$AirTemp < 0 | 
                          weather$YLevel > 2 | 
                          weather$YLevel < -2 ,
                          NA, weather$Precip)

length(weather$Total.QC[is.na(weather$Total.QC)])



### Question 2 ###

weather$BatteryFlag <- ifelse(weather$BatVolt < 8500,
                             1,0)

### Question 3 ###

unreal <- function(x){
  mean_x = mean(x , na.rm = TRUE)
  sd_x = sd(x, na.rm = TRUE)
  top <- (mean_x + sd_x*2)
  bottom <- (mean_x - sd_x*2)
  length(x[ x > top | x < bottom])
}
  
unreal(weather$SolRad)

unreal(weather$AirTemp)

### Question 4 ###

Weather2021 <- weather[weather$year == 2021, ]

ggplot(data=Weather2021[Weather2021$doy > 1 & Weather2021$doy < 90 ,],
       aes(x=dateF,
           y=AirTemp))+
  geom_col(color="royalblue4")+
  theme_classic()

ggplot(data=Weather2021[Weather2021$doy > 335 & Weather2021$doy < 365 ,],
       aes(x=dateF,
           y=AirTemp))+
  geom_col(color="tomato2")+
  theme_classic()


