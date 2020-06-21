#Uber data analysis
install.packages("ggthemes")
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(maps)

# create vectors for color to be shown in plots
color<-c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

# read data files

setwd("/Users/akankshamishra/Documents/Uber Dataset/Uber-dataset")
apr_data<-read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

#formatting the data of Date.Time column to create factors of time objects like day, month, year etc.

data_2014<-rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)
data_2014
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

data_2014$Date.Time<-ymd_hms(data_2014$Date.Time)
data_2014$day<-factor(day(data_2014$Date.Time))
data_2014$month<-factor(month(data_2014$Date.Time,label=TRUE))
data_2014$year<-factor(year(data_2014$Date.Time))
data_2014$dayofweek<-factor(wday(data_2014$Date.Time,label=TRUE))
#data_2014$dayofweek
data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))


hour_data<-data_2014 %>% group_by(hour) %>% dplyr::summarise(Total=n())
datatable(hour_data)
#hour_data
#table(data_2014)
data_2014


# use ggplotvto plot the number of trips that the passengers had made in a day.

ggplot(hour_data,aes(hour,Total)) +
  geom_bar(stat="identity",fill="steelblue",color="red") +
  ggtitle("Trips every hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels=comma)

month_hour<-data_2014 %>% group_by(month,hour) %>% dplyr::summarise(Total=n())
month_hour

month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)


####
#Trips during every day of the month

day_group<-data_2014%>% group_by(day)%>%dplyr::summarise(Total=n())
datatable(day_group)

ggplot(day_group,aes(day,Total)) +
  geom_bar(stat = "identity",fill="steelblue")+
  ggtitle("Trips every day")+
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


day_month_group <- data_2014 %>% group_by(month, day) %>% dplyr::summarize(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#CC0000", "#006600", "#669999", "#00CCCC", 
                                "#660099", "#CC0066", "#FF9999", "#FF9900", 
                                "black", "black", "black", "black", "black"))


month_group <- data_2014 %>% group_by(month) %>% dplyr::summarize(Total = n()) 
datatable(month_group)


ggplot( month_group, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())
pdf("dayofweek")
ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#CC0000", "#006600", "#669999", "#00CCCC", 
                               "#660099", "#CC0066", "#FF9999", "#FF9900", 
                               "black", "black", "black", "black", "black"))



dev.off()


#plot the number of trips that have been taken by the passengers from each of the bases. 
#There are five bases in all out of which, we observe that B02617 had the highest number of trips

ggplot(data_2014,aes(Base)) + 
  geom_bar(fill="darkblue")+
  scale_y_continuous(labels=comma)+
  ggtitle("Trips by Bases")


#Plot trips by base and month
#Thursday observed highest trips in the three bases – B02598, B02617, B02682.

ggplot(data_2014,aes(Base,fill=month)) +
  geom_bar(position="dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month")



ggplot(data_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek")



## Heat Map by Hour and Day

day_and_hour<-data_2014 %>% group_by(day,hour)%>%dplyr::summarise(Total=n())
datatable(day_and_hour)

ggplot(day_and_hour,aes(day,hour,fill=Total)) + 
  geom_tile(color="white") + 
  ggtitle("Heat map by Hour and Day")

ggplot(day_month_group,aes(day,month,fill=Total))+
  geom_tile(color="white") +
  ggtitle("Heat Map by Month and Day")

ggplot(month_weekday,aes(dayofweek,month,fill=Total)) +
  geom_tile(color="yellow") +
  ggtitle("Heat Map by Day of week and Month")


month_base<-data_2014 %>% group_by(Base,month) %>%
  dplyr::summarise(Total=n())
dayofweek_base<-data_2014%>%group_by(Base,dayofweek)%>%dplyr::summarise(Total=n())

ggplot(month_base,aes(Base,month,fill=Total)) +
  geom_tile(color="white") +
  ggtitle("Heat Map by Month and Bases")

#Creating a map visualization of rides in New York

min_lat<-40.5774
max_lat<-40.9176
min_long<-74.15
max_long<-73.7004


ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")


ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  theme_void()+
  theme(legend.position = "none")+
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")


geom_map(data=data_2014,map=US,aes(x=Lon,y=Lat,map_id=region,group = group),
         fill = "white", color = "black", size = 0.25) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  theme_map()

ggplot(data_2014, aes(Lon, Lat, group = ))+
  geom_polygon(aes(fill = Assault), color = "white")+
  scale_fill_viridis_c(option = "C")















