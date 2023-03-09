#Installing the packages
install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')

#Loading the packages
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(readr)

#Uploading data sets
file_path <- "/Users/charlottewang/Desktop/Cyclistic/"
Jan <- read_csv(paste0(file_path,"202201-divvy-tripdata.csv"))
Feb <- read_csv(paste0(file_path,"202202-divvy-tripdata.csv"))
Mar <- read_csv(paste0(file_path,"202203-divvy-tripdata.csv"))
Apr <- read_csv(paste0(file_path,"202204-divvy-tripdata.csv"))
May <- read_csv(paste0(file_path,"202205-divvy-tripdata.csv"))
Jun <- read_csv(paste0(file_path,"202206-divvy-tripdata.csv"))
Jul <- read_csv(paste0(file_path,"202207-divvy-tripdata.csv"))
Aug <- read_csv(paste0(file_path,"202208-divvy-tripdata.csv"))
Sep <- read_csv(paste0(file_path,"202209-divvy-tripdata.csv"))
Oct <- read_csv(paste0(file_path,"202210-divvy-tripdata.csv"))
Nov <- read_csv(paste0(file_path,"202211-divvy-tripdata.csv"))
Dec <- read_csv(paste0(file_path,"202212-divvy-tripdata.csv"))

#Check columnns and data types
str(Jan)
str(Feb)
str(Mar)
str(Apr)
str(May)
str(Jun)
str(Jul)
str(Aug)
str(Sep)
str(Oct)
str(Nov)
str(Dec)

#Merge all datasets into one
all_data <- bind_rows(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
summary(all_data)

#Add day of the week
all_data$day_of_week <- wday(all_data$started_at, label = T, abbr = T)

#Add month
all_data$month <- format(as.Date(all_data$started_at), '%m')

#Add starting hour 
all_data$starting_hour <- format(as.POSIXct(all_data$started_at), '%H')

#Add length of trip
all_data$trip_length <- difftime(all_data$ended_at, all_data$started_at, units = "secs")
all_data$trip_length <- as.numeric(as.character(all_data$trip_length))

#Remove data with bike taken out of docks and data with negative trip length 
cleaned_all_data <- all_data[!(all_data$start_station_name == "HQ QR" | all_data$trip_length <= 0),]

cleaned_all_data$started_at <- as.character(cleaned_all_data$started_at)
cleaned_all_data$ended_at <- as.character(cleaned_all_data$ended_at)

cleaned_all_data <- cleaned_all_data[!apply(is.na(cleaned_all_data) | cleaned_all_data == "", 1, all),]

#cleaned_all_data$started_at <- as.POSIXct(cleaned_all_data$started_at, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
#cleaned_all_data$ended_at <- as.Date(cleaned_all_data$ended_at, format("%Y/%m/%d %H:%M:%S"))



#Analysis on trip length

summary(cleaned_all_data$trip_length)
summary(cleaned_all_data)

# Compare members and casual users
aggregate(cleaned_all_data$trip_length ~ cleaned_all_data$member_casual, FUN = mean)
aggregate(cleaned_all_data$trip_length ~ cleaned_all_data$member_casual, FUN = median)
aggregate(cleaned_all_data$trip_length ~ cleaned_all_data$member_casual, FUN = max)
aggregate(cleaned_all_data$trip_length ~ cleaned_all_data$member_casual, FUN = min)

# Average ride time by each day for members vs casual users
aggregate(cleaned_all_data$trip_length ~ cleaned_all_data$member_casual + cleaned_all_data$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
cleaned_all_data %>% 
  group_by(member_casual, day_of_week) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(trip_length)) %>% 		# calculates the average duration
  arrange(member_casual, day_of_week)								# sorts

#Plot number of rides by member type by day of week
options(scipen = 999)
ggplot(data = cleaned_all_data) +
  aes(x = day_of_week, fill = member_casual) +
  geom_bar(position = 'dodge') +
  labs(x = 'Day of week', y = 'Number of rides', fill = 'Member type', title = 'Number of rides by member type')
ggsave("number_of_rides_by_member_type.png")

#Plot average duration by type by day of week
cleaned_all_data %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#Plot number of rides by member type by month
ggplot(data = cleaned_all_data) +
  aes(x = month, fill = member_casual) +
  geom_bar(position = 'dodge') +
  labs(x = 'Month', y = 'Number of rides', fill = 'Member type', title = 'Number of rides per month')

#Plot hourly uses of bikes by type
ggplot(data = cleaned_all_data) +
  aes(x = starting_hour, fill = member_casual) +
  facet_wrap(~day_of_week) +
  geom_bar() +
  labs(x = 'Starting hour', y = 'Number of rides', fill = 'Member type', title = 'Hourly use of bikes throughout the week') +
  theme(axis.text = element_text(size = 5))

#Output csv file for other visualisation use
write.csv(cleaned_all_data, paste0(file_path,"cleaned_all_data.csv"))
