install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')
install.packages('ggplot2')

library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(hms) #time
library(data.table) #exporting data frame

#Importing the CSV files
jan01_df <- read_csv("CSV files/202101-divvy-tripdata.csv") 
feb02_df <- read_csv("CSV files/202102-divvy-tripdata.csv") 
mar03_df <- read_csv("CSV files/202103-divvy-tripdata.csv")
apr04_df <- read_csv("CSV files/202104-divvy-tripdata.csv")
may05_df <- read_csv("CSV files/202105-divvy-tripdata.csv")
jun06_df <- read_csv("CSV files/202106-divvy-tripdata.csv")
jul07_df <- read_csv("CSV files/202107-divvy-tripdata.csv")
aug08_df <- read_csv("CSV files/202108-divvy-tripdata.csv")
sep09_df <- read_csv("CSV files/202109-divvy-tripdata.csv")
oct10_df <- read_csv("CSV files/202110-divvy-tripdata.csv")
nov11_df <- read_csv("CSV files/202111-divvy-tripdata.csv")
dec12_df <- read_csv("CSV files/202112-divvy-tripdata.csv")

# Inspect the dataframes and look for incongruencies
str(jan01_df)

str(feb02_df)

str(mar03_df)

str(apr04_df)

str(may05_df)

str(jun06_df)

str(jul07_df)

str(aug08_df)

str(sep09_df)

str(oct10_df)

str(nov11_df)

str(dec12_df)

# Combining the individual files into one using rbind()
bikerides <- bind_rows(jan01_df, feb02_df, mar03_df, apr04_df, may05_df, jun06_df, jul07_df, aug08_df, sep09_df, oct10_df, nov11_df, dec12_df)

# Confirming the total number of rows for the individual dataframes
rowtotal <- sum(nrow(jan01_df), nrow(feb02_df), nrow(mar03_df), nrow(apr04_df), nrow(may05_df), nrow(jun06_df), nrow(jul07_df), nrow(aug08_df), nrow(sep09_df), nrow(oct10_df), nrow(nov11_df), nrow(dec12_df))

print (rowtotal)

# Confirming the total number of rows for the combined dataframe
print (nrow(bikerides))

str(bikerides)

head (bikerides)

# Add columns that list the date, month, day, and year of each ride
bikerides$date <- as.Date(bikerides$started_at)

bikerides$month <- format(as.Date(bikerides$date), "%b")

bikerides$day <- format(as.Date(bikerides$date), "%d")

bikerides$year <- format(as.Date(bikerides$date), "%Y")

bikerides$day_of_week <- format(as.Date(bikerides$date), "%A")

head(bikerides)

# Remove duplicates from dataframe
bikerides_no_duplicates <- bikerides[!duplicated(bikerides$ride_id), ]
print(paste("Removed", nrow(bikerides) - nrow(bikerides_no_duplicates), "duplicate rows"))

# Creating a column to determine the ride length 
bikerides_v2 <- mutate(bikerides_no_duplicates, ride_length = difftime(ended_at, started_at, units = "mins"))
str(bikerides_v2)

# filtering out trips with a ride length less than 0.
nrow(bikerides_v2[bikerides_v2$ride_length < 0,])
bikerides_v3 <- bikerides_v2[!bikerides_v2$ride_length <0,]
glimpse(bikerides_v3)

# determining the amount of members vs casual riders
rider_type_total <- table(bikerides_v3$member_casual)
View(rider_type_total)

# Statistical analysis
trip_stats <- bikerides_v3 %>%
  group_by(member_casual) %>%
  summarise(average_ride_length = mean(ride_length), standard_deviation = sd(ride_length), median_ride_length = median(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length))
head(trip_stats)

# Determine the mode for the day of the week
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

weekday_mode <- getmode(bikerides_v3$day_of_week)
print(weekday_mode)

# Analyze ridership data by type and weekday
bikerides_v3$day_of_week <- ordered(bikerides_v3$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
bikerides_v3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(rider_type_total = n(), average_ride_length = mean(ride_length)) %>%
  arrange(member_casual, day_of_week)

# Determining the most popular months during 2021
popular_month <- bikerides_v3 %>%
  group_by(month) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(-number_of_rides)

View(popular_month)

# Determine the most popular start station
station_mode <- getmode(bikerides_v3$start_station_name)

print(station_mode)

#Getting Clean Data file
bikerides_v3 %>%
  
  write.csv("bikerides_clean.csv")

# Determine the most popular start station for members
popular_start_stations_member <- bikerides_v3 %>% 
  filter(member_casual == 'member') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(- number_of_starts)

head(popular_start_stations_member)

# Determine the most popular start station for casual riders
popular_start_stations_casual <- bikerides_v3 %>% 
  filter(member_casual == 'casual') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(- number_of_starts)

head(popular_start_stations_casual)

# Visualization of the total number of riders by member type
bikerides_v3 %>% 
  group_by(member_casual) %>% 
  summarise(total_rider_type = n()) %>% 
  ggplot(aes(x = member_casual, y = total_rider_type, fill = member_casual)) + 
  geom_col(position = "dodge") + geom_text(aes(label = total_rider_type, vjust = -0.25)) +
  labs(title = "Number of Riders: Casual vs. Member Riders")

# Visualization of the rider types ride duration

rider_type_average_duration <- bikerides_v3 %>% 
  group_by(member_casual) %>% 
  summarize(average_ride_duration = mean(ride_length))

rider_type_average_duration %>% 
  ggplot(aes(x = member_casual, y = average_ride_duration, fill = member_casual)) +
  geom_col(position = "dodge") + geom_text(aes(label = average_ride_duration, vjust = -0.25)) +
  labs(title = "Average Ride Duration: Casual vs. Member Riders")

# Visualization of the usage by members and casual riders by the weekday

bikerides_v3 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Ride Duration by Day of Week: Casual vs. Member Riders")

# Visualization of the number of trips by members and casual riders by the weekday


bikerides_v3 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides by Day of Week: Casual vs. Member Riders")

# Visualization of the usage by members and casual riders by the month

bikerides_v3$month <- ordered(bikerides_v3$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

bikerides_v3 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length) ) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = number_of_rides, angle = 90)) +
  facet_wrap(~member_casual) + 
  labs(title = "Average Ride Duration by Month: Casual vs. Member Riders")

# Visualization of the number of trips by members and casual riders by the month

bikerides_v3 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = number_of_rides, angle = 90)) +
  labs(title = "Number of Rides by Month: Casual vs. Member Riders")

bikerides_v3 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = month, y = number_of_rides, color = member_casual, group = member_casual)) +
  geom_line() +
  labs(title = "Number of Rides per Month: Casual vs. Member", x = "Month", y = "Number of Rides")

