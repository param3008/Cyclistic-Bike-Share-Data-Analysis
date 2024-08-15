##Installing Packages and importing libraries

install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
library(ggplot2)
install.packages("skimr")
library(skimr)
install.packages("janitor")
library(janitor)
install.packages("dplyr")
library(dplyr)
library(readr)    
#Importing Data from the system

X202301_divvy_tripdata <- read_csv("C:/Users/purni/OneDrive/Desktop/Cyclistic Bike share/Latest_Dataset/202301-divvy-tripdata.csv")
print(X202301_divvy_tripdata)
View(X202301_divvy_tripdata)
X202302_divvy_tripdata <- read_csv("C:/Users/purni/OneDrive/Desktop/Cyclistic Bike share/Latest_Dataset/202302-divvy-tripdata.csv")
print(X202302_divvy_tripdata)
View(X202302_divvy_tripdata)
X202303_divvy_tripdata <- read_csv("C:/Users/purni/OneDrive/Desktop/Cyclistic Bike share/Latest_Dataset/202303-divvy-tripdata.csv")
print(X202303_divvy_tripdata)
View(X202303_divvy_tripdata)
X202304_divvy_tripdata <- read_csv("C:/Users/purni/OneDrive/Desktop/Cyclistic Bike share/Latest_Dataset/202304-divvy-tripdata.csv")
print(X202304_divvy_tripdata)
View(X202304_divvy_tripdata)
X202305_divvy_tripdata <- read_csv("C:/Users/purni/OneDrive/Desktop/Cyclistic Bike share/Latest_Dataset/202305-divvy-tripdata.csv")
print(X202305_divvy_tripdata)
View(X202305_divvy_tripdata)
X202306_divvy_tripdata <- read_csv("C:/Users/purni/OneDrive/Desktop/Cyclistic Bike share/Latest_Dataset/202306-divvy-tripdata.csv")
print(X202306_divvy_tripdata)
View(X202306_divvy_tripdata)
X202307_divvy_tripdata <- read_csv("C:/Users/purni/OneDrive/Desktop/Cyclistic Bike share/Latest_Dataset/202307-divvy-tripdata.csv")
print(X202307_divvy_tripdata)
View(X202307_divvy_tripdata)
X202308_divvy_tripdata <- read_csv("C:/Users/purni/OneDrive/Desktop/Cyclistic Bike share/Latest_Dataset/202308-divvy-tripdata.csv")
print(X202308_divvy_tripdata)
View(X202308_divvy_tripdata)
X202309_divvy_tripdata <- read_csv("C:/Users/purni/OneDrive/Desktop/Cyclistic Bike share/Latest_Dataset/202309-divvy-tripdata.csv")
print(X202309_divvy_tripdata)
View(X202309_divvy_tripdata)
X202310_divvy_tripdata <- read_csv("C:/Users/purni/OneDrive/Desktop/Cyclistic Bike share/Latest_Dataset/202310-divvy-tripdata.csv")
print(X202310_divvy_tripdata)
View(X202310_divvy_tripdata)
X202311_divvy_tripdata <- read_csv("C:/Users/purni/OneDrive/Desktop/Cyclistic Bike share/Latest_Dataset/202311-divvy-tripdata.csv")
print(X202311_divvy_tripdata)
View(X202311_divvy_tripdata)
X202312_divvy_tripdata <- read_csv("C:/Users/purni/OneDrive/Desktop/Cyclistic Bike share/Latest_Dataset/202312-divvy-tripdata.csv")
print(X202312_divvy_tripdata)
View(X202312_divvy_tripdata)

#Binding the data into one as all data contains same column

data_df <- rbind(X202301_divvy_tripdata, X202302_divvy_tripdata, X202303_divvy_tripdata,X202304_divvy_tripdata, X202305_divvy_tripdata, X202306_divvy_tripdata, X202307_divvy_tripdata, X202308_divvy_tripdata, X202309_divvy_tripdata, X202310_divvy_tripdata, X202311_divvy_tripdata, X202312_divvy_tripdata)
print(data_df)
View(data_df)
colnames(data_df)
summary(data_df)
head(data_df)
print(data_df)
str(data_df)
glimpse(data_df)
##Rename column names according to the data given
new_df <- data_df %>%
  dplyr::rename('trip_id'= ride_id
                ,'bike_type' = rideable_type
                ,'start_time' = started_at 
                ,'end_time' = ended_at
                ,'starting_point' = start_station_name
                ,'start_station_id' = start_station_id
                ,'ending_point' = end_station_name 
                ,'end_station_id' = end_station_id 
                ,'user_type' = member_casual)
print(new_df)
colnames(new_df)
glimpse(new_df)
View(new_df)
## Now Looking for NA or null values
sum(is.na(new_df))
##Removing null values
new_df_1 <- new_df[!(is.na(new_df$start_station_id)),]
print(new_df_1)
View(new_df_1)
sum(is.na(new_df_1))
new_df_2 <- na.omit(new_df_1)
print(new_df_2)
View(new_df_2)
sum(is.na(new_df_2))

#removing some unnecessary column
data_new <- new_df_2 %>%
  select(-c(start_lat,start_lng,end_lat,end_lng))
print(data_new)
View(data_new)
##Add date, day, month, year and time column separately
data_new$date <- as.Date(data_new$start_time)
data_new$month <- format(as.Date(data_new$start_time),"%m")
data_new$day <- format(as.Date(data_new$start_time),"%d")
data_new$day_of_the_week <- format(as.Date(data_new$start_time),"%A")
data_new$year <- format(as.Date(data_new$start_time),"%y")
data_new$month_name <- lubridate::month(data_new$start_time, label = TRUE, abbr = FALSE)
data_new$start_time1 <- format(as.POSIXct(data_new$start_time),
                               format = "%H:%M:%S")
data_new$end_time1 <- format(as.POSIXct(data_new$end_time),
                             format = "%H:%M:%S")
data_new$end_date <- as.Date(data_new$end_time)
#calculate the ride length and add a column for further calculations 
data_new$ride_length <- difftime(data_new$end_time,data_new$start_time)
print(data_new)
View(data_new)
#selecting Required column fot the analysis
df_1 <- data_new %>% select(trip_id,bike_type,date,start_time1,end_date,end_time1,ride_length,starting_point,start_station_id,ending_point,end_station_id,user_type,month_name,year,day_of_the_week )
print(df_1)
View(df_1)
#renaming columns for better readability
clean_data <- df_1 %>%
  dplyr::rename('trip_id'= trip_id
                ,'bike_type' = bike_type
                ,'start_date' = date
                ,'start_time' = start_time1
                ,'end_date' = end_date
                ,'end_time' = end_time1
                ,'ride_length' = ride_length
                ,'starting_point' = starting_point
                ,'start_station_id' = start_station_id
                ,'ending_point' = ending_point
                ,'end_station_id' = end_station_id 
                ,'user_type' = user_type
                ,'month' = month_name
                ,'year' = year
                ,'day' = day_of_the_week)
print(clean_data)
View(clean_data)
glimpse(clean_data)
str(clean_data)
summary(clean_data)
colnames(clean_data)
sum(is.na(clean_data))

#Removing '0' and '-ve' and rest null values from ride length
clean_data$ride_length[clean_data$ride_length < 0] <- NA
print(clean_data)
sum(is.na(clean_data))
final_data <- na.omit(clean_data)
print(final_data)
sum(is.na(final_data))
data_final <- final_data[final_data$ride_length != 0, ]
print(data_final)

#checking for unique values in the selected columns for better understanding the data for further analysis
print(unique(data_final$bike_type))
print(unique(data_final$user_type))
print(unique(data_final$starting_point))

#Calculate Mean, Median,Max, Min

aggregate(data_final$ride_length ~ data_final$user_type, FUN = mean)

aggregate(data_final$ride_length ~ data_final$user_type, FUN = median)

aggregate(data_final$ride_length ~ data_final$user_type, FUN = max)

aggregate(data_final$ride_length ~ data_final$user_type, FUN = min)

#Ordered Days based on Weekdays and months based on month 
data_final$day <- ordered(data_final$day, 
                          levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
data_final$month <- ordered(data_final$month, 
                            levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
#Average ride time by day and user type Table
avg_ride_time <- data_final %>%
  group_by(day, user_type) %>%
  summarise(avg_ride_time = mean(ride_length)) %>%
  arrange(user_type,day)
print(avg_ride_time)

#Total ride count by day and user type Table
number_of_rides <- data_final %>%
  group_by(user_type, day) %>%
  summarise(total_rides = n(), avg_ride_sec = mean(ride_length)) %>%
  arrange(user_type, day)
print(number_of_rides)
#Total ride count by month and user type Table
number_of_rides_month_wise <- data_final %>%
  group_by(user_type, month) %>%
  summarise(total_rides = n(), avg_ride_sec = mean(ride_length)) %>%
  arrange(user_type, month)
print(number_of_rides_month_wise)
#Print the bar chart day vs total number of rides
library(ggplot2)
ride_counts <- data_final %>%
  group_by(day, user_type) %>%
  summarise(count = n())
ride_count_bar <- ggplot(ride_counts, aes(x = day, y = count, fill = user_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Rides by Weekday and Member Type",
       x = "Day of the Week",
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
print(ride_count_bar)

#ride over time line chart print

ride_counts_per_Month <- data_final %>%
  group_by(month, user_type) %>%
  summarise(count = n())
ride_over_time_line_chart <- ggplot(ride_counts_per_Month, aes(x = month, y = count, color = user_type, group = user_type)) +
  geom_line(linewidth = 1, linetype = "solid") +
  geom_point(size = 3)
labs(title = "Rides over time",
     x = "month",
     y = "Number of Rides",
     color = "user_type") +
  theme_minimal()
ride_over_time_line_chart + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Ride length vs days bar chart based on user type

ride_length_avg <- data_final %>%
  group_by(day, user_type) %>%
  summarise(total_rides = n(), avg_ride_length = mean(ride_length))
ride_length_over_day_bar <- ggplot(ride_length_avg, aes(x = day, y = avg_ride_length, fill = user_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "length of Rides by Weekday and Member Type",
       x = "Day of the Week",
       y = "Avg length of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
print(ride_length_over_day_bar)

#percentage og rides among user type
total_ride <- data_final %>% 
  group_by(user_type) %>% 
  summarise(num_rides = n()) %>% 
  mutate(percent_total_rides = num_rides/sum(num_rides) * 100)
print(total_ride)

# pie chart displaying percentages of user riders
user_pie <- ggplot(data = total_ride, aes(x = "", y = percent_total_rides, fill = user_type)) +
  geom_bar(stat = "identity", width = 1) +
  geom_label(aes(label = paste0(round(percent_total_rides, 1), "%")), 
             position = position_stack(vjust = 0.5),show.legend = FALSE) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Rides by user_type",
       fill = "User Type") +
  theme_void() +
  theme(legend.position = "right")
print(user_pie)


#Total Ride of users by bike type bar chart
ride_with_bike <- data_final %>%
  group_by(bike_type, user_type) %>%
  summarise(total_rides = n(), avg_ride_length = mean(ride_length))
ggplot(ride_with_bike, aes(x = bike_type, y = total_rides, fill = user_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Rides by bike and Member Type",
       x = "Type of Bike",
       y = "Total no of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# User type vs mean ride time
mean_ride_times <- data_final %>% 
  group_by(user_type) %>% 
  summarise(mean_ride_time_sec = mean(ride_length), max_ride_time=max(ride_length), 
            min_ride_time=min(ride_length))
print(mean_ride_times)
ggplot(data=mean_ride_times) + 
  geom_col(mapping=aes(x=user_type, y=mean_ride_time_sec,fill=user_type)) +
  geom_text(aes(x=user_type, y=mean_ride_time_sec, label=round(mean_ride_time_sec, 2)),vjust=1) +
  labs(title = "Average Ride Times by Usertype", x="Usertype",y="Avg. Ride Time(Sec)")
#It has been seen that Causal Rider: just over 22 min 56 sec average ride time
# Member Rider: just over 12 min 7 sec average ride time

#top 5 station//# Get the top 5 stations with the most num_rides for each user_type
Start_station <- data_final %>% 
  group_by(bike_type, user_type, starting_point) %>% 
  select(user_type, starting_point, bike_type) %>% 
  summarise(total_rides = n())
print(Start_station)
top_5_stations <- Start_station %>%
  group_by(user_type) %>%
  top_n(5, total_rides) %>% 
  arrange(user_type, desc(total_rides))
top_5_stations

## Bar graph for Top 5 start stations by num rides (casual rider)
top_5_stations %>% 
  filter(user_type == "casual") %>% 
  ggplot(aes(x=starting_point, y=total_rides,fill="All Stations")) + 
  geom_bar(stat = "identity",) +
  labs(title = "Top 5 Stations by total_rides (Casual)",
       x = "Start Station", y = "Total Number of Rides") +
  geom_text(aes(x=starting_point,y=total_rides,label=total_rides),vjust=1.5,)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Bar graph for Top 5 start stations by num rides (member rider)
top_5_stations %>% 
  filter(user_type == "member") %>% 
  ggplot(aes(x=starting_point, y=total_rides,fill="All Stations")) + 
  geom_bar(stat = "identity",) +
  labs(title = "Top 5 Stations by total_rides (member)",
       x = "Start Station", y = "Total Number of Rides") +
  geom_text(aes(x=starting_point,y=total_rides,label=total_rides),vjust=1.5,)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculating the number of rides and percent of total rides taken with each type of bike for casual riders
casual_ride <- data_final %>% 
  filter(user_type == "casual") %>% 
  group_by(bike_type) %>% 
  summarise(num_rides = n()) %>% 
  mutate(percent_total_rides = num_rides/sum(num_rides) * 100)
print(casual_ride)

# Pie chart displaying percentages of each bike used for casual riders
casual_pie <- ggplot(data = casual_ride, aes(x = "", y = percent_total_rides, fill = bike_type)) +
  geom_bar(stat = "identity", width = 1) +
  geom_label(aes(label = paste0(round(percent_total_rides, 1), "%")), 
             position = position_stack(vjust = 0.5),show.legend = FALSE) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Rides by bike_type (Casual)",
       fill = "Rideable Type") +
  theme_void() +
  theme(legend.position = "right")
print(casual_pie)

# Calculating the number of rides and percent of total rides taken with each type of bike for member riders
member_ride <- data_final %>% 
  filter(user_type == "member") %>% 
  group_by(bike_type) %>% 
  summarise(num_rides = n()) %>% 
  mutate(percent_total_rides = num_rides/sum(num_rides) * 100)
print(member_ride)

# Pie chart displaying percentages of each bike used for member riders
member_pie <- ggplot(data = member_ride, aes(x = "", y = percent_total_rides, fill = bike_type)) +
  geom_bar(stat = "identity", width = 1) +
  geom_label(aes(label = paste0(round(percent_total_rides, 1), "%")), 
             position = position_stack(vjust = 0.5),show.legend = FALSE) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Rides by bike_type (member)",
       fill = "Rideable Type") +
  theme_void() +
  theme(legend.position = "right")
print(member_pie)

