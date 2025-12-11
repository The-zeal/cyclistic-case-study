# Load essential libraries
install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")

# Load essential libraries
library(tidyverse)
library(lubridate)
library(janitor)

# List all CSV files in the data folder
file_list <- list.files(path = "C:/data/Cyclistic_Project/data", pattern = "*.csv", full.names = TRUE)

# Read and combine them into one dataframe
all_trips <- file_list %>%
  map_df(read_csv)

# Check the structure of the combined data
glimpse(all_trips)

all_trips <- all_trips %>%
  clean_names()  # from janitor package

head(all_trips)
colnames(all_trips)

summary(all_trips)
any(is.na(all_trips))     # checks for missing values


colSums(is.na(all_trips))

all_trips <- all_trips %>%
  drop_na(started_at, ended_at, member_casual)

# Create ride_length and day_of_week columns
all_trips <- all_trips %>%
  mutate(
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = wday(started_at, label = TRUE, abbr = TRUE)
  )

summary(all_trips$ride_length)

all_trips <- all_trips %>%
  filter(ride_length > 0)

all_trips <- all_trips %>%
  mutate(
    month = format(started_at, "%B"),
    hour = hour(started_at)
  )

glimpse(all_trips)

write_csv(all_trips, "outputs/all_trips_cleaned.csv")


# Average ride length by user type
all_trips %>%
  group_by(member_casual) %>%
  summarise(
    mean_ride = mean(ride_length),
    median_ride = median(ride_length),
    max_ride = max(ride_length),
    min_ride = min(ride_length)
  )

# Average ride duration by day of week and user type
all_trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(mean_ride = mean(ride_length)) %>%
  arrange(member_casual, day_of_week)

all_trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, day_of_week)

all_trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides by Weekday", x = "Day of Week", y = "Number of Rides") +
  theme_minimal()

all_trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_duration = mean(ride_length)) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Ride Duration by Weekday", x = "Day of Week", y = "Average Duration (minutes)") +
  theme_minimal()


all_trips %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Rides by Month", x = "Month", y = "Number of Rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

summary_by_user <- all_trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(mean_ride = mean(ride_length), number_of_rides = n())

write_csv(summary_by_user, "outputs/summary_by_user.csv")













