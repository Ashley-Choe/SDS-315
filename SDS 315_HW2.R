#Problem 1: Beauty in the Classroom
#Part A: Histogram of Course Evaluation Scores
library(ggplot2)
# Load data
profs <- read.csv("profs.csv")
# Create histogram
ggplot(profs, aes(x = eval)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Course Evaluation Scores", 
       x = "Evaluation Score", y = "Frequency") +
  theme_minimal()

#Part B: Boxplot of Scores by Native English Speaker Status
# Create boxplot
ggplot(profs, aes(x = native, y = eval, fill = native)) +
  geom_boxplot() +
  labs(title = "Course Evaluation Scores by Native English Speaker Status", 
       x = "Native English Speaker", y = "Evaluation Score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

#Part C: Faceted Histogram by Gender
# Create faceted histogram
ggplot(profs, aes(x = eval, fill = gender)) +
  geom_histogram(binwidth = 0.2, color = "black", position = "dodge") +
  facet_wrap(~gender) +
  labs(title = "Distribution of Course Evaluation Scores by Gender", 
       x = "Evaluation Score", y = "Frequency") +
  theme_minimal()

#Part D: Scatterplot of Attractiveness vs. Evaluation Scores
# Create scatterplot
ggplot(profs, aes(x = beauty, y = eval)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship Between Physical Attractiveness and Course Evaluation Scores", 
       x = "Physical Attractiveness", y = "Evaluation Score") +
  theme_minimal()


#Problem 2: Bike Sharing
#Plot A: Average Hourly Rentals Across the Day
bikeshare <- read.csv("bikeshare.csv")
# Average hourly rentals
library(dplyr)
hourly_avg <- bikeshare %>% group_by(hr) %>% summarize(avg_total = mean(total))
# Line graph
ggplot(hourly_avg, aes(x = hr, y = avg_total)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Average Hourly Bike Rentals", x = "Hour", y = "Average Rentals") +
  theme_minimal()
#Plot B: Average Hourly Rentals by Working Day
# Group by hour and working day
workingday_avg <- bikeshare %>% group_by(hr, workingday) %>% summarize(avg_total = mean(total))
# Faceted line graph
ggplot(workingday_avg, aes(x = hr, y = avg_total, color = factor(workingday))) +
  geom_line(size = 1) +
  facet_wrap(~workingday) +
  labs(title = "Average Hourly Bike Rentals by Working Day Status", 
       x = "Hour", y = "Average Rentals", color = "Working Day") +
  theme_minimal()

#Plot C: Ridership During 9 AM by Weather Situation
# Filter data for 9 AM
bikeshare_9am <- bikeshare %>% filter(hr == 9) %>% group_by(weathersit, workingday) %>% summarize(avg_total = mean(total))
# Bar plot
ggplot(bikeshare_9am, aes(x = factor(weathersit), y = avg_total, fill = factor(workingday))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~workingday) +
  labs(title = "Average Ridership at 9 AM by Weather Situation", 
       x = "Weather Situation", y = "Average Rentals", fill = "Working Day") +
  theme_minimal()

#Problem 3: Capital Metro UT Ridership
#Faceted Line Graph: Average Boardings by Hour and Day of Week
capmetro <- read.csv("capmetro_UT.csv")
# Average boardings
boarding_avg <- capmetro %>% group_by(hour_of_day, day_of_week, month) %>% summarize(avg_boarding = mean(boarding))
# Faceted line graph
ggplot(boarding_avg, aes(x = hour_of_day, y = avg_boarding, color = month)) +
  geom_line(size = 1) +
  facet_wrap(~day_of_week) +
  labs(title = "Average Boardings by Hour and Day of Week", 
       x = "Hour of Day", y = "Average Boardings", color = "Month") +
  theme_minimal()
#Faceted Scatterplot: Boardings vs. Temperature
# Scatterplot
ggplot(capmetro, aes(x = temperature, y = boarding, color = factor(weekend))) +
  geom_point(alpha = 0.6) +
  facet_wrap(~hour_of_day) +
  labs(title = "Boardings vs. Temperature by Hour of Day", 
       x = "Temperature (F)", y = "Boardings", color = "Weekend") +
  theme_minimal()

#Problem 4: Wrangling the Billboard Top 100
#Part A: Top 10 Most Popular Songs
billboard <- read.csv("billboard.csv")
# Count weeks on chart
song_weeks <- billboard %>% group_by(performer, song) %>% summarize(weeks = n()) %>% arrange(desc(weeks)) %>% head(10)
song_weeks

#Part B: Musical Diversity Over Time
# Unique songs per year
diversity <- billboard %>% group_by(year) %>% summarize(unique_songs = n_distinct(song))
# Line graph
ggplot(diversity, aes(x = year, y = unique_songs)) +
  geom_line(color = "green", size = 1) +
  labs(title = "Musical Diversity on Billboard Top 100 Over Time", 
       x = "Year", y = "Number of Unique Songs") +
  theme_minimal()

#Part C: Artists with 30+ Ten-Week Hits
# Ten-week hits
hits <- billboard %>% group_by(performer, song) %>% filter(n() >= 10) %>% group_by(performer) %>% summarize(ten_week_hits = n()) %>% filter(ten_week_hits >= 30)
# Bar plot
ggplot(hits, aes(x = reorder(performer, ten_week_hits), y = ten_week_hits)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Artists with 30+ Ten-Week Hits", 
       x = "Artist", y = "Number of Ten-Week Hits") +
  theme_minimal()