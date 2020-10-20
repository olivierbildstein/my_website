library(tidyverse)
# install.packages("mosaic") # in case you haven't installed the mosaic package
library(mosaic)
library(here)
library(lubridate)


#read the CSV file
bike <- read_csv(here::here("data", "londonBikes.csv"))

# fix dates using lubridate, and generate new variables for year, month, month_name, day, and day_of _week
bike <- bike %>%   
  mutate(
    date=dmy(date),
    year=year(date),
    month = month(date),
    month_name=month(date, label = TRUE),
    day = day(date),
    day_of_week = wday(date, label = TRUE)) 

# generte new variable season_name to turn seasons from numbers to Winter, Spring, etc
bike <- bike %>%  
  mutate(
    season_name = case_when(
      season == 1 ~ "Winter",
      season == 2 ~ "Spring",
      season == 3 ~ "Summer",
      season == 4 ~ "Autumn"
    ),
    
    #relevel the factor, otherwise it would have seasons in alphabetical order
    season_name = fct_relevel(season_name, "Winter", "Spring", "Summer", "Autumn")
  )

# examine what the resulting data frame looks like
glimpse(bike)
skim(bike)


# Time series plot of bikes rented
ggplot(bike, aes(x=date, y=bikes_hired))+
  geom_smooth()+
  geom_point(alpha = 0.4)+
  theme_bw()+
  NULL


#summary statistics
favstats(~ bikes_hired, data= bike)

# if we wanted to get summary statistics by `year`, `day_of_week`,  `month_name`, or `season_name`
# we use mosaic's syntax `Y ~ X` that allows us to facet our analysis of a variable Y by variable X 
# using the syntax `favstats( Y ~ X, data=...)`

favstats(bikes_hired ~ year, data=bike)
favstats(bikes_hired ~ day_of_week, data=bike)
favstats(bikes_hired ~ month_name, data=bike)
favstats(bikes_hired ~ season_name, data=bike)


# Histogram of bikes rented
ggplot(bike, aes(x=bikes_hired))+
  geom_histogram()+
  theme_bw()+
  NULL

# Histogram faceted by season_name
ggplot(bike, aes(x=bikes_hired))+
  geom_histogram()+
  facet_wrap(~season_name)+
  theme_bw()+
  NULL

# Histogram faceted by month_name
ggplot(bike, aes(x=bikes_hired))+
  geom_histogram()+
  facet_wrap(~month_name)+
  theme_bw()+
  NULL

# Histogram faceted by month_name in 4 rows
ggplot(bike, aes(x=bikes_hired))+
  geom_histogram()+
  facet_wrap(~month_name, nrow = 4)+
  theme_bw()+
  NULL


# Density plot 
ggplot(bike, aes(x=bikes_hired))+
  geom_density()+
  theme_bw()+
  NULL

# Density plot filled by season_name 
ggplot(bike, aes(x=bikes_hired))+
  geom_density(aes(fill=season_name), alpha = 0.3)+
  theme_bw()+
  NULL

# Density plot filled by season_name, and faceted by season_name
ggplot(bike, aes(x=bikes_hired))+
  geom_density(aes(fill=season_name), alpha = 0.3)+
  facet_wrap(~season_name, nrow = 4)+
  theme_bw()+
  NULL

# Density plot filled by season_name, and faceted by month_name
ggplot(bike, aes(x=bikes_hired))+
  geom_density(aes(fill=season_name), alpha = 0.3)+
  facet_wrap(~month_name, nrow = 4)+
  theme_bw()+
  theme(legend.position="none")+
  NULL

#Boxplot of bikes_hired  by month
# since 'month' is a number, it treats it as a continuous variable; hence we get just one box
ggplot(bike, aes(x=month, y= bikes_hired))+
  geom_boxplot()+
  theme_bw()+
  NULL


#Boxplot  by month_name
ggplot(bike, aes(x=month_name, y= bikes_hired))+
  geom_boxplot()+
  theme_bw()+
  NULL


#Boxplot by month_name
ggplot(bike, aes(x=month_name, y= bikes_hired, fill=season_name))+
  geom_boxplot()+
  theme_bw()+
  NULL


#Violin plot by month_name
ggplot(bike, aes(x=month_name, y= bikes_hired))+
  geom_violin()+
  theme_bw()+
  NULL


# Summary stats of bikes hired vs rain and snow
favstats(bikes_hired ~ rain, data=bike)
favstats(bikes_hired ~ rain + season_name, data=bike)


favstats(bikes_hired ~ snow, data=bike)
favstats(bikes_hired ~ snow + season_name, data=bike)

#Boxplot of bikes_hired temperature by rain (TRUE/FALSE)
ggplot(bike, aes(x=rain, y= bikes_hired))+
  geom_boxplot()+
  theme_bw()+
  NULL

#Boxplot of bikes_hired temperature by snow (TRUE/FALSE)
ggplot(bike, aes(x=snow, y= bikes_hired))+
  geom_boxplot()+
  theme_bw()+
  NULL


# bikes_hired vs. `avg_temp`, `avg_humidity`, `avg_pressure`, and `avg_windspeed` 
ggplot(bike, aes(x=avg_temp, y= bikes_hired))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  NULL

ggplot(bike, aes(x=avg_humidity, y= bikes_hired))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  NULL

ggplot(bike, aes(x=avg_pressure, y= bikes_hired))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  NULL

ggplot(bike, aes(x=avg_windspeed, y= bikes_hired))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  NULL