# Teo Yee Kang
# TP050877
# PFDA Assignment


library("ggplot2")
library("ggcorrplot")
library("dplyr")
library("data.table")

# Loading data =================================================================

# Change working directory
dataset = read.csv(file = "~//4. Hourly weather data.csv")

# Data Pre-processing ==========================================================
str(dataset)

# Check for NA value
table(is.na(dataset))
colSums(is.na(dataset))

# Replace missing values with mean
dataset$wind_speed[is.na(dataset$wind_speed)] = mean(dataset$wind_speed, na.rm = TRUE)
dataset$pressure[is.na(dataset$pressure)] = mean(dataset$pressure, na.rm = TRUE)

# Data Exploration =============================================================

# Data visualization of temperature throughout the 2013 year
ggplot(dataset, aes(x = as.POSIXct(time_hour, "%d/%m/%Y %H:%M", tz = Sys.timezone()), y = temp, colour = origin)) +
  geom_line() + 
  theme_light() + 
  labs(x = "Date", y = "Temperature")

  
# Find and plot the correlation of data
cols = c("day", "temp", "dewp", "humid", "precip", "visib", "month", "pressure")
corr = cor(dataset[,(names(dataset) %in% cols)])
corr_mat = cor_pmat(dataset[,(names(dataset) %in% cols)])
ggcorrplot(corr, hc.order = TRUE, type = "lower", p.mat=corr_mat)


# Data analysis ================================================================

# Analysis 1 
# Find the number of days rained or snowed across the months in both the region
# Table
dataset %>% 
  group_by(origin, month, day) %>%
  summarise(precip = sum(precip)) %>%
  filter(precip != 0) %>%
  summarise(precip = n()) %>%
  View()

# Visualization
dataset %>% 
  group_by(origin, month, day) %>%
  summarise(precip = mean(precip)) %>%
  filter(precip != 0) %>%
  ggplot(aes(x = month, fill = origin)) + 
  geom_bar(color = "black", size = 1) + 
  facet_wrap(~origin) + 
  labs(y = 'Count of Days Rained/Snowed', x = "Month") +
  ggtitle("Number of days rained/snowed across the Months in year 2013") +
  scale_x_discrete(limits = factor(1:12)) + 
  theme_light()


# Analysis 2
# Analyze the total precipitations across the month in both the region
# Table
dataset %>% 
  group_by(origin, month, day) %>%
  summarise(precip = sum(precip)) %>%
  summarise(total_precip = sum(precip)) %>%
  View()

# Visualization
dataset %>% 
  group_by(origin, month, day) %>%
  summarise(precip = sum(precip)) %>%
  summarise(total_precip = sum(precip)) %>%
  ggplot(aes(x = month, y = total_precip, color = origin)) + 
  geom_line(size = 1) + 
  facet_wrap(~origin) + 
  labs(y = "Total Precipitation", x = "Month") +
  ggtitle("Total Precipitation across the Month in 2013") +
  scale_x_discrete(limits=factor(1:12)) + 
  theme_light()


# Analysis 3
# Find the longest rain/snow time consecutively

# JFK region
JFK_consec = dataset %>%
  filter(origin == "JFK") %>%
  group_by(consec_wet_id = rleid(precip != 0)) %>%
  filter(precip != 0) %>%
  mutate(start_time = first(time_hour)) %>%
  group_by(origin, start_time) %>%
  summarise(consec_precip = n()) %>%
  arrange(consec_precip) %>%
  top_n(3)

# LGA region
LGA_consec = dataset %>%
  filter(origin == "LGA") %>%
  group_by(consec_wet_id = rleid(precip != 0)) %>%
  filter(precip != 0) %>%
  mutate(start_time = first(time_hour)) %>%
  group_by(origin, start_time) %>%
  summarise(consec_precip = n()) %>% 
  arrange(consec_precip) %>%
  top_n(3)

# Table
View(rbind(JFK_consec, LGA_consec))

# Visualization
ggplot(rbind(JFK_consec, LGA_consec), aes(x = start_time, y = consec_precip)) +
  geom_bar(aes(fill = origin), stat = 'identity', width = .5) +
  labs(x = "Start time", y = "Consecutive hour count") +
  ggtitle("Number of consecutive hours rained or snowed in year 2013") +
  theme_light()


# Analysis 4
# Analyze the temperature changes across the month in both the region
ggplot(dataset, aes(x = day, y = temp, colour = origin)) +
  geom_point() +
  geom_smooth(method = "loess", formula = 'y ~ x') + 
  theme_light() + 
  ggtitle("Temperature changes across the Months") +
  labs(x = "day", y = "Temperature") +
  ggtitle("Temperature changes across the months") +
  facet_wrap(~month)


# Analysis 5
# Analyze the relation of temperature changes and the range of months(season)
dataset %>% 
  mutate(season = ifelse((3 <= month & month <= 5), "Spring",
                         ifelse((6 <= month & month <= 8), "Summer",
                         ifelse((9 <= month & month <= 11), "Autumn", "Winter")))) %>%
  ggplot(aes(x = season,y = temp)) +
  geom_boxplot(aes(fill = season)) +
  geom_jitter(aes(color = season), alpha = 0.3) +
  ggtitle("Temperature changes across the season") +
  labs(x = "Season", y = "Temperature") +
  facet_wrap(~origin) +
  theme_light()


# Analysis 6
# Analyze the relation between variable (temperature - dew point) and humidity
dataset %>%
  mutate(diff_in_temp_dewp = temp - dewp) %>%
  ggplot(aes(x = humid, y = diff_in_temp_dewp)) +
  geom_point() +
  ggtitle("Relation between (Temperature - Dew point) and Humidity") +
  labs(x = "Relative Humidity (%)", y = "Different in Temperature and Dew point (F)") +
  theme_light()


# Extra feature 1
# Add a non-linear line for the relation for better prediction and analysis
dataset %>%
  mutate(diff_in_temp_dewp = temp - dewp) %>%
  ggplot(aes(x = humid, y = diff_in_temp_dewp)) +
  geom_point() +
  ggtitle("Relation between (Temperature - Dew point) and Humidity") +
  labs(x = "Relative Humidity (%)", y = "Different in Temperature and Dew point (F)") +
  geom_smooth(
    method = "nls", 
    se = FALSE, 
    formula = y ~ a * log(x) + k, 
    method.args = list(start=c(a=1, k=1)), 
    color="red", 
    size = 1.5
    ) +
  theme_light()


# Analysis 7
# Analyze the relation between temperature and dew point
ggplot(dataset, aes(x = dewp, y = temp)) +
  geom_point() +
  ggtitle("Relation between Dew point and Temperature") +
  labs(x = "Dew point (F)", y = "Temperature (F)") +
  theme_light()


# Extra feature 2
# Adding a linear graph to represent the relationship better
ggplot(dataset, aes(x = dewp, y = temp)) +
  geom_point() +
  geom_abline(
    intercept = coef(lm(dataset$temp~dataset$dewp))[1], 
    slope = coef(lm(dataset$dewp~dataset$temp))[2],
    size = 1.5,
    color = "red"
    ) +
  ggtitle("Relation between Dew point and Temperature") +
  labs(x = "Dew point (F)", y = "Temperature (F)") +
  theme_light()


# Analysis 8
# Analyze on the visibility changes across the months according to season
# Table
dataset %>%
  mutate(season = ifelse((3 <= month & month <= 5), "Spring",
                         ifelse((6 <= month & month <= 8), "Summer",
                         ifelse((9 <= month & month <= 11), "Autumn", "Winter")))) %>%
  group_by(month,season) %>%
  summarise(visib = mean(visib))

# Visualization
dataset %>%
  mutate(season = ifelse((3 <= month & month <= 5), "Spring",
                         ifelse((6 <= month & month <= 8), "Summer",
                         ifelse((9 <= month & month <= 11), "Autumn", "Winter")))) %>%
  group_by(month,season) %>%
  summarise(visib = mean(visib)) %>%
  ggplot(aes(x = month, y = visib)) +
  geom_point(aes(color = season),size = 5) +
  ggtitle("Visibility changes across the months and season") +
  labs(x = "Month", y = "Visibility (miles)") +
  geom_line() +
  scale_x_discrete(limits = factor(1:12)) + 
  theme_light()


# Analysis 9
# Analyze the uncomfortable days according to dew point
dataset %>% 
  group_by(month, day) %>%
  summarise(dewp = mean(dewp)) %>%
  mutate( comfortability = ifelse((dewp >= 55 & dewp < 60), "Humid", 
                                  ifelse((dewp >= 60 & dewp < 65), "Muggy", 
                                  ifelse((dewp >= 65 & dewp < 70), "Uncomfortable", 
                                  ifelse((dewp >= 70), "Very Uncomfortable", "Comfortable"))))) %>% 
  ggplot(aes(x = month , y = dewp, color = comfortability)) +
  geom_point(size = 4) +
  ggtitle("Dew point against Month according to uncomfortable range") +
  labs(x = "Month", y = "Dew point (F)") +
  scale_x_discrete(limits = factor(1:12)) + 
  theme_light()


# Extra feature 3
# Visualize the uncomfortable range
dataset %>% 
  mutate(season = ifelse((3 <= month & month <= 5), "Spring",
                         ifelse((6 <= month & month <= 8), "Summer",
                                ifelse((9 <= month & month <= 11), "Autumn", "Winter")))) %>%
  group_by(season, month, day) %>%
  summarise(dewp = mean(dewp)) %>% 
  ggplot(aes(x = month , y = dewp, color = season)) +
  geom_rect(aes(xmin = -Inf,xmax = Inf, ymin = 70,ymax = Inf, fill = "Very Uncomfortable"), colour = NA, alpha = 0.02) +
  geom_rect(aes(xmin = -Inf,xmax = Inf, ymin = 65,ymax = 70, fill = "Uncomfortable"), colour = NA, alpha = 0.02) +
  geom_rect(aes(xmin = -Inf,xmax = Inf, ymin = 60,ymax = 65, fill = "Muggy"), colour = NA, alpha = 0.02) +
  geom_rect(aes(xmin = -Inf,xmax = Inf, ymin = 55,ymax = 60, fill = "Humid"), colour = NA, alpha = 0.05) +
  ggtitle("Dew point against Month according to uncomfortable range") +
  labs(x = "Month", y = "Dew point (F)") +
  scale_x_discrete(limits = factor(1:12)) + 
  scale_fill_manual('Uncomfortable Range',
    values = c('lightyellow', 'skyblue2', 'palegreen2', 'pink'), 
    guide = guide_legend(override.aes = list(alpha = 1))) +
  geom_point(size = 4) +
  theme_light()


# Analysis 10
# Find the wind speed and intensity changes across the month
dataset %>%
  mutate(wind_intensity = ifelse((wind_speed <= 0), "Calm",
                                  ifelse((wind_speed <= 15), "Light",
                                  ifelse((15 < wind_speed & wind_speed <= 25), "Breezy",
                                  ifelse((25 < wind_speed & wind_speed <= 35), "Windy", "Very Strong"))))) %>%
  ggplot(aes(x = as.POSIXct(time_hour, "%d/%m/%Y %H:%M", tz=Sys.timezone()), y = wind_speed, color = wind_intensity)) +
  geom_point() + theme_light() +
  ggtitle("Wind speed changes against date time") +
  labs(x = "Date", y = "Wind Speed")


# Analysis 11
# Find the number flights delayed due to low visibility of < 0.5
# Table
dataset %>% 
  filter(visib < 0.5) %>%
  group_by(origin, month) %>%
  summarise(num_of_flights = n())

# Visualization
dataset %>% 
  filter(visib < 0.5) %>%
  group_by(origin, month) %>%
  ggplot(aes(x = month ,  fill = origin)) +
  geom_histogram(stat = "count") +
  scale_x_discrete(limits = factor(1:12)) + 
  ggtitle("Number of flights delayed due to visibility across the yar 2013") +
  labs(x = "Month", y = "Number of flights delayed") +
  theme_light()


# Analysis 12
# Analyze which runway to use depends on wind direction for both airport
# JFK airport
dataset[!is.na(dataset$wind_dir), ] %>%
  filter(origin == "JFK") %>%
  mutate(runway = ifelse((wind_dir < 76 | wind_dir > 346), "Runway 04L/R",
                         ifelse((wind_dir > 76 & wind_dir < 166), "Runway 13L/R",
                         ifelse((wind_dir > 166 & wind_dir < 256), "Runway 22L/R",
                         ifelse((wind_dir > 256 & wind_dir < 346), "Runway 31L/R", "Null"))))) %>%
  ggplot(aes(x = wind_dir, fill = runway)) +
  geom_bar() +
  ggtitle("Runway to use according to wind direction in JFK airport") +
  labs(x = "Wind direction", y = "Count") +
  coord_polar(start = 0) +
  theme_light() 

# LGA airport
dataset[!is.na(dataset$wind_dir), ] %>%
  filter(origin == "LGA") %>%
  mutate(runway = ifelse((wind_dir < 77 | wind_dir > 347), "Runway 04",
                         ifelse((wind_dir > 77 & wind_dir < 167), "Runway 13",
                         ifelse((wind_dir > 167 & wind_dir < 257), "Runway 22",
                         ifelse((wind_dir > 257 & wind_dir < 347), "Runway 31", "Null"))))) %>%
  ggplot(aes(x = wind_dir, fill = runway)) +
  geom_bar() +
  coord_polar(start = 0) +
  ggtitle("Runway to use according to wind direction in LGA airport") +
  labs(x = "Wind direction", y = "Count") +
  theme_light() 


# Analysis 13
# Find the number of times each runways used in each airport

# JFK airport
JFK = dataset[!is.na(dataset$wind_dir), ] %>%
  filter(origin == "JFK") %>%
  mutate(runway = ifelse((wind_dir < 76 | wind_dir > 346), "Runway 04L/R",
                         ifelse((wind_dir > 76 & wind_dir < 166), "Runway 13L/R",
                                ifelse((wind_dir > 166 & wind_dir < 256), "Runway 22L/R",
                                       ifelse((wind_dir > 256 & wind_dir < 346), "Runway 31L/R", "Null"))))) %>%
  group_by(origin, runway) %>%
  summarise(number_of_time_used = n()) 

# LGA airport
LGA = dataset[!is.na(dataset$wind_dir), ] %>%
  filter(origin == "LGA") %>%
  mutate(runway = ifelse((wind_dir < 77 | wind_dir > 347), "Runway 04",
                         ifelse((wind_dir > 77 & wind_dir < 167), "Runway 13",
                                ifelse((wind_dir > 167 & wind_dir < 257), "Runway 22",
                                       ifelse((wind_dir > 257 & wind_dir < 347), "Runway 31", "Null"))))) %>%
  group_by(origin, runway) %>%
  summarise(number_of_time_used = n())

# Table
rbind(JFK,LGA)

# Visualization
ggplot(rbind(JFK,LGA), aes(x = runway, y = number_of_time_used, color = runway)) +
  geom_point(size = 6.5) +   
  geom_segment(aes(
    x = runway, 
    xend = runway, 
    y = 0, 
    yend = number_of_time_used
  ), 
  size = 1) +
  ggtitle("Runway used according to wind direction in JFK airport") +
  labs(x = "runway", y = "Count") +
  facet_wrap(~origin) +
  coord_flip() +
  theme_light() 


# Analysis 14
# Analyze on how the sea level pressure would impact on the precipitation
dataset %>%
  group_by(origin, month, day) %>%
  summarise(
    precip = mean(precip),
    pressure = mean(pressure)
  ) %>%
  ggplot(aes(x = pressure, y = precip, color = origin)) +
    geom_point(size = 3) +
    geom_vline(xintercept = 1028, size = 1, color = 'red') +
    ggtitle("Sea level pressure against Precipitation (inches)") +
    labs(x = "Pressure", y = "Precipitation (inches)") +
    theme_light()


# Analysis 15
# Calculate the number of days that Frost/Aircraft icing occurs
# Table
dataset %>%
  group_by(origin, month, day) %>%
  summarise(temp = mean(temp)) %>%
  filter(temp < 32) %>%
  summarise(days_frost_occur = n()) %>% 
  View()

# Visualization
dataset %>%
  group_by(origin, month, day) %>%
  summarise(temp = mean(temp)) %>%
  filter(temp < 32) %>%
  summarise(day_frost_occur = n()) %>% 
  ggplot(aes(x = month, y = day_frost_occur, fill = origin)) +
  geom_bar(stat = 'identity') +
  ggtitle("Days that Frost/Aircraft icing occurs according to temperature") +
  labs(x = "Month", y = "Number of days") +
  scale_x_discrete(limits = factor(1:12)) + 
  theme_light()


# Analysis 16
# Find the frequency of hours that Frost/Aircraft icing occurs in a day 
# based on the months Frost/Air craft icing occurred
# Table
dataset %>%
  filter(month %in% c(1, 2, 11, 12)) %>%
  filter(temp < 32) %>%
  group_by(hour) %>%
  summarise(count_frost_occur = n()) %>%
  View()

# Visualization
dataset %>%
  filter(month %in% c(1, 2, 11, 12)) %>%
  filter(temp < 32) %>%
  ggplot(aes(x = hour)) +
  geom_bar(aes(fill = origin), color = 'black', width = 0.8) +
  labs(x = "Hours", y = "Frequency that Frost/Aircraft icing occurred") +
  ggtitle("Frequency of hours that Frost/Aircraft icing occurred") +
  theme_light()


# Analysis 17
# Find the mean, max and min of dew point across the month
# Table
dataset %>%
  group_by(origin,month) %>%
  summarise(
    mean = mean(dewp),
    max = max(dewp),
    min = min(dewp)
  ) %>%
  View()

# Visualization
dataset %>%
  group_by(origin, month) %>%
  summarise(
    mean = mean(dewp),
    max = max(dewp),
    min = min(dewp)
  ) %>%
  ggplot(aes(x = month, color = origin)) +
  geom_line(aes(y = mean), size = 1.5) +
  geom_line(aes(y = max), size = 1.5) +
  geom_line(aes(y = min), size = 1.5) +
  ggtitle("Max, Mean and min of dew point") +
  labs(x = "Month", y = "Dewp") +
  scale_x_discrete(limits = factor(1:12)) + 
  theme_light()


# Analysis 18
# Analyze on the relation between visibility and humidity
ggplot(dataset, aes(x = visib, y = humid)) +
  geom_point() +
  geom_abline(
    intercept = coef(lm(dataset$humid~dataset$visib))[1], 
    slope = coef(lm(dataset$humid~dataset$visib))[2],
    size = 1.5,
    color = "red") +
  ggtitle("Relation of visibility and humidity") +
  labs(x = "Visibility", y = "Relative humidity") +
  theme_light()
