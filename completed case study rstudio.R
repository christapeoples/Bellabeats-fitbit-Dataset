#Installing the Packages

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("googlesheets4")
install.packages("httr")
install.packages("lubridate")
install.packages("viridisLite")
install.packages("tidyr")
install.packages("skimr")
install.packages("dplyr")
install.packages("janitor")
install.packages("here")
install.packages("sqldf")

#Unloading the Packages to environment
library(tidyverse)
library(sqldf)
library(skimr)
library(here)
library(janitor)
library(dplyr)
library(ggplot2)
library(googlesheets4)
library(httr)
library(lubridate)
library(viridisLite)
library(tidyr)


#Loading the Data set
dailyActivity_merged <- read_csv("../input/fit/Fitabase Data 4.12.16-5.12.16/dailyactivity_merged.csv")
dailyIntensities_merged <- read_csv("../input/fit/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
dailyCalories_merged <- read_csv("~/FitBit Bellabeats case study/Fitabase Data 4.12.16-5.12/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")

dailySteps_merged <- read.csv("~/FitBit Bellabeats case study/Fitabase Data 4.12.16-5.12/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")

heartrate_seconds_merged <- read_csv("~/FitBit Bellabeats case study/Fitabase Data 4.12.16-5.12/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")

minuteSleep_merged <- read.csv("~/FitBit Bellabeats case study/Fitabase Data 4.12.16-5.12/Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv")

sleepDay_merged <- read.csv("~/FitBit Bellabeats case study/Fitabase Data 4.12.16-5.12/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

weightLogInfo_merged <- read.csv("~/FitBit Bellabeats case study/Fitabase Data 4.12.16-5.12/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

minute_METsNarrow <- read_csv("../input/fit/Fitabase Data 4.12.16-5.12.16/minute_METsNarrow_merged.csv")


#Viewing the data Frames

colnames(dailyActivity_merged)
glimpse(dailyActivity_merged)
head(dailyActivity_merged)

colnames(dailyCalories_merged)
glimpse(dailyCalories_merged)
head(dailyCalories_merged)

colnames(dailySteps_merged)
head(dailySteps_merged)
glimpse(dailySteps_merged)

colnames(heartrate_seconds_merged)
head(heartrate_seconds_merged)
glimpse(heartrate_seconds_merged)

head(minuteSleep_merged)
glimpse(minuteSleep_merged)
colnames(minuteSleep_merged)

head(sleepDay_merged)
glimpse(sleepDay_merged)
colnames(sleepDay_merged)

head(weightLogInfo_merged)
glimpse(weightLogInfo_merged)
colnames(weightLogInfo_merged)

head(minuteMETsNarrow_merged)
colnames(minuteMETsNarrow_merged)
glimpse(minuteMETsNarrow_merged)

head(dailyIntensities_merged)
colnames(dailyIntensities_merged)
glimpse(dailyIntensities_merged)


#Filtering entries with values greater than zero
(Logging_occurances <- filter(dailyActivity_merged,LoggedActivitiesDistance > "0"))

#Summarizations of datasets
dailyActivity_merged %>% select(TotalSteps,TotalDistance,SedentaryMinutes) %>% summary()
sleepDay_merged %>% select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>% summary()
weightLogInfo_merged %>% select(WeightPounds, BMI, Id) %>% summary()
hourlySteps_merged %>% select(Id, ActivityHour, StepTotal) %>% summary()
dailyCalories_merged %>% select(Id, ActivityDay, Calories)
minuteMETsNarrow_merged %>% select(Id, ActivityMinute, METs)
print(minuteMETsNarrow_merged)
summary(minuteMETsNarrow_merged)
dailyIntensities_merged %>% select(Id, ActivityDay,SedentaryMinutes, LightActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes)

#Looking at the heartrate dataset for any key values
by_id <- group_by(heartrate_seconds_merged, Id)
heart_rate_summary_by_id <- summarise(by_id, count = n(), mean = mean(Value, na.rm = TRUE), min = min(Value, na.rm = TRUE), max = max(Value, na.rm = TRUE))
print(heart_rate_summary_by_id)
  
#Checking for unique characters present
n_distinct(dailyActivity_merged $ Id)
n_distinct(dailyCalories_merged $ Id)
n_distinct(dailySteps_merged $ Id)
n_distinct(heartrate_seconds_merged $ Id)
n_distinct(minuteSleep_merged $ Id)
n_distinct(sleepDay_merged $ Id)
n_distinct(weightLogInfo_merged $ Id)
n_distinct(hourlySteps_merged $ Id)
n_distinct(dailyIntensities_merged$Id)

#Merging the datassets
daily_activity_ready <- select(dailyActivity_merged, Id, ActivityDate, TotalSteps, TotalDistance, TrackerDistance, LoggedActivitiesDistance, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories) %>%
  rename(id = Id) %>%
  rename(distance_in_km = TotalDistance) %>%
  rename(total_steps = TotalSteps) %>%
  rename(sedentary_minutes = SedentaryMinutes) %>%
  rename(new_date = ActivityDate) %>%
  rename(tracked_kms = TrackerDistance) %>%
  rename(logged_kms = LoggedActivitiesDistance) %>%
  rename(very_active_minutes = VeryActiveMinutes) %>%
  rename(fairly_active_minutes = FairlyActiveMinutes) %>%
  rename(lightly_active_minutes = LightlyActiveMinutes)
  
sleep_day_ready <- sleepDay_merged %>% separate(SleepDay, into = c("new_date", "hour", "_am or _pm"), sep = " ") %>%
                                                    rename(id = Id) %>%
                                                    rename(minutes_asleep = TotalMinutesAsleep) %>%
                                                    rename(minutes_in_bed = TotalTimeInBed) %>%
                                                    select(id, new_date, minutes_asleep, minutes_in_bed)
                                                  
#Combining the two datasets together :Daily Activity and Daily Sleep------------
                                                  
combine_data <- left_join(daily_activity_ready, sleep_day_ready)
glimpse(combine_data)
n_distinct(combine_data $id)
head(combine_data)
glimpse(combine_data)
colnames(combine_data)                                                  
                                                  
#Including the day par week for activities
                                                        
combine_data_completed <- combine_data %>%
mutate(new_date = mdy(new_date)) %>%
mutate(day_of_the_week = weekdays(new_date))
                                                  
combine_data_completed$day_of_the_week <- factor(combine_data_completed$day_of_the_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" ))
head(combine_data_completed)
colnames(combine_data_completed)
glimpse(combine_data_completed)
                                                  
                                                  
##Visualizations/Identifying Trends##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(data = combine_data, mapping = aes(x =total_steps, y = distance_in_km)) + geom_point()                    
ggplot(data = combine_data, mapping = aes(x = total_steps, y = distance_in_km, color = distance_in_km > 25 )) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~id)
ggplot(data = daily_activity_ready, mapping = aes(x = distance_in_km, y = tracked_kms + logged_kms)) +
  geom_point() +
  labs(title = "Do Tracked and Logged Distances Add up to Total Distances?")

logging_occurance <- filter(combine_data_completed, logged_kms > "0")
tracking_occurance <- filter(combine_data_completed, tracked_kms > "0")  
ggplot() +
  geom_bar(data = tracking_occurance, mapping = aes(x = tracked_kms), color = "yellow", position = "dodge")+
  geom_bar(data = logging_occurance, mapping = aes(x = logged_kms), color = "blue", position = "dodge")+
  labs(title = "Instances of Tracking and Logged Distances", x= "kilometers", y = "Count") 

logging_occurance <- filter(combine_data_completed, logged_kms > "0")
logged <- summarize(logging_occurance, logged = sum(logged_kms))
tracking_occurance <- filter(combine_data_completed, tracked_kms > "0") 
tracked <- summarize(tracking_occurance, tracked = sum(tracked_kms))

percent_logged_distance <- (logged)/(tracked + logged)
logged
tracked
percent_logged_distance
head(logging_occurance)
summary(logging_occurance)

#Daily Behaviors per day of the week

steps_by_day <- combine_data_completed %>%
  group_by(day_of_the_week) %>%
  summarise(total_steps = sum(total_steps))
ggplot(data = steps_by_day) +
  geom_col(mapping = aes(x = day_of_the_week, y = total_steps, fill = day_of_the_week))+
  labs(title = "Total Steps by Day")

no_steps <- combine_data_completed %>%
  select(total_steps, sedentary_minutes,day_of_the_week, logged_kms )%>%
  group_by(day_of_the_week)%>%
  filter(total_steps == 0)
ggplot(data = no_steps, mapping = aes(x= day_of_the_week, fill = day_of_the_week))+
  geom_bar()+
  labs(title = "Days with No Steps Recorded")
ggplot(data = combine_data_completed, aes(x=total_steps, y=Calories)) + geom_point()+ stat_smooth() + labs(title = "The Relationship between total steps and calories burned")

ggplot(data = combine_data_completed, aes(x=logged_kms, y=tracked_kms)) + geom_point() + geom_smooth() + labs( title = "The Relationship between tracked and logged instances")
ggplot(data=daily_activity_ready, aes(x=very_active_minutes, y=Calories)) + geom_point() + stat_smooth(method=lm) + labs(title="The Relationship between Very Active Minutes and Total Daily Calories Burned")
ggplot(data=daily_activity_ready, aes(x=very_active_minutes, y=Calories))+ geom_point()+ stat_smooth(method=lm)+ labs(title="The Relationship between Total Daily Steps and Total Daily Calories Burned")
ggplot(data=daily_activity, aes(x=TotalDistance, y=Calories))+ geom_smooth()+ labs(title="The Relationship between Total Distance and Total Daily Calories Burned") 
ggplot(data=daily_activity_ready, aes(x=distance_in_km, y=Calories))+ geom_smooth()+ labs(title="The Relationship between Total Distance and Total Daily Calories Burned") 
ggplot(data=sleep_day_ready, aes(x=minutes_asleep, y=minutes_in_bed))+ geom_point()+ stat_smooth(method=lm)+ labs(title="The Relationship between Total Minutes Asleep and Total Time in Bed") 
ggplot(data = combine_data_completed, aes(x=day_of_the_week, y=Calories)) + geom_point()+ geom_smooth()+ labs(title="The Relationship between days of the week and calories being burned the most")
ggplot(data = combine_data_completed, aes(x=day_of_the_week, y=minutes_asleep))+ geom_point()+ geom_smooth()+ labs(title = "The relationship between minutes asleep and days of the week")
