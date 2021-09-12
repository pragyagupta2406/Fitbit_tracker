# Analyzing Consumer Use of smart wellenss devices
## Data source: FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius). Available via Kaggle website (https://www.kaggle.com/arashnic/fitbit)

setwd("~/Desktop/kaggke/Fitabase_Data")
library(tidyverse)
library(dplyr)
library(ggplot)
library(janitor)
install.packages("skimer")
library(skimer) # not available for R v 3.6.0
library(lubridate)

# import data files

daily_activity<-read_csv("dailyActivity_merged.csv")
daily_calories<-read_csv("dailyCalories_merged.csv")
daily_steps<-read_csv("dailySteps_merged.csv")
heart_rate<-read_csv("heartrate_seconds_merged.csv")
sleep_day<-read_csv("sleepDay_merged.csv")
weight_log<-read_csv("weightLogInfo_merged.csv")

View(daily_activity)
summary(daily_activity)
length(unique(daily_activity$Id)) ## 33 individuals
daily_activity%>% 
mutate(date2 = mdy(ActivityDate))-> daily_activity2 # converting ActivityDate to date format and adding new column day corssponding to the date
daily_activity2%>% 
  mutate(day=wday(date2, label = TRUE, abbr = FALSE)) -> daily_activity2
daily_activity2%>% 
  mutate(day_temp=wday(date2, label = FALSE, abbr = FALSE)) -> daily_activity2
daily_activity2%>% 
  mutate(weekday=ifelse(day_temp==1|day_temp==7, 1, 0)) -> daily_activity2 # Sunday is day 1 and Saturday is day 7. 
# check the data for skewness
ggplot(data=daily_activity) + geom_histogram(mapping=aes(x=VeryActiveDistance)) 
ggplot(data=daily_activity) + geom_histogram(mapping=aes(x=ModeratelyActiveDistance)) 
ggplot(data=daily_activity) + geom_histogram(mapping=aes(x=LightActiveDistance)) 
ggplot(data=daily_activity) + geom_histogram(mapping=aes(x=SedentaryActiveDistance)) 
ggplot(data=daily_activity) + geom_histogram(mapping=aes(x=VeryActiveMinutes)) 
ggplot(data=daily_activity) + geom_histogram(mapping=aes(x=FairlyActiveMinutes)) 
ggplot(data=daily_activity) + geom_histogram(mapping=aes(x=LightlyActiveMinutes)) 
ggplot(data=daily_activity) + geom_histogram(mapping=aes(x=SedentaryMinutes)) 
## Data is skewed

# For each person (represented by Id), calculate the total distances and minutes by activity level (total of all days available per individual)
daily_activity2%>%
  group_by(Id)%>%
  summarise(days_total=n(),Sed_Dist_total= sum(SedentaryActiveDistance), Lit_Dist_total=sum(LightActiveDistance), Mod_Dist_total=sum(ModeratelyActiveDistance),Very_Dist_total=sum(VeryActiveDistance),
            Sed_Min_total= sum(SedentaryMinutes), Lit_Min_total=sum(LightlyActiveMinutes), Mod_Min_total=sum(FairlyActiveMinutes),Very_Min_total=sum(VeryActiveMinutes))->user_group
## 'days_total' counts the total number of days for which the data is available for each individual. Since each rpeated Id represents a different day, we count the number of times each Id is ocurring in the dataset
# Next, we want to catgorize all the 33 individuals into 4 groups based on activity level - Sedntary, Inactive, Active, Very Active
## For that we first find the active minutes per day for each individual for each of the activity level - SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes- and the
## take the median value for each as the benchmark. Median value is used instead of mean because data is skewed to the right and will pull the mean in that direction. 
user_group%>%
  mutate(Sedentary_per_day=Sed_Min_total/days_total, Inactive_per_day=Lit_Min_total/days_total, Active_per_day=Mod_Min_total/days_total, VeryActive_per_day=Very_Min_total/days_total)->user_group
user_group%>%
  summarise(across(c(Sedentary_per_day,Inactive_per_day,Active_per_day,VeryActive_per_day), list(mean=mean, median=median)))->avg_per_day
# we take the median as the benchmark values to determine if a person is Sedntary, Inactive, Active, Very Active
## Sedentary = 1077.55; Inactive = 206.1935; Active = 12.32258, Very Active = 10.3871 (units are in minutes)
user_group%>%
  mutate(VeryActive=ifelse(VeryActive_per_day>10.3876, 1, 0))->user_group
user_group%>%
  mutate(Active=ifelse(VeryActive==0 & Active_per_day>12.32258, 1, 0))->user_group
user_group%>%
  mutate(Inactive=ifelse(VeryActive==0 & Active==0 & Inactive_per_day>206.1935, 1, 0))->user_group
user_group%>%
  mutate(Sedentary=ifelse(VeryActive==0 & Active==0 & Inactive==0, 1, 0 )) -> user_group
user_group%>%
  mutate(activity_level=ifelse(VeryActive==1, "Very Active", ifelse(Active==1, "Active", ifelse(Inactive==1, "Inactive", ifelse(Sedentary==1, "Sedentary", "NA")))))->user_group
user_group%>%
  group_by(activity_level)%>%
  summarise(n())
user_group%>% ## converting the string var to factor var
  mutate(ActivityLevel=recode_factor(activity_level, "Very Active"= "Very Active","Active"="Active", "Inactive"="Inactive", "Sedentary"="Sedentary", "NA"="NA" )) -> user_group
left_join(daily_activity2, user_group, by="Id") -> daily_activity_user_group
daily_activity_user_group%>%
mutate(week=recode_factor(weekday, `0`= "Weekday",`1`="Weekend" )) -> daily_activity_user_group
ggplot(data=daily_activity_user_group,mapping = aes(ActivityLevel, TotalSteps, color=ActivityLevel))+geom_boxplot() +facet_wrap(~week)
ggplot(data=daily_activity_user_group,mapping = aes(ActivityLevel, Calories, color=ActivityLevel))+geom_boxplot() +facet_wrap(~week)
ggplot(data=daily_activity_user_group,mapping = aes(week, TotalSteps, color=week))+geom_boxplot() +facet_wrap(~ActivityLevel)
length(unique(sleep_day$Id)) #24
right_join(daily_activity_user_group, sleep_day, by="Id") -> user_sleep
ggplot(data=user_sleep,mapping = aes(ActivityLevel, TotalMinutesAsleep, color=ActivityLevel))+geom_boxplot()
length(unique(heart_rate$Id))#14

 




















