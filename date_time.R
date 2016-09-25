library(lubridate)

# Data
library(nycflights13)

# EDA
library(dplyr)
library(ggplot2)

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)

################
### Creation ###
################

# 1. What happens if you parse a string that contains invalid dates?

ymd(c("2010-10-10", "bananas"))
# Warning message + NA in answer.

# 2. What does the tzone argument to today() do? Why is it important?
# Time zone. Defaults to system, can make a difference.

# 3. Use the appropriate lubridate function to parse each of the following dates:
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)

##################
### Components ###
##################

# 1. How does the distribution of flight times within a day change over the course of the year?
flights_dt %>%
  mutate(dep_hour = update(dep_time, yday = 1), dep_month = month(dep_time, label = T), dep_day = wday(dep_time, label = T)) %>%
  ggplot(aes(dep_hour, ..density.., colour = dep_month)) +
  geom_freqpoly(binwidth = 300)

# 2. Compare dep_time, sched_dep_time and dep_delay. Are they consistent? Explain your findings.
flights_dt %>%
  mutate(dep_diff = dep_time - sched_dep_time, diff_diff = as.numeric(dep_diff - dep_delay)) %>%
  ggplot(aes(diff_diff)) +
  geom_bar() +
  coord_cartesian(xlim = c(-1000,10000))

# Nope and I'm not really sure why. Maybe rounding? 
#   Plus outliers are where dates are incorrect (scheduled in the evening, departed in the morning)

# 3. Compare airtime with the duration between the departure and arrival. Explain your findings. 
#   (Hint: consider the location of the airport.)
flights_dt %>%
  mutate(duration = as.numeric(arr_time-dep_time), difference = duration - air_time) %>%
  ggplot(aes(dest, difference)) +
  geom_boxplot()
# Time difference
# 20 min difference??? Time spent in air vs counting getting in and out?

# 4. How does the average delay time change over the course of a day? Should you use dep_time or sched_dep_time? Why?
flights_dt %>%
  mutate(sched_dep_hour = update(sched_dep_time, yday = 1), dep_hour = update(dep_time, yday = 1)) %>%
  ggplot(aes(dep_hour, arr_delay)) +
  geom_boxplot(aes(group = cut_width(dep_hour, 1800)))
  
# Depends on the question, no?
# Actually, dep_hour is kind of obvious: flights only during the day (and rather unifrom) so delayed flights are all later.
# Fun to see flights delayed to the next day.

# 5. On what day of the week should you leave if you want to minimise the chance of a delay?
flights_dt %>%
  mutate(day_of_week = wday(sched_dep_time, label = T)) %>%
  ggplot(aes(day_of_week, arr_delay)) +
  geom_boxplot(aes(group = day_of_week))

flights_dt %>%
  mutate(day_of_week = wday(sched_dep_time, label = T)) %>%
  group_by(day_of_week) %>%
  summarise(n = n(), mean_delay = mean(arr_delay, na.rm = T), med_delay = median(arr_delay, na.rm = T))
# Saturday

# 6. What makes the distribution of diamonds$carat and flights$sched_dep_time similar?
# Rounding effects from recording by humans

diamonds %>%
  ggplot(aes(carat)) +
  geom_freqpoly(binwidth = 0.01)

flights_dt %>%
  mutate(dep_hour = update(sched_dep_time, yday = 1)) %>%
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)

# 7. Confirm my hypothesis that the early departures of flights in minutes 20-30 and 50-60 are caused 
#   by scheduled flights that leave early. Hint: create a binary variable that tells you whether or not a flight was delayed.

check_hyp <- flights_dt %>%
  mutate(dep_minute = minute(dep_time), is_delayed = arr_delay > 10)

check_hyp %>%
  filter(is_delayed) %>%
  group_by(dep_minute) %>%
  summarise(n = n(), avg_delay = mean(arr_delay, na.rm = T)) %>%
  ggplot(aes(dep_minute, avg_delay)) +
  geom_line()

# Doesn't seem so to me...

##################
### Time spans ###
##################

# 1. Why is there months() but no dmonths()?
# Different lengths?

# 2. Explain days(overnight * 1) to someone who has just started learning R. How does it work?
# Logical is 1 for true, 0 for false. Adding a day only to overnight flights.

# 3. Create a vector of dates giving the first day of every month in 2015. 
#   Create a vector of dates giving the first day of every month in the current year.
days_2015 <- seq(ymd('2015-01-01'), ymd('2015-12-31'), 'month')
days_this_year <- seq(update(today(), month = 1, day = 1), update(today(), month = 12, day = 1), 'month')

# 4. Write a function that given your birthday (as a date), returns how old you are in years.
my_age <- function(birthday) {
  age <- (birthday %--% today()) %/% years(1)
  return(age)
}

my_birthday = ymd('1990-11-01')
my_age(my_birthday)

# 5. Why can’t (today() %--% (today() + years(1)) / months(1) work?
(today() %--% (today() + years(1)) ) / months(1)
# Bracket was missint
# Also, it's not integer months

##################
### Time zones ###
##################
# No exercises
