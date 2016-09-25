library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(nycflights13)
library(ggplot2)
library(lubridate)

##################
##### Filter #####
##################
# Save and print
(jan1 <- filter(flights, month == 1, day == 1))

# Note for comparison
(1/49) * 49 == 1
near((1/49) * 49, 1)

# Note on cumany & cumall
df <- tibble(
  x = c(FALSE, TRUE, FALSE), 
  y = c(TRUE, FALSE, TRUE)
)

filter(df, cumany(x)) # all rows after first TRUE
#> # A tibble: 2 × 2
#>       x     y
#>   <lgl> <lgl>
#> 1  TRUE FALSE
#> 2 FALSE  TRUE
filter(df, cumall(y)) # all rows until first FALSE
#> # A tibble: 1 × 2
#>       x     y
#>   <lgl> <lgl>
#> 1 FALSE  TRUE

# On NA
is.na(NA)

### Exercises for filter ###
# 1. Had an arrival delay of two or more hours.
filter(flights, arr_delay >= 120)
# 2. Flew to Houston (IAH or HOU)
filter(flights, dest == "IAH" | dest == "HOU")
# 3. Were operated by United, American, or Delta
filter(flights, carrier == "UA" | carrier == "AA" | carrier == "DL")
# 4. Departed in summer (July, August, and September)
filter(flights, between(month, 7, 9))
# 5. Arrived more than two hours late, but didn't leave late
filter(flights, arr_delay > 120, dep_delay == 0)
# 6. Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, dep_delay >= 60, arr_delay < dep_delay-30)
# 7. Departed between midnight and 6am (inclusive)
filter(flights, between(dep_time, 0, 360))

# How many flights have a missing dep_time? What other variables are missing? What might these rows represent?
no_dep_time = filter(flights, is.na(dep_time))

##################
##### Arrange ####
##################
# 1. How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
arrange(flights, desc(is.na(dep_time)))
# 2. Sort flights to find the most delayed flights. Find the flights that left earliest.
arrange(flights, desc(dep_delay), dep_time)
# 3. Sort flights to find the fastest flights.
arrange(flights, desc(distance/air_time))

###### I don't think this is what they meant by "fastest"
# Note - this is a bit tricky since the time stamps are just encoded as integers
# so if a flight left at midnight (i.e. dep_time=2400) and arrived at 00:54 (arr_time=54),
# it's hard to just do arr_time - dep_time to get the travel time (you get back -2346, which doesn't make sense). 
# Taking absolute values doesn't help either.
# A workaround solution is just to add 2400 if the travel time is ever negative.
# A better solution is to properly encode the times as timestamps
# note: we use the `mutate` function and the pipe character `%>%`, which haven't been introduced yet

flights %>% mutate(travel_time = ifelse((arr_time - dep_time < 0), 
                                        2400+(arr_time - dep_time),
                                        arr_time - dep_time)) %>% 
  arrange(travel_time) %>% select(arr_time, dep_time, travel_time)

# for demonstration purposes, the naive solution is
arrange(flights, desc(arr_time - dep_time))

# 4. Which flights travelled the longest? Which travelled the shortest?
arrange(flights, distance) %>% select(1:5, distance)
arrange(flights, desc(distance)) %>% select(1:5, distance)


##################
##### Select #####
##################
select(flights, ends_with("time"), ends_with("delay"))
select(flights, matches("time$|delay$"))
select(flights, carrier, everything())

vars = c("carrier", "air_time", "distance")
select(flights, one_of(vars))

select(flights, contains("TIME"))
select(flights, contains("TIME", ignore.case = FALSE))

##################
##### Mutate #####
##################

# Example
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)
# Add new coloumns
mutate(flights_sml, gain=arr_delay - dep_delay, speed=distance*60/air_time, gain_per_hour=gain*60/air_time)
# Keep only new ones
transmute(flights, gain = arr_delay - dep_delay)

# Note on operators
# %/% (integer division) and %% (remainder)
# lead() and lag() allow you to refer to leading or lagging values -> first/last is NA
# m(), cumprod(), cummin(), cummax(); and dplyr provides cummean() for cumulative means
# Ranking: e.g. min_rank()

# 1. Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they're not 
#     really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.
mutate(flights, dep_time = (dep_time %/% 100)*60 + dep_time %% 100)

# 2. Compare airtime with arr_time - dep_time. What do you expect to see? What do you see? What do you need to do to fix it?
times <- mutate(flights, dep_time = (dep_time %/% 100)*60 + dep_time %% 100,
       arr_time = (arr_time %/% 100)*60 + arr_time %% 100,
       arr_minus_dep = (arr_time - dep_time) %% (60*24)) %>% select(arr_minus_dep, air_time) %>% mutate(difference = arr_minus_dep - air_time)

# 3. Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers to be related?
flights %>% 
  mutate(dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100),
         sched_dep_time = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100),
         arr_time = (arr_time %/% 100) * 60 + (arr_time %% 100),
         sched_arr_time = (sched_arr_time %/% 100) * 60 + (sched_arr_time %% 100)) %>%
  transmute(near((sched_dep_time + dep_delay) %% (60*24), dep_time, tol=1))

# 4. Find the 10 most delayed flights using a ranking function. How do you want to handle ties? 
#     Carefully read the documentation for min_rank().
filter(flights, min_rank(desc(dep_delay)) <= 10)

##################
##### Grouping ###
##################

# 1. Brainstorm at least 5 different ways to assess the typical delay characteristics 
#      of a group of flights. Consider the following scenarios:
#   A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time.
#   A flight is always 10 minutes late.
#   A flight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time.
#   99% of the time a flight is on time. 1% of the time it's 2 hours late.
# Which is more important: arrival delay or departure delay?
not_cancelled <- filter(flights, !is.na(dep_delay), !is.na(arr_delay))
flight_delay_summary <- group_by(not_cancelled, flight) %>% summarise(num_flights = n(),
                                                                p_15_early = sum(arr_delay < -15)/num_flights,
                                                                p_15_late = sum(arr_delay > 15)/num_flights,
                                                                p_on_time = sum(arr_delay == 0)/num_flights)
# Correlation between leaving time and delays
flight_delays <- group_by(flights, hour, minute10=minute%/%10) %>% summarise(num_flights = n(),
                                                               n_cancelled = sum(is.na(arr_delay) | is.na(dep_delay)),
                                                               aver_delay = mean(arr_delay, na.rm = TRUE)) %>% filter(num_flights >= 10)
ggplot(flight_delays, mapping=aes(x=(hour*60+minute10*10)/60, y=aver_delay/60)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Scheduled departure [hours]", y = "Arrival delay [hours]")

# 2. Come up with another approach that will give you the same output as not_cancelled %>% count(dest) 
#   and not_cancelled %>% count(tailnum, wt = distance) (without using count()).
not_cancelled %>% count(dest)
group_by(not_cancelled, dest) %>% summarise(n = n())
group_by(not_cancelled, dest) %>% tally()

not_cancelled %>% count(tailnum, wt = distance)
group_by(not_cancelled, tailnum) %>% summarise(n = sum(distance))

# 3. Our definition of cancelled flights (!is.na(dep_delay) & !is.na(arr_delay) ) is slightly suboptimal. 
#  Why? Which is the most important column?
flights %>%
  group_by(departed = !is.na(dep_delay), arrived = !is.na(arr_delay)) %>%
  summarise(n=n())
# DOES NOT WORK - ODD!!!

# 4. Look at the number of cancelled flights per day. Is there a pattern? Is the proportion of cancelled 
#  flights related to the average delay?
# If it arrives, it had to depart. Thus enough to use dep_delay
# Model solution
flights %>%
  mutate(dep_date = lubridate::make_datetime(year, month, day)) %>%
  group_by(dep_date) %>%
  summarise(cancelled = sum(is.na(dep_delay)), 
            n = n(),
            mean_dep_delay = mean(dep_delay,na.rm=TRUE),
            mean_arr_delay = mean(arr_delay,na.rm=TRUE)) %>%
  ggplot(aes(x= cancelled/n)) + 
  geom_point(aes(y=mean_dep_delay), colour='blue', alpha=0.5) + 
  geom_point(aes(y=mean_arr_delay), colour='red', alpha=0.5) + 
  ylab('mean delay (minutes)')

# My exploration
flights %>% mutate(dep_date = lubridate::make_datetime(year, month, day)) %>% 
  group_by(dep_date) %>%
  summarise(n = n(),
            prop_cancelled = sum(is.na(dep_delay))/n,
            mean_dep_delay = mean(dep_delay, na.rm=TRUE),
            mean_arr_delay = mean(arr_delay, na.rm=TRUE)) %>%
  ggplot(aes(x = lubridate::wday(dep_date), y = prop_cancelled)) +
  geom_point()

# 5. Which carrier has the worst delays? Challenge: can you disentangle the effects of bad airports vs. 
#  bad carriers? Why/why not? (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))
delay_by_carrier <- flights %>% group_by(carrier) %>%
  summarise(n = n(),
            prop_cancelled = sum(is.na(dep_delay))/n,
            mean_dep_delay = mean(dep_delay, na.rm=TRUE),
            mean_arr_delay = mean(arr_delay, na.rm=TRUE))

ggplot(data = delay_by_carrier, aes(x = prop_cancelled, y = mean_arr_delay, label = carrier)) +
  geom_text()

# Disentangle
delay_by_airport_carrier <- flights %>% group_by(carrier, origin, dest) %>%
  summarise(n = n(),
            prop_cancelled = sum(is.na(dep_delay))/n,
            mean_dep_delay = mean(dep_delay, na.rm=TRUE),
            mean_arr_delay = mean(arr_delay, na.rm=TRUE)) %>%
  filter(n >= 10) %>%
  arrange(mean_arr_delay)

flights %>%
  summarise(n_distinct(carrier),
            n_distinct(origin),
            n_distinct(dest))

ggplot(data = delay_by_airport_carrier, aes(x = dest, y = prop_cancelled, size = n, colour = carrier)) +
  geom_point()

# 6. For each plane, count the number of flights before the first delay of greater than 1 hour.
data1 <- flights %>% group_by(tailnum) %>% 
  mutate(dep_date = lubridate::make_datetime(year, month, day)) %>%
  arrange(dep_date) %>%
  filter(cumall(arr_delay <= 60)) %>%
  count(sort = TRUE)

# 7. What does the sort argument to count() do. When might you use it?
#   The sort argument to count() sorts by descending order of n. 
#   This is useful because often the most common group is the most important.

##########################
#### Grouped operations ##
##########################

# 2. Which plane (tailnum) has the worst on-time record?
flights %>% filter(!is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarise(n = n(),
            on_time_rec = min(abs(arr_delay))) %>%
  arrange(desc(on_time_rec))

# Model solution
flights %>%
  group_by(tailnum) %>%
  summarise(prop_on_time = sum(arr_delay <= 30 & !is.na(arr_delay))/n(),
            mean_arr_delay = mean(arr_delay, na.rm=TRUE),
            flights = n()) %>%
  arrange(prop_on_time, desc(mean_arr_delay))

flights %>%
  group_by(tailnum) %>%
  filter(all(is.na(arr_delay))) %>%
  tally(sort=TRUE)

# 3. What time of day should you fly if you want to avoid delays as much as possible?
flights %>% mutate(minute_of_day = hour*60 + minute, minute10 = (minute_of_day %/% 10)*10) %>%
  group_by(minute10) %>%
  summarise(n = n(), mean_delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(n > 1) %>%
  ggplot(aes(x = minute10, y = mean_delay)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Model solution
flights %>%
  ggplot(aes(x=hour, fill=arr_delay>5 | is.na(arr_delay))) + geom_bar()

# 4. Delays are typically temporally correlated: even once the problem that caused the initial 
#   delay has been resolved, later flights are delayed to allow earlier flights to leave. Using lag() 
#   explore how the delay of a flight is related to the delay of the immediately preceding flight.
#
# Add both previous journey on same plane and at same dest / origin airport
lag_delays <- flights %>% 
  mutate(new_sched_dep_time = lubridate::make_datetime(year, month, day, hour, minute)) %>%
  group_by(tailnum) %>%
  arrange(new_sched_dep_time) %>%
  mutate(prev_plane_arr = lag(arr_delay), prev_plane_dep = lag(dep_delay)) %>%
  group_by(origin) %>%
  arrange(new_sched_dep_time) %>%
  mutate(origin_dep = lag(dep_delay)) %>%
  group_by(dest) %>%
  arrange(new_sched_dep_time) %>%
  mutate(dep_arr = lag(arr_delay))

ggplot(data = lag_delays, aes(x = arr_delay, y = dep_arr)) +
  geom_point()

# Max's
flights %>%
  mutate(new_sched_dep_time = lubridate::make_datetime(year, month, day, hour, minute)) %>%
  group_by(origin) %>%
  arrange(new_sched_dep_time) %>%
  mutate(prev_flight_dep_delay = lag(dep_delay)) %>%
  ggplot(aes(x=prev_flight_dep_delay, y= dep_delay)) + geom_point()

flights %>%
  mutate(new_sched_dep_time = lubridate::make_datetime(year, month, day, hour, minute)) %>%
  group_by(origin) %>%
  arrange(new_sched_dep_time) %>%
  mutate(prev_flight_dep_delay = lag(dep_delay)) %>%
  lm(dep_delay ~ prev_flight_dep_delay,.) %>% summary()

# 5. Look at each destination. Can you find flights that are suspiciously fast? (i.e. flights 
#   that represent a potential data entry error). Compute the air time a flight relative to the 
#   shortest flight to that destination. Which flights were most delayed in the air?
flights %>%
  mutate(speed = distance/air_time) %>%
  arrange(desc(speed)) %>%
  select(carrier, origin, dest, air_time, distance, speed) %>%
  ggplot(aes(x = dest, y = speed)) +
  geom_boxplot()

# 6. Find all destinations that are flown by at least two carriers. Use that information to rank the carriers.
flights %>%
  group_by(dest) %>%
  mutate(n_distinct_carrier = n_distinct(carrier)) %>%
  filter(n_distinct_carrier > 2) %>%
  group_by(carrier) %>%
  summarise(aver_dep_delay = mean(dep_delay, na.rm = T)) %>%
  arrange(aver_dep_delay)

flights %>%
  group_by(dest) %>%
  filter(n_distinct(carrier)>=2) %>%
  group_by(carrier) %>%
  summarise(possible_transfers = n_distinct(dest)) %>%
  arrange(desc(possible_transfers))