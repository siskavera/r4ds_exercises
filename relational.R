library(nycflights13)
library(Lahman)
library(babynames)
library(nasaweather)
library(fueleconomy)
library(dplyr)
library(ggplot2)
library(maps)

flights
airlines
airports
planes
weather

####################
### nycflights13 ###
####################

# 1. Imagine you wanted to draw (approximately) the route each plane flies from its origin to its destination. 
#   What variables would you need? What tables would you need to combine?

# 2. I forgot to draw the a relationship between weather and airports. What is the relationship and how should it appear in the diagram?

# 3. weather only contains information for the origin (NYC) airports. If it contained weather records for all 
#   airports in the USA, what additional relation would it define with flights?

# 4. You might expect that there’s an implicit relationship between plane and airline, because each plane is flown 
#   by a single airline. Confirm or reject this hypothesis using data.
flights %>%
  group_by(tailnum) %>%
  summarise(n = n(), n_carriers = n_distinct(carrier)) %>%
  arrange(desc(n_carriers))
# Nope.

# 5. We know that some days of the year are “special”, and fewer people than usual fly on them. How might you represent
#  that data as a data frame? What would be the primary keys of that table? How would it connect to the existing tables?

############
### Keys ###
############

# 1. Add a surrogate key to flights.
flights_withkey <- mutate(flights, key = row_number())

# 2. Identify the keys in the following datasets

Lahman::Batting %>%
  count(yearID, playerID, stint) %>%
  filter(n > 1)

babynames::babynames %>%
  count(name, year, sex) %>%
  filter(nn > 1)
  
nasaweather::atmos %>%
  count(lat, long, year, month) %>%
  filter(n > 1)

fueleconomy::vehicles %>%
  count(id) %>%
  filter(n > 1)

# Not unique!
ggplot2::diamonds %>%
  count(price, carat, cut, color, clarity, x, y, z, depth, table) %>%
  filter(n > 1)

# 3. Draw a diagram illustrating the connections between the Batting, Master, and Salaries tables in the Lahman package. 
# XXX  Draw another diagram that shows the relationship between Master, Managers, AwardsManagers.
# XXX  How would you characterise the relationship between the Batting, Pitching, and Fielding tables?

Salaries %>%
  count(yearID, playerID, teamID) %>%
  filter(n > 1)

######################
### Mutating joins ###
######################

# 1. Compute the average delay by destination, then join on the airports data frame so you can show 
#   the spatial distribution of delays. Here’s an easy way to draw a map of the United States:

flights %>%
  group_by(dest) %>%
  summarise(aver_delay = mean(arr_delay, na.rm = T)) %>%
  inner_join(airports, c("dest" = "faa")) %>%
  ggplot(aes(lon, lat)) +
  borders("state", fill = "grey30") +
  geom_point(aes(color = aver_delay)) +
  coord_quickmap(xlim = c(-130, -60), ylim = c(25, 50)) +
  scale_color_gradient2(low = "dark blue", mid = "white", high = "dark red")

# (Don’t worry if you don’t understand what semi_join() does — you’ll learn about it next.)
# You might want to use the size or colour of the points to display the average delay for each airport.

# 2. Add the location of the origin and destination (i.e. the lat and lon) to flights.
airports_mut <- airports %>%
  select(faa, lat, lon)

flights %>%
  left_join(airports_mut, c("dest" = "faa")) %>%
  rename(dest_lat = lat, dest_lon = lon) %>%
  left_join(airports, c("origin" = "faa")) %>%
  rename(origin_lat = lat, origin_lon = lon)

# 3. Is there a relationship between the age of a plane and its delays?
flights %>%
  filter( !is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarise(n = n(), aver_delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(n > 5) %>%
  inner_join(planes, "tailnum") %>%
  ggplot(aes(year, aver_delay)) +
  geom_point()

delays <- flights %>%
  filter( !is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarise(n = n(), aver_delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(n > 5) %>%
  inner_join(planes, "tailnum")

my_model <- lm(delays$year ~ delays$aver_delay)
summary(my_model)
anova(my_model)
# Yep

# 4. What weather conditions make it more likely to see a delay?


# 5. What happened on June 13 2013? Display the spatial pattern of delays, and then use Google to cross-reference with the weather.