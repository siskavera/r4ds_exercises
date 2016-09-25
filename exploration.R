library(ggplot2)
library(dplyr)

#####################
#### Variation ######
#####################

# 1. Explore the distribution of each of the x, y, and z variables in diamonds. 
#   What do you learn? Think about a diamond and how you might decide which dimension is the length, width, and depth.
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = x), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 20))

# 2. Explore the distribution of price. Do you discover anything unusual or surprising? 
#   (Hint: Carefully think about the binwidth and make sure you try a wide range of values.)
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price), binwidth = 100)+
  coord_cartesian(xlim = c(1450, 1550)) 

diamonds %>%
  filter(price > 1450 & price < 1550) %>%
  select(price) %>%
  group_by(price) %>%
  summarise(n = n())

diamonds %>%
  filter(price < 10000) %>%
  select(price) %>%
  arrange(desc(price)) %>%
  transmute(diff = lag(price) - price) %>%
  ggplot() +
  geom_histogram(mapping = aes(x = diff)) +
  coord_cartesian(ylim = c(0,20)) # There is a gap around 1500???

diamonds %>%
  group_by(price) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# 3. How many diamonds are 0.99 carat? How many are 1 carat? What do you think is the cause of the difference?
diamonds %>%
  filter(carat == 0.99) %>%
  count()

diamonds %>%
  filter(carat == 1) %>%
  count()

diamonds %>%
  filter(carat < 1 & carat > 0.99) %>%
  count()

# 4. Compare and contrast coord_cartesian() vs xlim() or ylim() when zooming in on a histogram. 
#   What happens if you leave binwidth unset? What happens if you try and zoom so only half a bar shows?
diamonds %>%
  ggplot(mapping = aes(x = price)) +
  geom_histogram(binwidth = 10) +
  xlim(0, 1000)

diamonds %>%
  ggplot(mapping = aes(x = price)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(0,1000))

########################
#### Missing values ####
########################

# Just a useful note
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

# 1. What happens to missing values in a histogram? What happens to missing values in bar chart? Why is there a difference? - ???
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()

ggplot(data = diamonds2, mapping = aes(x = y)) + 
  geom_histogram()

ggplot(data = diamonds2, mapping = aes(x = y)) + 
  geom_bar()

# 2. What does na.rm = TRUE do in mean() and sum()?
diamonds2 %>%
  group_by(cut) %>%
  summarise(n = n(), mean_no = mean(y), mean_yes = mean(y, na.rm = T), sum_no = sum(y), sum_yes = sum(y, na.rm = T))

###################
### Covariation ###
###################

# 1. Use what you've learned to improve the visualisation of the departure times of cancelled vs. non-cancelled flights.
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(x = sched_dep_time, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)
# Seems the same then!

# 2. What variable in the diamonds dataset is most important for predicting the price of a diamond? 
#   How is that variable correlated with cut? Why does the combination of those two relationships lead to 
#   lower quality diamonds being more expensive?
diamonds_num <- mutate(diamonds, cut = as.numeric(cut), color = as.numeric(color), clarity = as.numeric(clarity))
for (n_col in 1:4)
{
  print( cor(diamonds_num$price, diamonds_num[,n_col] ) )
}

for (n_col in 1:4)
{
  print( cor(diamonds_num$cut, diamonds_num[,n_col] ) )
}
# The worse the cut, the bigger the diamonds. The bigger the diamond, the pricier it is.

# 3. Install the ggstance package, and create a horizontal boxplot. How does this compare to using coord_flip()?
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

#install.packages("devtools")
#library(devtools)
#devtools::install_github("lionel-/ggstance")
library(ggstance)
ggplot(data = mpg) +
  geom_boxploth(mapping = aes(y = reorder(class, hwy, FUN = median), x = hwy))

# 4. One problem with boxplots is that they were developed in an era of much smaller datasets and tend to 
#   display a prohibitively large number of "outlying values". One approach to remedy this problem is the letter value plot. 
#   Install the lvplot package, and try using geom_lv() to display the distribution of price vs cut. What do you learn?
#   How do you interpret the plots?
#install.packages("lvplot")
library(lvplot)

ggplot(diamonds) +
  geom_lv(mapping = aes(x = cut, y = price))

# 5. Compare and contrast geom_violin() with a facetted geom_histogram(), or a coloured geom_freqpoly(). 
#   What are the pros and cons of each method?
ggplot(diamonds) +
  geom_violin(mapping = aes(x = cut, y = price))

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price)) +
  facet_grid(. ~ cut)

ggplot(diamonds) +
  geom_freqpoly(mapping = aes(x = price, color = cut))

# 6. If you have a small dataset, it's sometimes useful to use geom_jitter() to see the relationship between a 
#   continuous and categorical variable. The ggbeeswarm package provides a number of methods similar to geom_jitter(). 
#   List them and briefly describe what each one does.
#install.packages("ggbeeswarm")
library(ggbeeswarm)
#swarmx adjusts coordinates to the left or right; swarmy adjusts coordinates up or down.

# priority controls the order in which the points are placed; this has generally has a noticeable effect on the resulting appearance. 
# "ascending" gives the "traditional" beeswarm plot in which the points are placed in an ascending order. 
# "descending" is the opposite. 
# "density" prioritizes points with higher local density. 
# "random" places points in a random order. 
# "none" places points in the order provided.

ggplot(mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point()

ggplot(mpg, mapping = aes(x = cty, y = hwy)) +
  geom_jitter()

ggplot(mpg, mapping = aes(x = cty, y = hwy)) +
  geom_beeswarm(priority = "descending")

beeswarm::beeswarm(hwy ~ cty, data = mpg, method = "square")

###########################
### Two categorial vars ###
###########################

# 1. How could you rescale the count dataset above to more clearly show the distribution of cut within colour, or colour within cut?
# Ratios are more representative
diamonds %>%
  group_by(color, cut) %>%
  summarise(n = n()) %>%
  mutate(n_per_color = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n_per_color))

diamonds %>%
  group_by(cut, color) %>%
  summarise(n = n()) %>%
  mutate(n_per_cut = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n_per_cut))

# 2. Use geom_tile() together with dplyr to explore how average flight delays vary by destination and month of year. 
#   What makes the plot difficult to read? How could you improve it?
# Too many destinations, hard to read especially with horizontal labels.
library(nycflights13)
flights %>%
  filter( !is.na(arr_delay) ) %>%
  group_by(dest, month) %>%
  summarise(aver_delay = mean(arr_delay), n = n()) %>%
  mutate(n_per_dest = sum(n), prop_per_dest = n/sum(n)) %>%
  filter(n_per_dest > 500) %>%
  ggplot(mapping = aes(x = dest, y = month)) +
  geom_tile(mapping = aes(fill = aver_delay)) +
  theme(axis.text.x = element_text(angle = -90, hjust = 0)) +
  scale_y_continuous(breaks = 1:12)

# 3. Why is it slightly better to use aes(x = color, y = cut) rather than aes(x = cut, y = color) in the example above?
# More square
diamonds %>%
  group_by(cut, color) %>%
  summarise(n = n()) %>%
  mutate(n_per_cut = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n_per_cut))

diamonds %>%
  group_by(cut, color) %>%
  summarise(n = n()) %>%
  mutate(n_per_cut = n / sum(n)) %>%
  ggplot(mapping = aes(x = cut, y = color)) +
  geom_tile(mapping = aes(fill = n_per_cut))

###########################
### Two continuous vars ###
###########################
install.packages("hexbin")
library(hexbin)

# 1. Instead of summarising the conditional distribution with a boxplot, you could use a frequency polygon. 
#   What do you need to consider when using cut_width() vs cut_number()? How does that impact a visualisation of the 
#   2d distribution of carat and price?
smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_bin2d(bins = 30)

# 2. Visualise the distribution of carat, partitioned by price.
ggplot(smaller, aes(x = price, y = carat)) +
  geom_boxplot(aes(group = cut_number(price, 20)))

# 3. How does the price distribution of very large diamonds compare to small diamonds. Is it as you expect, or does it surprise you?
diamonds %>%
  filter(carat > 3) %>%
  select(carat, price, cut, color) %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point()

large <- diamonds %>%
  filter(carat > 3) %>%
  select(carat, price, cut, color)

# 4. Combine two of the techniques you've learned to visualise the combined distribution of cut, carat, and price.
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d() +
  facet_wrap( ~ cut, nrow = 2)

# 5. Two dimensional plots reveal outliers that are not visible in one dimensional plots. 
#   For example, some points in the plot below have an unusual combination of x and y values, 
#   which makes the points outliers even though their x and y values appear normal when examined separately.

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

#   Why is a scatterplot a better display than a binned plot for this case?
# A single value is really low and not shown
ggplot(diamonds, aes(x = x, y = y)) +
  geom_bin2d(bins = 70) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))
