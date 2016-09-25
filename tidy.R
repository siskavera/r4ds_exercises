library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(ggplot2)

################
### Examples ###
################
table1 <- tibble(country = c("Afghanistan", "Afghanistan", "Brazil", "Brazil", "China", "China"),
                 year = c(1999, 2000, 1999, 2000, 1999, 2000),
                 cases = c(745, 2666, 37737, 80488, 212258, 213766),
                 population = c(19987071, 20595360, 172006362, 174504898, 1272915272, 1280428583))

table2 <- tibble(country = c("Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan",
                             "Brazil", "Brazil", "Brazil", "Brazil",
                             "China", "China", "China", "China"),
                 year = c(1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000),
                 type = c("cases", "population", "cases", "population", "cases", "population",
                          "cases", "population", "cases", "population", "cases", "population"),
                 count = c(745, 19987071, 2666, 20595360, 37737, 172006362, 80488, 
                           174504898, 212258, 1272915272, 213766, 1280428583))

table3 <- tibble(country = c("Afghanistan", "Afghanistan", "Brazil", "Brazil", "China", "China"),
                 year = c(1999, 2000, 1999, 2000, 1999, 2000),
                 rate = c("745/19987071", "2666/20595360", "37737/172006362",
                          "80488/174504898", "212258/1272915272", "213766/1280428583"))

# Spread across two tibbles
table4a <- tibble(country = c("Afghanistan", "Brazil", "China"),
                  `1999` = c(745, 37737, 212258),
                  `2000` = c(2666, 80488, 213766))  # cases

table4b <- tibble(country = c("Afghanistan", "Brazil", "China"),
                  `1999` = c(19987071, 172006362, 1272915272),
                  `2000` = c(20595360, 174504898, 1280428583))  # population

# 1. Compute the rate for table2, and table4a + table4b. You will need to perform four operations:
#   1. Extract the number of TB cases per country per year.
#   2. Extract the matching population per country per year.
#   3. Divide cases by population, and multiply by 10000.
#   4. Store back in the appropriate place.
#   5. Which representation is easiest to work with? Which is hardest? Why?
tab2_cases <- filter(table2, type == "cases")
tab2_pop <- filter(table2, type == "population")
tab2_rates <- tab2_cases$count / tab2_pop$count
temptab <- tab2_cases
temptab$type <- "rates"
temptab$count <- tab2_rates
table2 <- rbind(table2, temptab)

table4c <- tibble(country = table4a$country,
                  `1999` = table4a$`1999` / table4b$`1999`,
                  `2000` = table4a$`2000` / table4b$`2000`)

table1 %>% 
  mutate(rate = cases / population * 10000)

# 2. Recreate the plot showing change in cases over time using table2 instead of table1. What do you need to do first?
table1 %>% 
  count(year, wt = cases)

ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

table2 %>% filter(type == "cases") %>%
  ggplot(aes(year, count)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country))

#########################
### Gather and Spread ###
#########################
# 1. Why are gather() and spread() not perfectly symmetrical?
#   Carefully consider the following example:
  
stocks <- data_frame(
    year   = c(2015, 2015, 2016, 2016),
    half  = c(   1,    2,     1,    2),
    return = c(1.88, 0.59, 0.92, 0.17)
  )
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)
# Can be reordered
# (Hint: look at the variable types and think about column names.)
# Both spread() and gather() have a convert argument. What does it do?
# Convert converts char to num / complex / factor /...

# 2. Why does this code fail?
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
# Need ``!
#> Error in eval(expr, envir, enclos): Position must be between 0 and n

# 3. Why does spreading this tibble fail? How could you add a new column to fix the problem?

people <- frame_data(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

spread(people, key = key, value = value)
# Two (different) ages for Phillip. Would be data loss otherwise.

# 4. Tidy the simple tibble below. Do you need to spread or gather it? What are the variables?

preg <- frame_data(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)
preg %>% gather(male, female, key = "gender", value = "number")

##########################
### Separate and unite ###
##########################

# 1. What do the extra and fill arguments do in separate()? 
#   Experiment with the various options for the following two toy datasets.
# Extra: what if too many pieces? Either "warn", "drip", or "merge"
# Fill: what if not enough pieces? Either "warn", "right" or "left"

tibble::tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "merge")

tibble::tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = "left")

# 2. Both unite() and separate() have a remove argument. What does it do? 
#   Why would you set it to FALSE?
# If TRUE, remove input column from output data frame. If I don't need the messed up original, it's handy.

# 3. Compare and contrast separate() and extract(). Why are there three variations of separation, 
#   but only one unite?
# Stuff can go wrong with separation, but not really with unite

######################
### Missing values ###
######################

# 1. Compare and contrast the fill arguments to spread() and complete().
# Both take values for missing values. In spread, both explicit and implicit values are filled, but in complete,
#   only implicit ones (since it is for turning implicit into explicit)

# 2. What does the direction argument to fill() do?
#  Direction to fill (previous or next non-missing value)

##################
### Case study ###
##################
who
who1 <- who %>%
  gather(key = 'key', value = 'cases', new_sp_m014:newrel_f65, na.rm = T) %>%
  mutate(key = str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "type", "sexage"), sep = "_") %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex","age"), sep = 1)
  
# 1. In this case study I set na.rm = TRUE just to make it easier to check that we had the correct values. 
#   Is this reasonable? Think about how missing values are represented in this dataset. Are there implicit missing values? 
#   What’s the difference between an NA and zero?
# na.rm sounds reasonable, I guess?? Not sure actually.
# implicit missing value: yes, a LOT (all the NA-s in the original table)
who_na <- who %>%
  gather(key = 'key', value = 'cases', new_sp_m014:newrel_f65, na.rm = T)
summary(who_na) # 329394 out of 405440 is missing. There are 11080 0-s though...
# NA is not data, zero is no new cases in that cohort. Not sure about this either.

# 2. What happens if you neglect the mutate() step?
# Warning at separate, as separator sometimes missing.

# 3. I claimed that iso2 and iso3 were redundant with country. Confirm my claim by creating a table that uniquely 
#   maps from country to iso2 and iso3.
country_codes <- who %>%
  select(country, iso2, iso3) %>%
  distinct()
# Check if unique
count(country_codes, country) # Same for iso2, iso3

# 4. For each country, year, and sex compute the total number of cases of TB. Make an informative visualisation of the data.
total_cases <- who1 %>%
  group_by(country, year, sex) %>%
  summarise(cases = sum(cases))

total_cases %>%
  group_by(year) %>%
  summarise(n = n())

total_cases %>% 
  filter(country == "Afghanistan") %>%
  ggplot(aes(x = year, y = cases, color = sex)) +
  geom_point() +
  geom_line()

