library(dplyr)
library(ggplot2)
library(tibble)
library(readr)

############################
########## Tibbles #########
############################

# 1. How can you tell if an object is a tibble? (Hint: trying print mtcars, which is a regular data frame).
my_tibble <- tibble(x = 1:5, y = 2*x, z = 1)
print(mtcars)
print(my_tibble)

# 2. Practice referring to non-syntactic names by:
#   Plotting a scatterplot of 1 vs 2.
#   Creating a new column called 3 which is 2 divided by 1.
#   Renaming the columns to one, two and three.
#   Extracting the variable called 1.
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)

ggplot(annoying, aes(x = `1`, y = `2`)) +
  geom_point()
annoying <- mutate(annoying, `3` = `2`/`1`) 
colnames(annoying) <- c("one","two","three")

# 3. What does tibble::enframe() do? When might you use it?
x = 2:8
enframed = enframe(x)


# 4. What option controls how many additional column names are printed at the footer of a tibble?
print(as_tibble(mtcars), n = 5, width = Inf)

###############################
###### Reading files ##########
###############################

# 1. What function would you use to read a file where fields were separated with "|"?
# read_delim()

# 2. Apart from file, skip, and comment, what other arguments do read_csv() and read_tsv() have in common?
# a bunch...

# 3. What are the most important arguments to read_fwf()?
# same as the rest + col_positions

# 4. Sometimes strings in a csv file contain commas. To prevent them from causing problems they need to be 
#   surrounded by a quoting character, like " or '. By convention, read_csv() assumes that the quoting character 
#   will be ", and if you want to change it you'll need to use read_delim() instead. What arguments do you need to 
#   specify to read the following text into a data frame?
#   "x,y\n1,'a,b'"
delim_odd <- read.delim(file = "delim.txt", sep = ",", quote = "'", header = F)

# 5. Identify what is wrong with each of the following inline csv files. What happens when you run the code?
read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
read_csv("a,b\n1,2\na,b")
read_csv("a;b\n1;3")


##############################
######## Parsing #############
##############################

# 1. What are the most important arguments to locale()?
# formats, encoding, etc...

# 2. What happens if you try and set decimal_mark and grouping_mark to the same character? 
#   What happens to the default value of grouping_mark when you set decimal_mark to ","? 
#   What happens to the default value of decimal_mark when you set the grouping_mark to "."?
locale(decimal_mark = ".", grouping_mark = ".") # Error
locale(decimal_mark = ",") # Grouping mark becomes "."
locale(grouping_mark = ".") # Same, but reverse
default_locale()

# 3. I didn't discuss the date_format and time_format options to locale(). What do they do? 
#   Construct an example that shows when they might be useful.

# 4. If you live outside the US, create a new locale object that encapsulates the settings for the types of file you read most commonly.

# 5. What's the difference between read_csv() and read_csv2()?
# It uses ; instead of , for EU countries like Hungary, where , is a decimal separator

# 6. What are the most common encodings used in Europe? What are the most common encodings used in Asia? Do some googling to find out.
#   Bla-bla

# 7. Generate the correct format string to parse each of the following dates and times:
  
d1 <- "January 1, 2010"
parse_date(d1, "%B %d, %Y")

d2 <- "2015-Mar-07"
parse_date(d2, "%Y-%b-%d")

d3 <- "06-Jun-2017"
parse_date(d3, "%d-%b-%Y")

d4 <- c("August 19 (2015)", "July 1 (2015)")
parse_date(d4, "%B %d (%Y)")

d5 <- "12/30/14" # Dec 30, 2014
parse_date(d5, "%m/%d/%y")

t1 <- "1705"
parse_time(t1, "%H%M")

t2 <- "11:15:10.12 PM"
parse_time(t2, "%H:%M:%OS %p")
