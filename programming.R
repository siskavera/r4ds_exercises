# Pipes -------------------------------------------------------------------


# No exercises


library(magrittr)


# Functions basics ---------------------------------------------------------------
#   Didn't do most

# 5. Implement a fizzbuzz function. It takes a single number as input. If the number is divisible by three, 
#   it returns “fizz”. If it’s divisible by five it returns “buzz”. If it’s divisible by three and five, 
#   it returns “fizzbuzz”. Otherwise, it returns the number. Make sure you first write working code before you create the function.

fizzbuzz <- function(x) {
  div_by_3 <- x %% 3 == 0
  div_by_5 <- x %% 5 == 0
  if (div_by_3 && div_by_5) {
    "fizzbuzz"
  }
  else if (div_by_3) {
    "fizz"
  }
  else if (div_by_5) {
    "buzz"
  }
  else {
    x
  }
}

fizzbuzz(1)

# 6. Write both_na(), a function that takes two vectors of the same length and returns the number of positions 
#   that have an NA in both vectors.

both_na <- function(x, y) {
  is_na_x <- is.na(x)
  is_na_y <- is.na(y)
  length(which(is_na_x & is_na_y))
}

x <- c(1,2,NA,4,NA,6,NA)
y <- c(NA,NA,3,4,NA,6,NA)

both_na(x,y)

# 7. What do the following functions do? Why are they useful even though they are so short?

is_directory <- function(x) file.info(x)$isdir
# Is x a directory? Useful e.g. before reading-writing

is_readable <- function(x) file.access(x, 4) == 0
# Is x readable? Useful before reading

# 8. Read the complete lyrics to “Little Bunny Foo Foo”. There’s a lot of duplication in this song. 
#   Extend the initial piping example to recreate the complete song, and use functions to reduce the duplication.
# LoL...


# Functions for humans ----------------------------------------------------

# 1. Read the source code for each of the following three functions, puzzle out what they do, and then brainstorm better names.

f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}
f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}
f3 <- function(x, y) {
  rep(y, length.out = length(x))
}

# 2. Take a function that you’ve written recently and spend 5 minutes brainstorming a better name for it and its arguments.
# myScript.R -> analyse_f3
# Arguments: target population - pop_target

# 3. Compare and contrast rnorm() and MASS::mvrnorm(). How could you make them more consistent?
# One takes SD, the other variance (SD^2)

# 4. Make a case for why norm_r(), norm_d() etc would be better than rnorm(), dnorm(). Make a case for the opposite.
# Can search for prefixes: different functions for normal distribution vs functions for the different distributions


# Conditional -------------------------------------------------------------

# 1. What’s the difference between if and ifelse()? Carefully read the help and construct three examples 
#   that illustrate the key differences.
# If is for one value, ifelse is to fill a vector with values
# Ifelse does not work for "verbs" only for "nouns"
x <- runif(10)
y1 <- ifelse(x < 0.5, 0, 1)

for (i in 1:length(x)) {
  if (x[i] < 0.5) {
    y2[i] <- 0
  } else {
    y2[i] <- 1
  } 
}

# 2. Write a greeting function that says “good morning”, “good afternoon”, or “good evening”, depending 
#   on the time of day. (Hint: use a time argument that defaults to lubridate::now(). That will make it 
#   easier to test your function.)
greeting <- function(time_now = lubridate::now()) {
  hour_now <- lubridate::hour(time_now)
  
  if (hour_now < 12) {
    "good morning"
  } else if (hour_now < 18) {
    "good afternoon"
  } else {
    "good night"
  }
}

greeting()

# 3. How could you use cut() to simplify this set of nested if-else statements?

temp_original <- function(temp){
  if (temp <= 0) {
    "freezing"
  } else if (temp <= 10) {
    "cold"
  } else if (temp <= 20) {
    "cool"
  } else if (temp <= 30) {
    "warm"
  } else {
    "hot"
  }
}

temp_mine <- function(temp) {
  breaks <- c(-Inf, 0, 10, 20, 30, +Inf)
  labels <- c("freezing", "cold", "cool", "warm", "hot")
  cut(temp, breaks, labels)
}

temp_original(15)
temp_mine(0)

#   How would you change the call to cut() if I’d used < instead of <=? What is the other chief advantage of cut() 
#   for this problem? (Hint: what happens if you have many values in temp?)
# Use right = T switch
# Cleaner code for many cut breaks

# 4. What happens if you use switch() with numeric values?
switch(5,
       "g" = "baba",
       "d" = "nana",
       stop("other"))

# If the integer is between 1 and nargs()-1 then the corresponding element of ... is evaluated and the 
#   result returned: thus if the first argument is 3 then the fourth argument is evaluated and returned.

# 5. What does this switch() call do? What happens if x is “e”?
x <- "a"
switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)
#   Experiment, then carefully read the documentation.

#  If there is a match then that element is evaluated unless it is missing, in which case the 
#   next non-missing element is evaluated
# . If there is more than one match, the first matching element is used. In the case of no match, 
#   if there is a unnamed element of ... its value is returned.


# Function arguments ------------------------------------------------------

# 1. What does commas(letters, collapse = "-") do? Why?
commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])

commas(letters, collapse = "-")
# Error because of resetting collapse

# 2. It’d be nice if you could supply multiple characters to the pad argument, e.g. rule("Title", pad = "-+"). 
#   Why doesn’t this currently work? How could you fix it?
rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- (getOption("width") - nchar(title) - 5)/stringr::str_length(pad)
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}

rule("Title", pad = "-+")

# 3. What does the trim argument to mean() do? When might you use it?
# the fraction (0 to 0.5) of observations to be trimmed from each end of x before the mean is computed. 
#   Values of trim outside that range are taken as the nearest endpoint.
# Something else at the ends?

# 4. The default value for the method argument to cor() is c("pearson", "kendall", "spearman"). 
#   What does that mean? What value is used by default?
# One of these three are accepted arguments, pearson is the default one


# Returns and environment -------------------------------------------------
# No exercises :(
# Pipeability
# Return: last expression by default
# Invisible: return but don't print - good for piping!
# Environment: searches for value "from inside to out"
