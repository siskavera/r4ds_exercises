library(purrr)
library(readr)
library(dplyr)

# Basic for loop ----------------------------------------------------------

# 1. Write for loops to:
#   1. Compute the mean of every column in mtcars.
mtcars_means <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  mtcars_means[[i]] <- mean(mtcars[[i]])
}
mtcars_means

#   2. Determine the type of each column in nycflights13::flights.
flight_types <- vector("character", ncol(nycflights13::flights))
for (i in seq_along(nycflights13::flights)) {
  flight_types[[i]] <- class(nycflights13::flights[[i]])[[1]]
}
flight_types

#   3. Compute the number of unique values in each column of iris.
iris_uniq <- vector("double", ncol(iris))
for(i in seq_along(iris)) {
  iris_uniq[[i]] <- length(unique(iris[[i]]))
}
iris_uniq

#   4. Generate 10 random normals for each of  μ=−10μ=−10 ,  00 ,  1010 , and  100100 .
#   Think about the output, sequence, and body before you start writing the loop.
n_numbers <- 10
mus <- c(-10, 0, 10, 100)
df <- tibble::as_tibble(matrix(1.0, nrow = n_numbers, ncol = length(mus)))
colnames(df) <- mus
for(i in seq_along(mus)) {
  df[[i]] <- rnorm(n_numbers, mean = mus[[i]])
}
df

# 2. Eliminate the for loop in each of the following examples by taking advantage of an existing function that works with vectors:
letters <- c("h","e","l","l","o")
out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
out2 <- stringr::str_c(letters, collapse = "")

x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))
sd2 <- sd(x)

x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
out2 <- cumsum(x)

# 3. Combine your function writing and for loop skills:
#   1. Write a for loop that prints() the lyrics to the children’s song “Alice the camel”.
alice_the_camel <- function() {
  n_humps <- c("five", "four", "three", "two", "one", "no")
  
  for (i in seq_along(n_humps)) {
    for (j in 1:3) {
      print(stringr::str_c("Alice the camel has ", n_humps[[i]], " humps."))
    }
    if (i == 0) {
      print("Now Alice is a horse.")
      print("")
    } else {
      print("So go, Alice, go.")
      print("")
    }
  }
}
alice_the_camel()

#   2. Convert the nursery rhyme “ten in the bed” to a function. Generalise it to any number of people in any sleeping structure.
# This is the same...

#   3. Convert the song “99 bottles of beer on the wall” to a function. Generalise to any number of any vessel 
#   containing any liquid on any surface.
# Same again

# 4. It’s common to see for loops that don’t preallocate the output and instead increase the length of a vector at each step:
n_cols <- 50000
x <- tibble::as_tibble(matrix(1.0, nrow = 10, ncol = n_cols))

t1 <- lubridate::now()
output <- vector("integer", 0)
for (i in seq_along(x)) {
  output <- c(output, length(x[[i]]))
}
t2 <- lubridate::now()
exec_time <- t2 - t1

t1 <- lubridate::now()
output2 <- vector("integer", n_cols)
for (i in seq_along(x)) {
  output2[[i]] <- length(x[[i]])
}
t2 <- lubridate::now()
exec_time2 <- t2 - t1

#   How does this affect performance? Design and execute an experiment.
# It slows it down, especially for long vectors.


# For loop variations -----------------------------------------------------

# 1. Imagine you have a directory full of csv files that you want to read in. 
#   You have their paths in a vector, files <- dir("data/", pattern = "\\.csv$", full.names = TRUE), 
#   and now want to read each one with read_csv(). Write the for loop that will load them into a single data frame.
files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)
output <- vector("list", length(files))
for (i in seq_along(files)) {
  output[[i]] <- read_csv(files[[i]])
}
output <- dplyr::bind_rows(output)

# 2. What happens if you use for (nm in names(x)) and x has no names? What if only some of the elements are named? 
#   What if the names are not unique?
x <- c(1,2,3)
for (nm in names(x)) {
  nm # Nothing since names(x) is NULL
}

x <- c(1, b=2, 3)
for (nm in names(x)) {
  print(nm) # Empty for non-named elements
}

x <- c(a = 1, a = 3, 5)
for (nm in names(x)) {
  print(nm) # Prints it without a problem
}

# 3. Write a function that prints the mean of each numeric column in a data frame, along with its name. 
#   For example, show_mean(iris) would print:
show_mean <- function(data) {
  for (i in seq_along(data)) {
    if (class(data[[i]]) == "numeric") {
      this_name <- names(data)[[i]]
      this_mean <- mean(data[[i]])
      cat(this_name, ":\t", format(this_mean, digits=3), "\n", sep = "")
    }
  }
} 

show_mean(iris)
#> Sepal.Length: 5.84
#> Sepal.Width:  3.06
#> Petal.Length: 3.76
#> Petal.Width:  1.20
#   (Extra challenge: what function did I use to make sure that the numbers lined up nicely, even though the variable 
#   names had different lengths?)
# cat with a tab...

# 4. What does this code do? How does it work?

trans <- list( 
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
# It converts the disp and am columns


# For loops vs functionals ------------------------------------------------

# 1. Read the documentation for apply(). In the 2d case, what two for loops does it generalise?
# Depending on margin: for (i in seq_along(df)) or for(i in 1:nrow(df))

# 2. Adapt col_summary() so that it only applies to numeric columns You might want to start with 
#   an is_numeric() function that returns a logical vector that has a TRUE corresponding to each numeric column
col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}

col_summary(mtcars, mean)

col_summary_num <- function(df, fun) {
  is_num <- which(sapply(df, is.numeric))
  out <- vector("double", length(df[,is_num]))
  names(out) <- names(df)[is_num]
  j <- 1
  for (i in is_num) {
    out[j] <- fun(df[[i]])
    j <- j + 1
  }
  out
}

col_summary_num(mtcars, mean)


# The map functions -------------------------------------------------------

# 1. Write code that uses one of the map functions to:
#   1. Compute the mean of every column in mtcars.
mtcars %>% map_dbl(mean)

#   2. Determine the type of each column in nycflights13::flights.
nycflights13::flights %>% map(class) %>% map_chr(1)

#   3. Compute the number of unique values in each column of iris.
iris %>% map(unique) %>% map_int(length)

#   4. Generate 10 random normals for each of  μ=−10μ=−10 ,  00 ,  1010 , and  100100 .
mus <- c(-10, 0, 10, 100)
mus %>% map(rnorm, n = 10)

# 2. How can you create a single vector that for each column in a data frame indicates whether or not it’s a factor?
iris %>% map(is.factor)

# 3. What happens when you use the map functions on vectors that aren’t lists? What does map(1:5, runif) do? Why?
map(1:5, runif)
# Goes through the values, not different at all, no?

# 4. What does map(-2:2, rnorm, n = 5) do? Why? What does map_dbl(-2:2, rnorm, n = 5) do? Why?
map(-2:2, rnorm, n = 5) # As expected
map_dbl(-2:2, rnorm, n = 5) # Error, was expecting a single value for output

# 5. Rewrite map(x, function(df) lm(mpg ~ wt, data = df)) to eliminate the anonymous function.
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))

x <- mtcars %>% split(.$cyl)
map(x, function(df) lm(mpg ~ wt, data = df))
map(x, ~lm(mpg ~ wt, data = .))


# Dealing with failure ----------------------------------------------------

# No exercises
# safely(), transpose(), quietly(), possibly()


# Other types of loops ----------------------------------------------------

# 1. Implement your own version of every() using a for loop. Compare it with purrr::every(). 
#   What does purrr’s version do that your version doesn’t?
x <- list(1:5, letters, list(10))
my_every <- function(input, f) {
  is_true = T
  if (is.null(input)) {
    is_true = F
  }
  for (col in input) {
    if (!f(col)) {
      is_true = F
    }
  }
  is_true
}

x %>% 
  every(is_vector)
x %>%
  my_every(is_vector)

# 2. Create an enhanced col_sum() that applies a summary function to every numeric column in a data frame.
col_sum <- function(input, summary_function) {
  input %>%
    keep(is_numeric) %>%
    map(summary_function)
}

col_sum(x, sum)

# 3. A possible base R equivalent of col_sum() is:
  
col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  
  sapply(df_num, f)
}
#   But it has a number of bugs as illustrated with the following inputs:
  
df <- tibble(
  x = 1:3, 
  y = 3:1,
  z = c("a", "b", "c")
)
# OK
col_sum3(df, mean)
# Has problems: don't always return numeric vector
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean)
#   What causes the bugs?

col_sum(df, mean)
col_sum(df[1:2], mean)
col_sum(df[1], mean)
col_sum(df[0], mean)
 # This was a bit too complicated for me, I don't fully understand...