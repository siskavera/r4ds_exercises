library(purrr)
library(tibble)

# Types of atomic vectors -------------------------------------------------

# 1. Describe the difference between is.finite(x) and !is.infinite(x).
is.finite(NA)
!is.infinite(NA)
# There is stuff that is neither (they are not a complete set)

# 2. Read the source code for dplyr::near(). How does it work?
# Is it within tolerance? By the fault, uses square root of machine double eps

# 3. A logical vector can take 3 possible values. How many possible values can an integer vector take? 
#   How many possible values can a double take? Use google to do some research.
# Integers have one special value: NA, while doubles have four: NA, NaN, Inf and -Inf

# 4. Brainstorm at least four functions that allow you to convert a double to an integer. How do they differ? Be precise.
# Floor, ceil, round 0.5 up, round 0.5 down

# 5. What functions from the readr package allow you to turn a string into logical, integer, and double vector?
# parse


# Using atomic vectors ----------------------------------------------------

# 1. What does mean(is.na(x)) tell you about a vector x? What about sum(!is.finite(x))?
# Proportion of missing data and number of not finite elements, respectively

# 2. Carefully read the documentation of is.vector(). What does it actually test for? 
#   Why does is.atomic() not agree with the definition of atomic vectors above?
# is.vector returns true for lists by default. Can have modes, which are more like atomic?
# NULL is is.atomic, but not is_atomic
# I'm not sure what the huge problem is...
# See more in is_vector,... documentation

# 3. Compare and contrast setNames() with purrr::set_names().
# If the second argument is ommitted a vector is named with itself. 
# Also stricter argument testing - whatever that means...

# 4. Create functions that take a vector as input and returns:
#   1. The last value. Should you use [ or [[?
#   2. The elements at even numbered positions.
#   3. Every element except the last value.
#   4. Only even numbers (and no missing values).
x <- c(1, 2, 3, 5, 2, Inf, NA, -5, -10, -Inf)
                                        
last_val <- function(x) {
  x[[length(x)]]
}
last_val(x)

even_elements <- function(x) {
  x[seq(2, length(x), 2)]
}
even_elements(x)

all_but_last <- function(x) {
  x[1:(length(x)-1)]
}
all_but_last(x)

only_evens <- function(x) {
  x_is_finite <- is.finite(x)
  x_is_even <- x %% 2 == 0
  x[x_is_finite & x_is_even]
}
only_evens(x)

# 5. Why is x[-which(x > 0)] not the same as x[x <= 0]?
x[-which(x > 0)]
x[x <= 0]
# Not sure...
                                        
# 6. What happens when you subset with a positive integer that’s bigger than the length of the vector? 
#   What happens when you subset with a name that doesn’t exist?
# Bigger than length: NA
# Non-existing name: NA


# Lists -------------------------------------------------------------------

# 1. Draw the following lists as nested sets:
#   list(a, b, list(c, d), list(e, f))
#   list(list(list(list(list(list(a))))))

# 2. What happens if you subset a tibble as if you’re subsetting a list? What are the key differences between a list and a tibble?
a_list <- list(a = c(1,2,3), b = c("a","b","c"), c = 1)
a_tibble <- tibble(a = c(1,2,3), b = c("a","b","c"), c = 1)
# Tibble is like a matrix, c gets recycle (elements need to have the same length)
# Subsetting seems to work the same way


# Attributes --------------------------------------------------------------
# Can be anything, no exercises
attr(a_list, "batman") <- "nanananana"
attributes(a_list)

# Augmented vectors -------------------------------------------------------

# 1. What does hms::hms(3600) return? How does it print? What primitive type is the augmented vector built on top of? 
#   What attributes does it use?
# 1 o'clock, built on difftime
# units, class

# 2. Try and make a tibble that has columns with different lengths. What happens?
# If constant, gets recycle; otherwise error

# 3. Based of the definition above, is it ok to have a list as a column of a tibble?
# Sure, but it's odd