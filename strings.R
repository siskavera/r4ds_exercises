library(stringr)
library(dplyr)
library(tibble)
library(stringi)

##############
### Basics ###
##############

# 1. In code that doesn’t use stringr, you’ll often see paste() and paste0(). 
#   What’s the difference between the two functions? What stringr function are they equivalent to? 
#   How do the functions differ in their handling of NA?

# paste0 is no separator and single output, a bit more efficient
# paste = str_c

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")
str_c("|-", str_replace_na(x), "-|", collapse = "")

paste0("|-", x, "-|")
# paste uses str_replace_na by default

# 2. In your own words, describe the difference between the sep and collapse arguments to str_c().
# Sep is per term, collapse is inbetween output terms (if it is a vector)
str_c("a","b", c("c","d"), sep = " ", collapse = ",")
str_c("a","b", c("c","d"), sep = " ")

# 3. Use str_length() and str_sub() to extract the middle character from a string. 
#   What will you do if the string has an even number of characters?
x = "daBda"
x = "abcdef"

str_sub(x, floor((str_length(x)+1)/2), ceiling((str_length(x)+1)/2))

# 4. What does str_wrap() do? When might you want to use it?
# It can format paragraphs
# Printing long texts

# 5. What does str_trim() do? What’s the opposite of str_trim()?
# Trims whitespace. Opposite is str_pad()

# 6. Write a function that turns (e.g.) a vector c("a", "b", "c") into the string a, b, and c. 
#   Think carefully about what it should do if given a vector of length 0, 1, or 2.
x = c("a", "b", "c")

my_function <- function(input) {
  output <- ""
  if (length(input) < 2)
  {
    output <- str_c(input)
  }
  else if (length(input) == 2)
  {
    output <- str_c(output, input[1], " and ", input[2])
  }
  else
  {
    for (i in 1:(length(input)-2))
    {
      output <- str_c(output, input[i], ", ")
    }
    output <- str_c(output, input[length(input)-1], " and ", input[length(input)])
  }
  writeLines(output)
}

my_function(x)

#######################
######## Regex ########
#######################

##############
### Basics ###
##############

x <- c("apple", "banana", "pear")
writeLines(x)
str_view(x, "a.")

# 1. Explain why each of these strings don’t match a \: "\", "\\", "\\\".
y = "\\"
writeLines(y)
str_view(y, "\\\\")
# Need to escape "\"
# str_view matches the actual "y", not the interpreted one -> need to escape both

# 2. How would you match the sequence "'\?
z = "\"'\\"
writeLines(z)
str_view(z, "\\\"'\\\\")

# 3. What patterns will the regular expression \..\..\.. match? How would you represent it as a string?
u = "fmeaoi.a.b.c"
writeLines(u)
str_view(u, "\\..\\..\\..")

###############
### Anchors ###
###############

# 1. How would you match the literal string "$^$"?
v = "feaoihfi$^$fe"
writeLines(v)
str_view(v, "\\$\\^\\$")

# 2. Given the corpus of common words in stringr::words, create regular expressions that find all words that:
#   1. Start with “y”.
#   2. End with “x”
#   3. Are exactly three letters long. (Don’t cheat by using str_length()!)
#   4. Have seven letters or more.
# Since this list is long, you might want to use the match argument to str_view() to show only the matching or non-matching words.
str_view(words, "^y", match = T)
str_view(words, "x$", match = T)
str_view(words, "^...$", match = T)
str_view(words, ".......", match = T)

###############
### Classes ###
###############

# 1. Create regular expressions to find all words that:
#   1. Start with a vowel.
#   2. That only contain consonants. (Hint: thinking about matching “not”-vowels.)
#   3. End with ed, but not with eed.
#   4. End with ing or ise.
str_view(words, "^[aeouiy]", match = T)
str_view(words, "^[^aeouiy]*$", match = T)
str_view(words, "[^e]ed$", match = T)
str_view(words, "i(ng|se)$", match = T)

# 2. Empirically verify the rule “i before e except after c”.
str_view(words, "([^c]|)ei", match = T)
# Not quite...

# 3. Is “q” always followed by a “u”?
str_view(words, "q[^u]", match = T)
# Yes.

# 4. Write a regular expression that matches a word if it’s probably written in British English, not American English.
str_view(words, "ize", match = T)

# 5. Create a regular expression that will match telephone numbers as commonly written in your country.
phone_numbers = c("49384","06-1-383-2254","0613832254","06306455952")
str_view(phone_numbers, "06(-|)\\d(\\d|)(-|)\\d\\d\\d(-|)\\d\\d(-|)\\d\\d", match = T)

##################
### Repetition ###
##################

# 1. Describe the equivalents of ?, +, * in {m,n} form.
# {1}, {1,}, {0,}

# 2. Describe in words what these regular expressions match: 
#   (read carefully to see if I’m using a regular expression or a string that defines a regular expression.)
#   1. ^.*$                Anything (even length 0)
#   2.  "\\{.+\\}"         {(at least one character)}
#   3. \d{4}-\d{2}-\d{2}   (4 digits)-(2 digits)-(2 digits)
#   4. "\\\\{4}"           \\\\

# 3. Create regular expressions to find all words that:
#   1. Start with three consonants.
#   2. Have three or more vowels in a row.
#   3. Have two or more vowel-consonant pairs in a row.
str_view(words, "^[^aeouiy]{3}", match = T)
str_view(words, "[aeouiy]{3,}", match = T)
str_view(words, "([aeouiy][^aeouiy]){2,}", match = T)

# 4. Solve the beginner regexp crosswords at https://regexcrossword.com/challenges/beginner.
# Done :)

##################################
### Grouping & back-references ###
##################################

# 1. Describe, in words, what these expressions will match:
#   1. (.)\1\1
#   2. "(.)(.)\\2\\1"
#   3. (..)\1
#   4. "(.).\\1.\\1"
#   5. "(.)(.)(.).*\\3\\2\\1"

a = "fffgeag"
str_view(a, "(.)\\1\\1")
a = "abba"
str_view(a, "(.)(.)\\2\\1")
a = "tata"
str_view(a, "(..)\\1")
a = "abaia"
str_view(a, "(.).\\1.\\1")
a = "abchgehgrheogrcba"
str_view(a, "(.)(.)(.).*\\3\\2\\1")

# 2. Construct regular expressions to match words that:
#   1. Start and end with the same character.
#   2. Contain a repeated pair of letters (e.g. “church” contains “ch” repeated twice.)
#   3. Contain one letter repeated in at least three places (e.g. “eleven” contains three “e”s.)
str_view(words, "^(.).*\\1$", match = T)
str_view(words, "(..).*\\1", match = T)
str_view(words, "(.).*\\1.*\\1", match = T)

######################
######## Tools #######
######################

######################
### Detect matches ###
######################

# 1. For each of the following challenges, try solving it by using both a single regular expression, 
#   and a combination of multiple str_detect() calls.
#   1. Find all words that start or end with x.
#   2. Find all words that start with a vowel and end with a consonant.
#   3. Are there any words that contain at least one of each different vowel?
str_view(words, "^x|x$", match = T)
start_x = str_detect(words, "^x")
end_x = str_detect(words, "x$")
words[start_x | end_x]

str_view(words, "^[aeuioy].*[^aeuioy]$", match = T)
start_vowel = str_detect(words, "^[aeuioy]")
end_consonant = str_detect(words, "[^aeuioy]$")
words[start_vowel & end_consonant]

hasA = str_detect(sentences, "a")
hasE = str_detect(sentences, "e")
hasO = str_detect(sentences, "o")
hasU = str_detect(sentences, "u")
hasI = str_detect(sentences, "i")
hasY = str_detect(sentences, "y")
sentences[hasA & hasE & hasO & hasU & hasI & hasY]

# 2. What word has the highest number of vowels? What word has the highest proportion of vowels? (Hint: what is the denominator?)
counts = str_count(words, "[aeouiy]")
lengths = str_length(words)
result <- tibble(words = words, counts = counts, lengths = lengths)

result %>%
  arrange(desc(counts))

result %>%
  mutate(prop = counts / lengths) %>%
  arrange(desc(prop))

#######################
### Extract matches ###
#######################

# 1. In the previous example, you might have noticed that the regular expression matched “flickered”, which 
#   is not a colour. Modify the regex to fix the problem.
colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colours_mod <- c("[^a-zA-Z0-9]red[^a-zA-Z0-9]", "[^a-zA-Z0-9]orange[^a-zA-Z0-9]", "[^a-zA-Z0-9]yellow[^a-zA-Z0-9]",
             "[^a-zA-Z0-9]green[^a-zA-Z0-9]", "[^a-zA-Z0-9]blue[^a-zA-Z0-9]", "[^a-zA-Z0-9]purple[^a-zA-Z0-9]")
colour_match_mod <- str_c(colours_mod, collapse = "|")
colour_match <- str_c(colours, collapse = "|")

has_colour <- str_subset(sentences, colour_match)
matches <- str_extract_all(has_colour, colour_match)

has_colour_mod <- str_subset(sentences, colour_match_mod)
matches_mod <- str_extract_all(has_colour, colour_match_mod)

has_colour[!(has_colour %in% has_colour_mod)]
has_colour[has_colour %in% has_colour_mod]

# 2. From the Harvard sentences data, extract:
#   1. The first word from each sentence.
#   2. All words ending in ing.
#   3. All plurals.
sentences %>% head(10)
str_extract(sentences, "[^ ]+") %>% head(10)

has_ing <- str_subset(sentences, "[^ ]+ing[ .]")
str_extract(has_ing, "[^ ]+ing[ .]") %>% str_sub(1, -2) %>% head(10)

is_noun <- str_subset(sentences, "(a|the) [^ \\.]+")
nouns <- str_extract(is_noun, "(a|the) [^ \\.]+")
is_plural <- str_subset(nouns, "(a|the) [^ \\.\\']+(s|\\')$")
str_extract(is_plural, "(a|the) [^ \\.\\']+(s|\\')$")

#######################
### Grouped matches ###
#######################

# 1. Find all words that come after a “number” like “one”, “two”, “three” etc. Pull out both the number and the word.
numbers = c("one", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
numbers_match = str_c("(", str_c(numbers, collapse = "|"), ") [^ .]+")

has_number <- str_subset(sentences, numbers_match)
str_extract(has_number, numbers_match) %>% head(10)

# 2. Find all contractions. Separate out the pieces before and after the apostrophe.
has_contraction = str_subset(sentences, "[^ ]+\\'[^ .]+")
str_extract(has_contraction, "[^ ]+\\'[^ .]+")

#########################
### Replacing matches ###
#########################

# 1. Replace all forward slashes in a string with backslashes.
str = "heoagjt/jgoiwe/\\/geoa"
writeLines(str)
writeLines(str_replace_all(str, "/", "\\\\"))

# 2. Implement a simple version of str_to_lower() using replace_all().
str="ghaeHFOEnjofieaJr"
str_replace_all(str, c("A" = "a", "F" = "f", "H" = "h"))

# 3. Switch the first and last letters in words. Which of those strings are still words?
switched <- str_replace(words, "^(.)(.*)(.)$", "\\3\\2\\1")
words[words %in% switched]

#################
### Splitting ###
#################

# 1. Split up a string like "apples, pears, and bananas" into individual components.
str = "apples, pears, and bananas"
str_split(str, ", and |, ")

# 2. Why is it better to split up by boundary("word") than " "?
# boundary(word) deals with whitespaces (any and any number)

# 3. What does splitting with an empty string ("") do? Experiment, and then read the documentation.
# Split by character (equivalent to boundary("character"))

######################
### Other patterns ###
######################

# 1. How would you find all strings containing \ with regex() vs. with fixed()?
strings = c("blabla", "nana", "gigi\\", "da\\da")
str_view(strings, regex("\\\\"))
str_view(strings, fixed("\\"))

# 2. What are the five most common words in sentences?
words <- str_split(sentences, boundary("word"), simplify = T) %>% 
  as.vector()
  
tibble(words = str_to_lower(words)) %>%
  group_by(words) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(6) %>%
  tail(5)

###############
### stringi ###
###############

# 1. Find the stringi functions that:
#   1. Count the number of words.  stri_count_words
#   2. Find duplicated strings.    stri_duplicated
#   3. Generate random text.       stri_rand_strings (stri_rand_lipsum for Lorem ipsum)

# 2. How do you control the language that stri_sort() uses for sorting?
# By the locale argument.
