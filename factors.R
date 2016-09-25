library(forcats)
library(ggplot2)
library(dplyr)

gss_cat

#############################
### General Social Survey ###
#############################

# 1. Explore the distribution of rincome (reported income). 
#   What makes the default bar chart hard to understand? How could you improve the plot?
gss_cat %>%
  ggplot(aes(rincome)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 2. What is the most common religion in this survey? What’s the most common partyid?
gss_cat %>%
  ggplot(aes(relig %>% fct_infreq() %>% fct_rev() %>% fct_relevel("Other","Don't know","No answer"))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

gss_cat %>%
  ggplot(aes(partyid)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 3. Which religion does denom (denomination) apply to? How can you find out with a table? 
#   How can you find out with a visualisation?
gss_cat %>%
  filter(!(denom %in% c("Not applicable", "No answer", "No denomination"))) %>%
  count(relig)
# Christian and protestant

gss_cat %>%
  ggplot(aes(relig)) +
  geom_bar(aes(fill = denom)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##################
### Reordering ###
##################

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count() %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

# 1. There are some suspiciously high numbers in tvhours. Is the mean a good summary?
relig <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = median(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig, aes(tvhours, reorder(relig, tvhours))) + geom_point()

# 2. For each factor in gss_cat identify whether the order of the levels is arbitrary or principled.

# 3. Why did moving “Not applicable” to the front of the levels move it to the bottom of the plot?
levels(gss_cat$marital) # Kind of
levels(gss_cat$race)    # Nope
levels(gss_cat$rincome) # Yes
levels(gss_cat$partyid) # Yes
levels(gss_cat$relig)   # Nope
levels(gss_cat$denom)   # Nope

# 4. Recreate the display of marital status by age, using geom_area() instead of geom_line(). 
#   What do you need to change to the plot? How might you tweak the levels?

ggplot(by_age, aes(age, prop, fill = fct_rev(marital))) +
  geom_area() +
  labs(colour = "marital")

#################
### Modifying ###
#################

# 1. How have the proportions of people identifying as Democrat, Republican, and Independent changed over time?
party_age <- gss_cat %>%
  mutate(partyid_rough = fct_collapse(partyid,
                                      other = c("No answer", "Don't know", "Other party"),
                                      rep = c("Strong republican", "Not str republican"),
                                      ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                      dem = c("Not str democrat", "Strong democrat"))) %>%
  group_by(year, partyid_rough) %>%
  count() %>%
  mutate(prop = n/sum(n)) 

ggplot(party_age, aes(year, prop, colour = partyid_rough)) +
  geom_line()

# 2. Display the joint distribution of the relig and denom variables in a single plot.
relig_denom <- gss_cat %>%
  mutate(denom = fct_lump(denom, n = 10)) 

ggplot(relig_denom, aes(relig)) +
  geom_bar(aes(fill = denom)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_brewer(palette = "Dark2")

# 3. How could you collapse rincome into a small set of categories?
collapsed_rincome <- gss_cat %>%
  mutate(rincome = fct_collapse(rincome,
                                Other = c("No answer", "Don't know", "Refused", "Not applicable"),
                                "Lt $5000" = c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999"),
                                "$5000 to 9999" = c("$5000 to 5999", "$6000 to 6999", "$7000 to 7999", "$8000 to 9999"),
                                "$10000 or more" = c("$10000 - 14999", "$15000 - 19999", "$20000 - 24999", "$25000 or more")))
