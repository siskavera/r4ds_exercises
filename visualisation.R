library(ggplot2)
library(tibble)
#library(dplyr)

# Check basic info
?mpg
summary(mpg)
lapply(mpg, class)

# Faceted plot
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  facet_grid(drv~ cyl)

# Smooth line plot
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(color = class)) +
  geom_point(mapping = aes(color = class))
#  geom_smooth(data = dplyr::filter(mpg, class = "subcompact"))

# Barplot
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

# Jitter
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter()

# Boxplot
ggplot(data = mpg, mapping = aes(x = drv, y = hwy, fill = class)) +
  geom_boxplot(position = 'dodge') +
  coord_flip()

# Map data
ggplot(map_data("nz"), aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# Pie chart
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity), width = 1) +
  coord_polar()

# produces a stacked bar chart
ggplot(mpg, aes(x = 1, fill=factor(drv))) + 
  geom_bar(width=1, stat='count') 

# produces the equivalent pie chart
ggplot(mpg, aes(x = 1, fill=factor(drv))) + 
  geom_bar(width=1, stat='count') +
  coord_polar(theta='y')

# Line + dots
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline()
  coord_fixed()
