## Libraries
library(tidyverse)
library(gapminder)
library(nycflights13)
library(ggthemes)

## Data
data(gapminder)
data(starwars)
data(flights)

## Exercise: create a plot that display the evolution of the
# gdp for all the countries of a continent of your choice

## Step 1: create a new column called 'gdp'
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  filter(continent == 'Europe') %>% 
  ggplot(aes(x = year, y = gdp, color = country)) +
  geom_point() +
  facet_wrap(~country) +
  theme_fivethirtyeight()
  








