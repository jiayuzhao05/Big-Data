### Libraries
library(tidyverse)
data(midwest)


### Data
data(starwars)
gapminder <- read_csv('gapminder.csv')
housing_data <- read_csv('texas_housing_data.csv')

### Renaming a col
gapminder
gapminder <- rename(gapminder, nation = country)

### Understanding the `group_by` function
starwars

### Running only group_by
starwars %>% 
  group_by(gender, species) %>% 
  summarize(avg_height = mean(height, na.rm = TRUE),
            max_height = max(height, na.rm = TRUE))


gapminder %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>% 
  summarize(total_pop = sum(pop, na.rm = TRUE))


### Exercise 1: import housing_data.csv

### Exercise 2: we want to know what was the biggest value of sales
### for Austin, San Antonio, Houston and Dallas.
housing_data

## step 1: filter for the cities
## step 2: group by the column you want to see the information displayed
## step 3: use the summary stats function that gets the maximum value 
housing_data %>% 
  filter(city %in% c('Austin', 'San Antonio', 'Houston', 'Dallas')) %>% 
  group_by(city) %>% 
  summarize(biggest_sales = max(sales, na.rm = TRUE))
