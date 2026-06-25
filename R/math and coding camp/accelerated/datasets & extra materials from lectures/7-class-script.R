### libraries
library(tidyverse)

### Importing our data
housing_data <- read_csv('Lectures/texas_housing_data.csv')

### Selecting the first 3
select(housing_data, c(1,2,3))
select(housing_data, c(3,2,1))

### Sorting our dataframe
housing_data
arrange(housing_data, desc(city), desc(month), sales)
arrange(housing_data, -year)

## Line 17 = Line 18
filter(housing_data, year == 2013, city == 'Houston')
filter(housing_data, year == 2013 & city == 'Houston')

## 'OR' = |
filter(housing_data, year == 2013 | city == 'Houston')

## Exercise 1
housing_data2 <- filter(housing_data,
       listings > 5500 & median > 235000)

## Exercise 2
housing_data3 <- filter(housing_data,
       city %in% c('Arlington', 'Bay Area', 'Collin County') & listings < 3000)

## Exercise 4
housing_data %>%
  select(c(city, month, listings)) %>% 
  filter(city %in% c('Arlington', 'Bay Area', 'Collin County') & year == 2013 & listings > 3500)

### Creating new columns using 'mutate'
housing_data %>% 
  mutate(ratio = volume / sales,
         harris = 'Ethan',
         big_sales_avg = ifelse(sales > mean(sales, na.rm = TRUE), 1, 0))








