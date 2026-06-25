
### In-class exercise

## (1) Download the `gapminder.csv` dataset from Canvas > Modules > Datasets

## (2) Build the if-else statement according to the steps a-b-c
## We want to create an if-else statement that will:
# (a) Check if a file exists within my folder (first, you will need to discover
# which functions can do that)
# (b) If the file exists (meaning, the condition is TRUE), import that file
# (c) If the file don't exists (meaning, the condition is FALSE), give me meaningful message

if (file.exists('gapminder.csv') == TRUE) {
  
  library(readr)
  gapminder <- read_csv('gapminder.csv')
  
} else {
  
  print('The file does not exist, change your working directory')
  
}

## (3) Let's filter this dataset using square brackets now. The goal is to keep only
# data about the year 2007
gapminder2 <- gapminder[gapminder$year == 2007, ]

## (4) Now, we will get a summarization of your filtered dataset:

## (4.1) According to this dataset, how many people were inhabiting Earth in 2007?
# You might want to use the $ to sum all the elements of your chosen column
sum(gapminder2$pop)

## (4.2) What was the average life expectancy of citizens all countries within this dataset?
mean(gapminder2$lifeExp)

## (4.3) Let's try to use one line of code (take a look at the function named `plot`)
# to create a graph that shows the relationship between gdpPercap (should be our x-axis)
# and lifeExp (will be our y-axis) in 2007.
plot(x = gapminder2$gdpPercap, y = gapminder2$lifeExp)

## (5) Now, we will create a column that is equal to 1 if a country's gdpPercap was 
# below average in 2007 and 0 otherwise. Name your column 'low_gdpPercap".
gapminder2$low_gdpPercap <- ifelse(gapminder2$gdpPercap < median(gapminder2$gdpPercap), 1, 0)

## (6) Create a third dataframe that contains only information about countries that
# were below the median point for gdpPercap (in other words, low_gdpPercap == 1)
gapminder3 <- gapminder2[gapminder2$low_gdpPercap == 1, ]

## (7) Now, let's plot the same graph from (4.3) but for the dataframe from (6)
plot(x = gapminder3$gdpPercap, y = gapminder3$lifeExp)
