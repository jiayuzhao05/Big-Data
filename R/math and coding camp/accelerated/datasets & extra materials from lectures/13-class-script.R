## libraries
library(tidyverse)
library(gapminder)
data(gapminder)

## What are functions?
sales <- c(13, 15, 17, 21, 25, 30, 45, 33, 25)

sum_of_sales <- function(input1, input2) {
  
  if (is.vector(input1)) {
    
    ## This is what the function does:
    # Sum all the values of the input
    total_sales <- sum(input1, na.rm = TRUE)
    
    # Print the result in a nice format
    cat('The total sales this week was:', total_sales, '-', input2)
    
  } else {
    
    ## Message to the user
    print('This function only works with vectors. Change your input!')
  }
  
}

## Case use 1:
sum_of_sales(sales)

## Case use 2:
sum_of_sales(c(1:100))

## Case use 3:
sum_of_sales(c(1:100, NA))

## Case use 4:
sum_of_sales(gapminder)

## Case use 5:
sum_of_sales(input1 = sales, input2 = 'amazing!!!')

## Getting all NAs for a given column of a given dataframe
all_nas <- function(df, col_name) {
  col_index <- which(colnames(df) == col_name)
  total_nas <- sum(is.na(df[[col_index]]))
  cat('Column', col_name, ',', 'of the', as.character(substitute(df)), 'dataset, has this many missing values:', total_nas)
  
}

library(gapminder)
data(gapminder)
all_nas(df = gapminder, col_name = 'pop')


