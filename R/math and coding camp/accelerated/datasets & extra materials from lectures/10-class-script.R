### data
pdat <- base::readRDS(url("https://slcladal.github.io/data/pvd.rda", "rb"))

### Plotting our data
plot(Prepositions ~ Date, # plot Prepositions by Date
     type = "p", # plot type "p" (points)
     data = pdat, # data from pdat
     ylab = "Prepositions (Frequency)", # add y-axis label
     xlab = "Date (year of composition)", # add x-axis label
     main = "Prepositions by Year")# add title

## Exercise 1: data
dta9_1 <- readRDS(url("https://slcladal.github.io/data/d03.rda","rb"))

## 2 - looking at our data
glimpse(dta9_1)

## 3 - mean and sd
summary(dta9_1)

avg_var1 <- mean(dta9_1$Variable1)
avg_var2 <- mean(dta9_1$Variable2)

sd_var1 <- sd(dta9_1$Variable1)
sd_var2 <- sd(dta9_1$Variable2)

## 4
reg <- lm(Variable2 ~ Variable1, data = dta9_1)

plot(Variable2 ~ Variable1,
     data = dta9_1,
     type = 'p')
abline(reg,
       'red')

## Frequency table
table(pdat$DateRedux)

## In-class exercise
library(gapminder)      
data(gapminder)      
      
### 1) Filter for a country of your choice
new_data <- gapminder %>% 
  filter(country == 'India')
      
### 2) Plot the lifeExp over time for your country using the plot function
plot(lifeExp ~ year,
     data = new_data,
     type = 'p')
  