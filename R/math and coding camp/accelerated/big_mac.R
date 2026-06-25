# 1. load data 
library(tidyverse)
big_mac_raw <- read_csv("https://raw.githubusercontent.com/TheEconomist/big-mac-data/master/source-data/big-mac-source-data-v2.csv")

# 2. explore (notice local_price  and dollar_ex; date)
big_mac_raw
View(big_mac_raw)


# 3. We want a comparable unit: convert all to dollars
big_mac <- 
  big_mac_raw %>%
  mutate(dollar_price = local_price / dollar_ex,
         year = lubridate::year(date)) 

ggplot(data = big_mac) +
  geom_line(mapping = aes(x = date, y = dollar_price)) 

ggplot(data = big_mac) +
  geom_point(mapping = aes(x = date, y = dollar_price)) 


ggplot(data = big_mac) +
  geom_line(mapping = aes(x = date,
                          y = dollar_price, 
                          color = name),
            show.legend = FALSE
  ) 


# 4. convert dollars to a country-level index relative to 2008
big_mac <- 
  big_mac %>%
   group_by(name) %>%
   mutate(index = dollar_price - max((year == 2008)*dollar_price))

ggplot(data = big_mac) +
  geom_line(mapping = aes(x = date, y = index, group = name)) 


# 5. Oh no! Some countries don't have data going back to 2008.
big_mac %>%
  count(name) 

big_mac <- 
  big_mac %>%
  group_by(name) %>%
  filter(n() > 33)

ggplot(data = big_mac) +
  geom_line(mapping = aes(x = date, y = index, group = name)) 



# 6. Let's learn about specific countries

highlight_big_mac <- 
  big_mac %>% 
  filter(iso_a3 %in% c("USA", "GBR","DNK"))

ggplot(data = big_mac, aes(x = date, y = index)) +
  geom_line(mapping = aes(group = name),
            color = "grey")  +
  geom_line(mapping = aes(color = name),
            data = highlight_big_mac) +
  geom_hline(mapping = aes(yintercept = 0), 
             linetype = "dotted") +
  labs(color = NULL, 
       y = "Big Mac Index",
       x = NULL)



## Final Script 

library(tidyverse)
big_mac_raw <- read_csv("https://raw.githubusercontent.com/TheEconomist/big-mac-data/master/source-data/big-mac-source-data-v2.csv")

big_mac <- 
  big_mac_raw %>%
  mutate(dollar_price = local_price / dollar_ex,
         year = lubridate::year(date))  %>%
  group_by(name) %>%
  mutate(index = dollar_price - max((year == 2008)*dollar_price)) %>%
  filter(n() > 33)

highlight_big_mac <- 
  big_mac %>% 
  filter(iso_a3 %in% c("USA", "GBR", "MEX",
                       "CHN", "DNK", "RUS"))

ggplot(data = big_mac, 
       mapping = aes(x = date, y = index,
                     group = name)) + 
  geom_line(color = "grey",  size = .25) + 
  geom_line(data = highlight_big_mac,
            aes(color = name), size = 1) +
  geom_hline(mapping = aes(yintercept = 0), 
             linetype = "dotted") + 
  theme_minimal() +
  labs(color = NULL, 
       y = "Big Mac Index",
       x = NULL,
       title = "Dollar and Yuan have parallel trajectories")

# Big mac index background
# https://www.visualcapitalist.com/cp/big-mac-index-purchasing-power-parity-burger-inflation/



