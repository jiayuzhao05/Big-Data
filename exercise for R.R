library(dplyr)
library(nycflights123)

flights <- nycflights123::flights
head(flights)

# flights delay for two or more hours
flights %>%
  filter(arr_delay >= 120) %>%
  head()

# flights head to Husdon("IAH","HOU")
flights %>%
  filter(dest %in% c("IAH","HOU")) %>%
  head()

# flight co-operated by UA and AA and DL
flight %>%
  filter(carrier %in% c("UA", "AA", "DL")) %>%
  head()

# flight departed in summer
flights %>%
  filter(month %in% c(7,8,9))n %>%
  head()

# flight delay more than two hours but depart on time
flights %>%
  filter(arr_delay >120, dep_delay <= 0) %>%
  head()

# flight delay more than one hour but makeup more than 30 minutes on thee journey
flights %>%
  filter(dep_delay >= 60, dep_delay - arr_delay > 30) %>%
  head()

# find the flight delay fareset ever
flight %>%
  arrange(desc(dep_delay)) %>%
  head()

# find the flight depart in the early hours of the day
flights %>%
  filter(dep_time >= 0, dep_time <= 600) %>%
  head()

#Sort the flights to find the fastest one
flights %>%
  mutate(speed = distance / air_time) %>%
  arrange(desc(speed)) %>%
  head()

# does everyday in 2023 have flights?
flights %>%
  count(year, month, day) %>%
  nrow() == 365

# which flight distance far? near?
#The farthest flight
flights %>%
  arrange(desc(distance)) %>%
  head()

# the nearest flight
flights %>%
  arrange(distance) %>%
  head()

# if use filter() and arrange(), sequence important?
#filter-arrange
flights %>%
  filter(month == 1) %>%
  arrange(desc(dep_delay)) %>%
  head()

#arrange-filter
flights %>%
  arrange(desc(dep_delay)) %>%
  filter(month == 1) %>%
  head()


#Q2
# compare dep_time, sched_dep_time, dep_time, how corecolect?
# verify relationship
flights %>%
  mutate(calculated_delay = dep_time - sched_dep_time) %>%
  summarise(
    correlation = cor(dep_delay, calculated_delay, use = "complete.obs"),
    consistency = all(dep_delay == calculated_delay, na.rm = TRUE)
  )

# multiple ways to choose dep_time, dep_delay, arr_time in flight
# method 1:Specify the variable name
flights %>%
  select(dep_time, dep_delay, arr_time, arr_delay)

# method 2:use variable model
flightss %>%
  select(starts_with("dep"), starts_with("arr"))

# method 3: choose by Vector dynamics
vars <- c("dep_time", "dep_delay", "arr_time", "arr_delay")
flights %>%
  select(all_of(vars))

# any_of() fx function & advantage
vars <- c("dep_time", "arr_time", "nonexistent_column")
flights %>%
  select(any_of(vars))


flights %>%
  select(contains("TIME"))
# contains() 的默认设置是 ignore.case = TRUE，所以它会忽略大小写进行匹配
#如果想让匹配对大小写敏感，可以将 ignore.case 参数设置为 FALSE
flights %>%
  select(contains("TIME", ignore.case = FALSE))


flights <- flights %>%
  rename(air_time_min = air_time) %>%
  select(air_time_min, everything()) # 将 air_time_min 移动到数据框的第一列，everything() 保持其余列的顺序。


# Q3
# which aircraft delay time longest?
flights %>%
  group_by(carrier) %>%
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_delay))

# how to seoerate effects of bad airports and airlines
flights %>%
  group_by(carrier, dest) %>%
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE), n = n()) %>%
  arrange(desc(avg_delay))

#Find the most delayed flights for each destination
flights %>%
  group_by(dest) %>%
  slice_max(dep_delay, n = 1) %>%
  arrange(desc(dep_delay))

# the change about delay in one day using chart
library(ggplot2)

flights %>%
  mutate(hour = sched_dep_time %/% 100) %>%
  group_by(hour) %>%
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = avg_delay)) +
  geom_line() +
  labs(title = "Average Delay by Hour of Day", x = "Hour of Day", y = "Average Delay (minutes)")


flights %>%
  slice_min(dep_delay, n = -1)

