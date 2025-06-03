install.packages("tidyverse")
library(tidyverse)

install.packages("palmerpenguins")
library(palmerpenguins)
library(ggthemes)

install.packages("ggthemes")
library(ggthemes)

glimpse(penguins)
view(penguins)

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
)+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "flipper length(mm)", y = "Body mass(g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

# check rows
dim(penguins)

?penguins # description of bill_depth_mm

# create scatterplots for bill_depth_mm vs.bill_length_mm
ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  labs(
    title = "Bill depth vs. length",
    x = "Bill length(mm)",
    y = "Bill depth(mm)"
  )


# Scatter plots are not suitable for categorical variables
# a better choice is a box plot
ggplot(data = penguins,
       mapping = aes(x = species, y = bill_depth_mm)) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    title = "Bill depth vs length",
    x = "Species",
    y = "Bill depth (mm)",
    size = "Bill depth (mm)"
  )


# run the code
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point(alpha = 0.7) +  #advance the transparency
  geom_smooth(se = FALSE) +  #添加标准误差带 (se = TRUE)
  labs(
    title = "Penguin body mass vs. flipper length",
    substitle = "By island",
    x = "Flipper length(mm)",
    y = "Body mass(g)",
    color = "Island"
  ) +
  theme_minimal() #use simple theme 添加平滑趋势线


ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

penguins |>
  drop_na() |>  # 移除缺失值
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

ggplot(penguins, aes(x = species)) +
  geom_bar()

ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

ggplot(penguins, aes(x = island, fill = species)) +  # x轴是岛屿，填充色是物种
  geom_bar(position = "fill")  # position = "fill" 将每个条形标准化为相同高度（100%）
# 使用百分比刻度
scale_y_continuous(labels = scales::percent) 

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

#exercise 1.5.5
# 1
library(ggplot2)
data(mpg)

str(mpg)

head(mpg)

?mpg

summary(mpg)

glimpse(mpg)

#2
# scatterplots
ggplot(mpg, aes(x = displ, y = hwy, shape = class)) +
  geom_point()

# linewidth 
ggplot(mpg, aes(x = displ, y = hwy, linewidth = cyl)) +
  geom_point()
# linewidth 通常用于线条，对点的效果有限

ggplot(mpg, aes(x = displ, y = hwy, color = class, shape = class)) +
  geom_point()
# Provides redundant coding, which may enhance readability

# add colors on species
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point()

# facet_wrap on species
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point()+
  facet_wrap(~species)

#6.recover pattern
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm, 
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species", shape = "Species") # tags need to be the same unless it will show two different pic layers

#7 compare Stacked Bar Chart
# P1:show the proportion of species on the island
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
#P2:show the Distribution on different islands
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")

#save
ggsave(filename = "penguin-plot.png")

?function_name

# conflicted
install.packages("conflicted")
library(conflicted)
dplyr::filter(mtcars, cyl == 4)

mean(1:6)
## 3.5

mean(die)
## 3.5

round(mean(die))

sample(x = 1:4, size = 2)
sample(x = die, size = 1)

roll2 <- function(bones) {
  dice <- sample(bones, size = 2, replace = TRUE)  # 抽取两个数
  sum(dice)  # 返回和
}

roll2(bones = 1:4)


#simulate mis-measured data
#set seed
set.seed(10000)

#create a list of true values: 10 observations
true_values <- rnorm(10, mean = 5, sd = 2)

# introduce measurement error with mean 1 and standard deriviation 1
me_values <- rnorm(10, mean = 1, sd =1)
observed_values <- true_values + me_values
