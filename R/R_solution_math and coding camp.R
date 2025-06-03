# Lecture solution

# page 16
# Q1
midwest$poverty_designation <- ifelse(midwest$percbelowpoverty > 10, "High Poverty", "Low Poverty") 
# Q2
midwest$ohio <- ifelse(midwest$state == "OH", "Ohio Counties", "Other Midwestern Counties") 

# page 17
# Q3
big_counties <- c("COOK", "WAYNE", "CUYAHOGA", "OAKLAND", "FRANKLIN")
midwest$populous_counties <- ifelse(midwest$county %in% big_counties, TRUE, FALSE)
# Q4
midwest$pop_index <- ifelse(midwest$poptotal, "High", ifelse(midwest$poptotal >= 30000 & midwest$poptotal <= 100000, "Medium", "Low"))

# First, create a new variable called ratio
housing_data$ratio <- housing_data$volume / housing_data$listings
# create new column 'r_level'
housing_data$r_level <- 0
housing_data$r_level[housing_data$ratio >= 10000] <- "H"
housing_data$r_level[housing_data$ratio >= 8000 & housing_data$ratio < 10000] <- "M"
housing_data$r_level[housing_data$ratio < 8000] <- "L"




aggregate(mean_price ~ city + year,
          housing_data,
          mean)

# answer to Q3
housing_data_new3 <- housing_data[housing_data$city %in% c("Arlington", "Bay Area", "Collin County") & housing_data$listings >= 3500, c("city", "year", "month", "listings")]



housing_data_q1$mean_price <- housing_data_q1$volume / housing_data_q1$sales


housing_data_q2$ref[housing_data_q2$mean_price > 100000] <- 1
housing_data_q2$ref[housing_data_q2$mean_price <= 100000] <- 0

# arrange | 排序 | 行 | arrange(df, var) 
# select | 选择列 | 列 | select(df, col1, col2)
# mutate | 创建 | 列 | mutate(df, var = expression)
# filter | 过滤 | 行 | filter(df, condition)
# ifelse | 条件 | 列 | ifelse(condition, value1, value2)
# case_when | 条件 | 列 | case_when(condition1 ~ value1, condition2 ~ value2, ...)
# aggregate | 聚合 | 列 | aggregate(var ~ group_var, df, function)
# nrow | 行数 | 行 | nrow(df)
# ncol | 列数 | 列 | ncol(df)
# mean | 平均值 | 列 | mean(df$var)

# answer to q1: use pipe operator
housing_data_newq1 <- 
  housing_data %>%
  filter(listings > 5500 & median >= 235000) %>%
  nrow()



housing_data_q3 <- 
  housing_data_q2 %>%
    mutate(level = case_when(  # 新增（或修改）一列 level，其值由右边的表达式决定
             mean_price >= 150000 ~ 1,
             mean_price < 150000 & mean_price >= 100000 ~ 0.5,
             mean_price < 100000 ~ 0
          ))


mutate(数据框, 新变量名 = 计算表达式, ...)

# output
# | x | y | z |
# |---|----|----|
# | 1 | 2 | 3 |
# | 2 | 4 | 6 |
# | 3 | 6 | 9 |
# | 4 | 8 | 12 |
# | 5 | 10 | 15 |

# tictoc::tic() 和 tictoc::toc()
# 启动计时器，记录代码开始执行的时间
# 停止计时器，输出循环执行所用的时间


big_alloc[[i]] <- input[[i]] * 10
# 对于每个索引 i，取出 input 的第 i 个元素，乘以 10，结果存到 big_alloc 的第 i 个位置

dta9_1 <-
  base::readRDS(url("https://slcladal.github.io/data/d03.rda","rb"))

tibble()


library(tibble)
tibble(
  x = 1:3,
  y = c("a", "b", "c")
)

# output:
# | x | y |
# |---|---|
# | 1 | a |
# | 2 | b |
# | 3 | c |


# 与 data.frame 的区别
# tibble 不会自动把字符串转成因子（factor）。
# 打印时只显示前几行和适当的列，避免刷屏。
# 支持列名为非标准名字（如带空格、特殊字符）。
# 更好地与 tidyverse 其他包配合。

nrow(dta9_1) # 行数
ncol(dta9_1) # 列数

plot(Variable2 ~ Variable1, # plot Variable2 by Variable1
     type = "p",            # plot type p (points) 
     data = dta9_1,         # data from dta9_1   
     ylab = "Variable2",    # add y-axis label 
     xlab = "Variable1",    # add x-axis label
     main = "Variable2 vs Variable1",        # add title 
     pch = 20               # use point symbol 20 (filled circles)
)


# 对数据框进行汇总，生成新的统计结果
# summarize(数据框, 新变量名 = 统计函数(原变量名), ...)

dta9_1 %>%
  summarize(mean_var1 = mean(Variable1, na.rm = TRUE),
            sd_var1 = sd(Variable1, na.rm = TRUE),
            mean_var2 = mean(Variable2, na.rm = TRUE),
            sd_var2 = sd(Variable2, na.rm = TRUE))



reg <- lm(Variable2 ~ Variable1, data = dta9_1)  # 拟合线性回归模型
#数据框 dta9_1 中，用 Variable1 作为自变量（解释变量、X），Variable2 作为因变量（被解释变量、Y），进行线性回归分析，并把回归结果保存到变量 reg 里。

# points(x, y, ...)：在当前的图形上添加点，x 是横坐标，y 是纵坐标
# 在图上用蓝色实心圆点，标出所有 Variable2 小于 predicted 的数据点
points(dta9_1$Variable1[dta9_1$Variable2 < predicted], # Variable2 小于 predicted 的那些行的 Variable1 作为横坐标
       dta9_1$Variable2[dta9_1$Variable2 < predicted], # Variable2 小于 predicted 的那些行的 Variable2 作为纵坐标
       pch = 19,  # 实心圆点
       col = "blue" 
)


# boxplot | 箱线图 | 一组/多组数据分布 | 比较分布、中位数、异常值
# plot | 散点图/线图等 | 两个变量的关系 | 变量关系、趋势、相关性

plot(x, y)
plot(y ~ x, data = df)


plot(Weight ~ Age,
     type = "b",
     pch = 15,
     cex = 1.5,
     data = weight_chart,
     ylim = c(2,10),
     xlab = "Age (months)",
     ylab = "Weight (kg)",
     main = "Weigh gain during early infant development"
)

boxplot(x)
boxplot(y ~ group, data = df)


boxplot(Temp ~ Month, 
        data = airquality,
        col = c("grey", "pink", "green", "purple", "blue")
)     


midwest %>%
  ggplot(aes(x = perchsd, y = percbelowpoverty)) +  # 设置 x 轴为 percbelowpoverty，y 轴为 perchsd
  geom_point(size = 1) +  # 设置点的大小为 1
  geom_smooth(method = "lm", se = FALSE) +  # 添加线性回归拟合曲线，se = FALSE 表示不显示置信区间
  facet_wrap(. ~ state) +  # 按 state 分组，每个州单独画一张小图
  labs(x = "% with hs diploma", y ="% below poverty line") +  # 设置 x 轴和 y 轴的标签
  ggtitle("Degree vs Poverty Rate") +  # 设置图表的标题
  theme(plot.title = element_text(hjust = 0.5)) #center the title


# 生成数据框
# | name | email |
# |-------|---------------------|
# | Alice | alice@company.com |
# | Bob | bob@company.com |
# | Carol | carol@company.com |
# | Dave | dave@company.com |
# | Eve | eve@company.com |


addr <- data.frame(name = c("Alice","Bob",
                            "Carol","Dave",
                            "Eve"),
                   email = c("alice@company.com",
                             "bob@company.com",
                             "carol@company.com",
                             "dave@company.com",
                             "eve@company.com"),
                   stringsAsFactors = FALSE)

# | 操作 | 结果包含的行 | 缺失信息如何显示 |
# |--------------|-----------------------------------|------------------|
# | left_join | 以左表（addr）为主，全部保留 | 右表缺失为 NA |
# | right_join | 以右表（phone）为主，全部保留 | 左表缺失为 NA |
# | inner_join | 只保留两表都出现的匹配行 | 都有，不会 NA |
# | full_join | 两表所有行都保留 | 缺失为 NA |

# Left join
addr %>% 
  left_join(phone, by = c("name" = "firstname"))

# Right join
addr %>% 
  right_join(phone, by = c("name" = "firstname"))

# Inner join
addr %>% 
  inner_join(phone, by = c("name" = "firstname"))

# Full join
addr %>% 
  full_join(phone, by = c("name" = "firstname"))




birth_years <- c(1944, 1950, 1981, 2016) # 创建一个向量 birth_years，包含 4 个家庭成员的出生年份
family_members <- double(length(birth_years))  # 创建一个和 birth_years 一样长的数值型向量 family_members，初始值都是 0，用来存放每个人的年龄

for(i in 1:length(family_members)) {                # Loop sequence: 从 1 到 family_members 的长度
  family_members[i] <- 2024 - birth_years[i]        # Loop body: 计算每个人的年龄
  cat("The age of family member", i, "is", family_members[i], fill = TRUE) # 打印每个人的年龄 fill = TRUE 表示每条信息换行显示
}

# output：
#   The age of family member 1 is 80
#   The age of family member 2 is 74
#   The age of family member 3 is 43
#   The age of family member 4 is 8



par(mfrow = c(2, 2))  # 设置图形布局为 2 行 2 列

# Create the sequence
selected.column <- c("poptotal", "percpovertyknown", "percollege", "percbelowpoverty") # 选择要绘制直方图的列

for (i in selected.column) {  # 遍历 selected.column 中的每个元素
  hist(midwest[[i]], # 取出 midwest 数据框中名为 i 的那一列
       main = paste("Histogram of", i), # 设置直方图的标题
       xlab = i) 
}

glimpse(msleep) # 以“宽格式”展示 msleep 数据集的每一列的名字、数据类型，以及前几行的内容
# 展示方式比 str() 更紧凑、易读，尤其适合有很多列的数据框

subset(msleep, select = c(name, genus) # Using subset() + c() to pick columns

# filter data by rows
# approach 1
subset(msleep, conservation == "domesticated")
# approach 2
msleep[msleep$conservation == "domesticated",]


# select() to pick columns
# filter() to pick rows that meet the criteria


# remove all rows with missing values using na.omit()
auto <- na.omit(auto)


wid_data_raw <- readxl::read_xlsx("world_wealth_inequality.xlsx",
                                 col_names = c("country", "indicator",
                                               "percentile", "year",
                                               "value")) %>%
  # 将 indicator 列拆分成三个新列：row_tag、type、notes
  separate(indicator, sep = "\\n", into = c("row_tag", "type", "notes"))



wid_data_raw <- readxl::read_xlsx("world_wealth_inequality.xlsx",
col_names = c("country", "indicator", "percentile", "year", "value"))


ggplot(diamonds) +
  geom_point(aes(x = price, y = carat),
             color = "black", fill = "lightblue") +
  ggtitle("Diamonds Price vs Carat by Cut") +
  scale_x_log10() + # 对 x 轴使用对数刻度
  facet_grid(. ~ cut) # 按 cut 分组，每个组画一张小图 横向排列


geom_col() # 直接用数据里的 y 值画柱状图
geom_bar() # 默认统计计数画柱状图

# pairs(数据框或矩阵)
# pairs(数据框[, c("变量1", "变量2", "变量3")])
pairs(iris[, 1:4]) #iris 数据集前四列（花萼长度、花萼宽度、花瓣长度、花瓣宽度）两两之间的散点图矩阵

college$Elite <- factor(ifelse(...))  # 根据某个条件，为 college 数据框新增（或覆盖）一个名为 Elite 的分类变量（factor），用来标记哪些学校是“精英”学校，哪些不是


radii <- c(0:10)
# using a loop
area <- double(length(radii))
for (i in seq_along(radii)) {  # 用循环依次计算每个半径对应的圆面积，公式是 πr2，结果存到 area 的每个位置
  area[[i]] <- pi * radii[[i]] ^ 2
}
area

# vectorized code is possibly faster, but definitely easier to read
# 向量化写法，直接对所有半径一次性计算面积，结果和上面循环一样
pi*radii^2

tictoc::tic()  # 启动计时器，记录代码开始执行的时间
for (i in seq_along(x)) {   # 用循环把向量 x 的每个元素乘以 10，结果存到 big 的每个位
  big[[i]] <- x[[i]]*10
}
tictoc::toc()  # 停止计时器，输出循环执行所用的时间

obs_sd <- sd(simulated_data)
z_score <- ...
z_score



make_mc_sample <- function(N, true_mean, B) {
  # pre-allocate output
  out <- vector("list", B)  # 创建一个长度为 B 的列表，用来存放每次模拟的结果
  for (i in 1:B) {  # 遍历 1 到 B 的每个整数
    out[[i]] <- calc_mean_and_sd_from_sample(N, true_mean)  # 调用 calc_mean_and_sd_from_sample 函数，生成一个包含均值和标准差的向量
  }
  # leave the following code unchanged
  bind_rows(out)
}



student_loan_debt <-
  read_xlsx("data/area_report_by_year.xlsx",
            sheet = "studentloan",
            skip = 3) %>%
  filter(state != "allUS") %>%
  pivot_longer(cols = -state,
               names_to = "year",
               values_to = "per_capita_student_debt") %>% 
  mutate(year = str_sub(year, 4, 7),
         year = as.numeric(year))
write_csv(student_loan_debt, "data/student_loan_debt.csv")



student_loan_debt %>% arrange(per_capita_student_debt)

summarize(student_loan_debt, latest_year = max(year), oldest_year = min(year))  
  
#Write a filter statement to get all states with an average per capita student debt of 10000 or higher in the year 2020 or higher

filter(student_loan_debt, per_capita_student_debt >= 10000, year >= 2020)

filter(student_loan_debt, state == "CA" & year >= 2013 | state == "IL" & year >= 2012)

filter(student_loan_debt, !is.na(per_capita_student_debt))



total_student_debt_data <-
  joined_data %>%
  mutate(total_student_debt = population * per_capita_student_debt,
         total_student_debt = round(total_student_debt/1e9, 1))



set.seed(60637)
estimates <- vector("list", 10)
n <- 1:10
for (i in n) {
  this_estimate <- mean(rnorm(n[i], mean = 0, sd = 5))
  estimates[[i]] <- c(n[i], this_estimate)
}
as_tibble(estimates)




# method 1
sample_sizes <- 1:50
estimates <- tibble(n = integer(), sample_mean = double())
set.seed(60637)
for (i in sample_sizes) {

  mean_of_sample <- mean(rnorm(i, 0, 5))

  this_estimate <- tibble(n = i, sample_mean = mean_of_sample)

  estimates <- bind_rows(estimates, this_estimate)
}

sample_sizes <- seq(1, 50000, 10)
estimates <- tibble(n = integer(), sample_mean = double())
tictoc::tic()
set.seed(60637)
for (i in sample_sizes) {
  
  mean_of_sample <- mean(rnorm(i, 0, 5))
  
  this_estimate <- tibble(n = i, sample_mean = mean_of_sample)
  
  estimates <- bind_rows(estimates, this_estimate)
}
tictoc::toc()



###redefining estimates in the end using a column vector
sample_sizes <- seq(1, 50000, 10)
mean_sample <- double(length(sample_sizes))
estimates <- tibble(n= integer(length(sample_sizes)), sample_mean = double(length(sample_sizes)))

set.seed(60637)
tictoc::tic()
for (i in seq_along(sample_sizes)) {
  
  mean_sample[[i]] <- mean(rnorm(sample_sizes[i], 0, 5))
}

estimates$n <- sample_sizes
estimates$sample_mean <- mean_sample

tictoc::toc()
estimates %>%
  ggplot(aes(x = n, y = sample_mean)) +
  geom_line()




# redefining estimates in the loop itself
sample_sizes <- seq(1, 50000, 10)
mean_sample <- double(length(sample_sizes))
estimates <- tibble(n= integer(length(sample_sizes)), sample_mean = double(length(sample_sizes)))

set.seed(60637)
tictoc::tic()
for (i in seq_along(sample_sizes)) {
  
  estimates[i, ] <- tibble(i, mean(rnorm(sample_sizes[i], 0, 5)))
}

tictoc::toc()
estimates %>%
  ggplot(aes(x = n, y = sample_mean)) +
  geom_line()


############


### estimates is a 2 dimensional vector 
set.seed(60637)
n <- seq(1, 50000, 10)
estimates <- array(1, c(length(n), 2))

colnames(estimates) <- c("n", "sample_mean")

tictoc::tic()
for (i in seq_along(n)) {
  this_estimate <- mean(rnorm(n[i], mean = 0, sd = 5))
  estimates[i, ] <- c(n[i], this_estimate)
}
estimates <- as_tibble(estimates)
tictoc::toc()
estimates %>%
  ggplot(aes(x = n, y = sample_mean)) +
  geom_line()





base_plot +
scale_color_manual(values = c("tomato", "blue", "green"))s