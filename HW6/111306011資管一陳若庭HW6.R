library(tidyverse)
data <- read.csv("ecommerce.csv")
## check your data1
str(data)
## descriptive statistics
summary(data)
## check the difference 
### test difference
data %>%
  group_by(landing_page) %>%
  summarise(mean_converted = mean(converted))

## Hypothesis Test:
### t-test for two sample mean
### 表示你想要檢驗新網頁的轉換率是否顯著大於舊網頁
t.test(data[data$landing_page == "old_page", ]$converted,
       data[data$landing_page == "new_page", ]$converted,
       alternative = "greater")
#### reject H0, if p-value is less than the significance level 0.05
#### p-value = 0.08518
#### 我們無法拒絕虛無假設（兩個群組的平均值相等）。
#### 也就是說，在 95% 的信心水準下，新網頁的轉換率似乎沒有顯著大於舊網頁



