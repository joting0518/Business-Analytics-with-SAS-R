#1
library(tidyverse)
getwd()
product_list <- read.csv("product_list.csv", sep=",")
product_list<-product_list %>% separate(Item, into=c("Product", "Item"), sep = "_")
product_list$Product <- as.numeric(pl$Product)
#2
client_list <- read.csv("client_list.csv", sep=",")
salesdata <- read.csv("salesdata.csv", sep=",")
str(product_list)
str(client_list)
str(salesdata)

full.table<- client_list|>
  full_join(salesdata, by = join_by(Client))%>%
  full_join(product_list, by = join_by(Product))
multiple = "all"
full.table
#full.table
full.table<-subset(full.table, select = -X.y)
full.table<-subset(full.table, select = -X.x)
full.table
write.table(full.table, file = "/Users/chenruoting/Desktop/R/HW1/fulltable.csv",row.names = FALSE)
#3
full.table<-full.table %>%mutate(spend = UnitPrice*Quantity)

full.table
write.table(full.table, file = "/Users/chenruoting/Desktop/R/HW1/fulltable3.csv",row.names = FALSE)
#4
#group with diamond and gold
#group with silver and basic

client_data<-full.table%>%group_by(Client, Age, Membership, Gender)%>%
  summarise(across(c(spend), sum, na.rm = TRUE))
client_data

dg_data <- filter(client_data,Membership == 'diamond'|Membership == 'gold')%>%
      as.data.frame()
dg_data

normal_data <- filter(client_data,Membership == 'basic'|Membership == 'silver')%>%
  as.data.frame()
normal_data

#dg male number
gdmale_number <- dg_data %>% 
  filter(Gender == 'male')%>%
    summarise(total_count=n(), .groups = 'drop')

#n male number
n_male_number <- normal_data %>% 
  filter(Gender == 'male')%>%
  summarise(total_count=n(), .groups = 'drop')

#dg female number
gdfemale_number <- dg_data %>% 
  filter(Gender == 'female')%>%
  summarise(total_count=n(), .groups = 'drop')
#n female number
n_female_number <- normal_data %>% 
  filter(Gender == 'female')%>%
  summarise(total_count=n(), .groups = 'drop')

#dg data
gdmale_number
gdfemale_number
mean(dg_data$Age)
mean(dg_data$spend,na.rm = TRUE)

#n data
n_male_number
n_female_number
mean(normal_data$Age)
mean(normal_data$spend,na.rm = TRUE)

#5
client_data
male_analysis <- client_data %>%
  filter(Gender == 'male')

male_analysis
write.table(male_analysis, file = "/Users/chenruoting/Desktop/R/HW1/male_analysis.csv",row.names = FALSE)
mean(male_analysis$Age)
mean(male_analysis$spend)

male_product <- full.table%>%
  filter(Gender == 'male')
male_product
p <- ggplot(data = male_product, aes(x = Item, y = spend)) +
  geom_bar(stat = "identity")
p + ggtitle("Male's total spending on each product") +
  xlab("Product") + ylab("Total spend")
write.table(male_product, file = "/Users/chenruoting/Desktop/R/HW1/male_product.csv",row.names = FALSE)
