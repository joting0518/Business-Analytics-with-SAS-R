
library(readv)
library(tidyverse)
new_product_df <-
  seperate(item, into=c("Produce","Item"),="_")

names(sales_df)
names(new_product_df)
names(client_df)

str(sales_df$Product)
str()
str()

class(sales_df$client)
class()
#2
full_table <- sales_df inner()
#3
full_table <-
  full_table %>%
    mutate(spend = UnitPrice*Quantity)
#4

Female <-
  full_table %>%
  filter(Gender=="female")

group by