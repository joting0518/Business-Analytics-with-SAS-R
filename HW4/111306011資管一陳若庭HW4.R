##feature extraction
data <- read.csv("financialdata.csv")
data = data[,-1]
data##show data
str(data)
library(dplyr)
data[,11] <- gsub(",","", data[,11])
data[,14] <- gsub(",","", data[,14])
data[,15] <- gsub(",","", data[,15])
data <- mutate(data, op_profit_growth_rate = as.numeric(op_profit_growth_rate))
data <- mutate(data, current_ratio = as.numeric(current_ratio))
data <- mutate(data, quick_rartio = as.numeric(quick_rartio))

##M = cor(data)

# 執行PCA或SPCA
install.packages("stats")
library(stats)
pca<- prcomp(data, center = TRUE, scale = TRUE)
#center data with mean =0. scale=T means standard deviation is set 1.
names(pca) #sdev=std. deviation, rotation=coefficient主成份的係數矩陣
summary(pca)
#the first two components account for over 49.31% of 
#the variance in the entire data set.

plot(pca) #Variation	explained
#first component is the one with the highest variance 
#and second will be the second highest in variance

pca$rotation
library(reshape2)
ggplot(melt(pca$rotation[,1:5]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
