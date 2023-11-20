install.packages("randomForest")
library(randomForest)
#read data
data <- read.csv("airline_survey.csv", na.strings = c(""))
data <- na.omit(data)
#--------------------------------------------#
#means1: change satisfaction into factor
#data$satisfaction <- factor(data$satisfaction,
                            #levels = c("satisfied","neutral or dissatisfied"),
                            #labels = c(0,1)
                            #)
#summary(data$satisfaction)
#means2: 將variable轉換為因子變數
data$Customer.Type <- as.numeric(ifelse(data$Customer.Type=="Loyal Customer", 0,1)) 
data$satisfaction <- factor(ifelse(data$satisfaction=="satisfied", 1,2)) 
data$Gender <- as.numeric(ifelse(data$Gender=="Male", 0,1))
data$Type.of.Travel <- as.numeric(ifelse(data$Type.of.Travel=="Personal Travel", 0,1))
data$Class <- as.numeric(ifelse(data$Class=="Business", 0,ifelse(data$Class=="Eco Plus",1,2)))


# 確認變數的轉換是否成功
table(data$satisfaction)
#--------------------------------------------#
set.seed(123)
data_short <- head(data, 5000)
data_short <- data_short[ -c(1,1:2) ]
data_short
rf_model <- randomForest(satisfaction ~ ., data = data_short, importance = TRUE)
print(rf_model)
prediction <- predict(rf_model, data_short)
prediction
#--------------------------------------------#
#accuracy <- mean(prediction == data_short$satisfaction)
#print(paste0("Accuracy: ", accuracy))
importance <- importance(rf_model)
print(importance)
varImpPlot(rf_model, sort=TRUE)
#挑選MeanDecreaseGini>100的variable
#--------------------------------------------#
set.seed(123)
# 檢查是否有缺失值
#sum(is.na(data_short[1:200,-23])) 
library(cluster)
#imp>100
customer_data <- data_short[, c("Type.of.Travel","Class","Flight.Distance","Inflight.wifi.service"
                                ,"Online.boarding","Seat.comfort","Inflight.entertainment","satisfaction")]
customer_data
# 移除缺失值
#data_short[1:200,-23] <- na.omit(data_short[1:200,-23])
#complete_cases <- complete.cases(data_short[1:200,-23])
#print(complete_cases)
# 尋找最佳的 k 值
library(factoextra)
#尋找聚類分析中最佳的聚類數量aka decide k value, and it uses elbow method
fviz_nbclust(customer_data[,-8], kmeans, method = "wss", k.max=5)
str(customer_data)
# 進行 k-means 分群
km <- kmeans(scale(customer_data[,-8]),2)
# 加入分群結果到資料框中
cluster_results <- cbind(customer_data, cluster = km$cluster)
# 印出前幾筆觀察結果
head(cluster_results)

# 繪製分群結果
#fviz_cluster視覺化聚類分析的結果ex:散點圖或熱圖
fviz_cluster(km, customer_data[,-8],
             palette = c("#00AFBB","#E78800"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
)

cluster_results$satisfaction <- as.numeric(cluster_results$satisfaction) 
cluster1 <- cluster_results[cluster_results$cluster==1,]
cluster2 <- cluster_results[cluster_results$cluster==2,]
mean1 <- colMeans(cluster1)
mean2 <- colMeans(cluster2)
mean1
mean2
# 標準化資料Z
#customer_datae <- scale(customer_data)

# 將資料分為5個群組
#set.seed(123)
#kmeans_model <- kmeans(customer_data, centers = 5)

# 將分群結果加入原始資料
#data_short$cluster <- kmeans_model$cluster
#data_short$cluster
#cc = data_short$cluster

#將分群結果做一個新的dataframe，比較男女...







