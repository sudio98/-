setwd("~/Desktop/yonsei/3학년/인공지능 경영/5주차")
data_cc <- read.csv("HW2.csv")
dim(data_cc)
summary(data_cc)
#1
library(dplyr)
data_cc_num <- data_cc %>% select(where(is.numeric))
table(data_cc$Attrition_Flag) 
data_cc$Attrition_Flag_Dummy <- as.numeric(data_cc$Attrition_Flag == "Attrited Customer")
cor(data_cc_num, data_cc$Attrition_Flag_Dummy)
library(ggplot2)
#barplot
ggplot(data_cc) + geom_bar(aes(x=Contacts_Count_12_mon , fill=Attrition_Flag)) + ggtitle("Barplot") + xlab("Contacts Count") + ylab("No. of Customers")
ggplot(data_cc) + geom_bar(aes(x=Months_Inactive_12_mon, fill=Attrition_Flag)) + ggtitle("Barplot") + xlab("No. of Inactive month") + ylab("No. of Customers")
ggplot(data_cc) + geom_bar(aes(x=Total_Trans_Ct , fill=Attrition_Flag)) + ggtitle("Barplot") + xlab("Total transaction Count") + ylab("No. of Customers")

#boxplot
ggplot(data_cc, aes(x=Attrition_Flag, y=Contacts_Count_12_mon)) + geom_boxplot() + ggtitle("Boxplot") + xlab("Attrition") + ylab("Contacts Count")
ggplot(data_cc, aes(x=Attrition_Flag, y=Months_Inactive_12_mon)) + geom_boxplot() + ggtitle("Boxplot") + xlab("Attrition") + ylab("No. of Month Inactive ")
ggplot(data_cc, aes(x=Attrition_Flag, y=Total_Trans_Ct)) + geom_boxplot() + ggtitle("Boxplot") + xlab("Attrition") + ylab("Total Transaction Count")

#2
# Logistic Regression
fit.lg <- glm(data_cc$Attrition_Flag_Dummy ~ data_cc$Total_Relationship_Count + data_cc$Months_Inactive_12_mon + data_cc$Credit_Limit + data_cc$Avg_Open_To_Buy + data_cc$Total_Trans_Ct + data_cc$Customer_Age + factor(data_cc$Gender) + factor(data_cc$Income_Category))
summary(fit.lg)
exp(fit.lg$coefficients)
fit.lg2 <- glm(Attrition_Flag_Dummy ~ data_cc$Months_on_book + data_cc$Avg_Open_To_Buy + data_cc$Total_Trans_Ct + data_cc$Total_Amt_Chng_Q4_Q1 + data_cc$Customer_Age + data_cc$Total_Revolving_Bal + factor(data_cc$Gender) + factor(data_cc$Income_Category), data=data_cc, family=binomial(link='logit'))
summary(fit.lg2)
exp(fit.lg2$coefficients)


#4
library(cluster)
data_cc_cl <- data_cc[,c("Months_on_book","Total_Relationship_Count", "Credit_Limit","Avg_Open_To_Buy","Total_Trans_Ct","Months_Inactive_12_mon")]
rownames(data_cc_cl) <- data_cc_cl$CLIENTNUM
w = c(); for (i in 1:10) w[i] = sum(kmeans(data_cc_cl,center=i)$withinss) # i = the number of clusters
plot(1:10, w, type='b', xlab="k: Number of cluster", ylab = "within group sum of squares", main = "Elbow Point Plot")
cluster.km <- kmeans(data_cc_cl, 4)
print(cluster.km)
data_cc_cl$cl_km <- cluster.km$cluster
table(data_cc_cl$cl_km)
cluster.km$centers
clusplot(data_cc_cl, data_cc_cl$cl_km, lines = 0, color=TRUE, shade=TRUE)
