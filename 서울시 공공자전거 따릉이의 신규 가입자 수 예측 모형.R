setwd("~/Desktop/yonsei/3학년/인공지능 경영/hw1")
data_bicycle <- read.csv("CopyOfHW1_data_shared.csv")
#1
dim(data_bicycle)
summary(data_bicycle$new_join) 
sd(data_bicycle$new_join) 
#2
data_bicycle_numeric <- data_bicycle[,-1]
cor(data_bicycle_numeric, data_bicycle$new_join)
#3
plot(data_bicycle$new_join, data_bicycle$duration_m, main = "Scatterplot", xlab = "New join", ylab = "Duration_m")
boxplot(data_bicycle$duration_m ~ data_bicycle$new_join)
#4
data_bicycle$lag_rent_num <- lag(data_bicycle$rent_num)
data_bicycle$usage_growth_rate <- (data_bicycle$rent_num - lag(data_bicycle$rent_num)) / lag(data_bicycle$rent_num) * 100
#5
fit.lm1 <- lm(formula = new_join ~ ave_temp + weekend + new_bike_daily + ave_windspeed + precipi + transpor + social + duration_m, data = data_bicycle)
summary(fit.lm1)
fit.lm2 <- lm(formula = new_join ~ rent_num + ave_temp + ave_windspeed + precipi + weekend + mean_duration_m + valence_total + social, data = data_bicycle)
summary(fit.lm2)