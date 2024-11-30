# Short version of Code 5: Choice model and multinomial logit 
# This code is for HW3.
# For multiple choice decisions, Multinomial Logit/Multinomial Probit is available. 
# PLEASE DO NOT SHARE THIS DATA WITH ANYONE. 

######################################################
### Multinomial Logit model  
## 1. Import Data
setwd("~/Desktop/yonsei/3학년/인공지능 경영/hw3")
data <- read.csv("brand choice sample data for HW_6brands.csv")

head(data)  #checking the variable names and the first 10 observation

install.packages("data.table")
library(data.table)
# For categorical variables, you need to create dummy variables. 
# Note. If you want to make dummy variables easily, please use fastDummies package. 
library(fastDummies)
# Create dummy variables 
dummy_HH_AGE <- dummy_cols(data["HH_AGE"])
dummy_HH_EDU <- dummy_cols(data["HH_EDU"])
dummy_HH_OCC <- dummy_cols(data["HH_OCC"])
# There are many other categorical variables. Please make sure to make dummy variables before using them in the mlogit model.   
# Once you create dummy variables, please merge the dummy variables into the original data
data2 <- cbind(data, dummy_HH_AGE, dummy_HH_EDU, dummy_HH_OCC)
#check the variable names in the merged data (i.e., data2)
head(data2)

#Create data for mlogit 
library(mlogit)
data.mlogit <- mlogit.data(data2, choice="BrandChoice", shape="wide", id.var="PANEL_ID", time.var="Month", varying=32:45)
#dfidx can also deal with data /frames in wide format.   
# To check the first 10 observations 
head(data.mlogit)


## 3. Running Multinomial Logit Model 
# When you choose "no purchase" as a base option  
### General Effect of variables (e.g., AD)
fit.ml0 <- mlogit(BrandChoice ~ 1 + AD, data=data.mlogit) 
summary(fit.ml0)

### Brand-specific Effect: Each effect estimated by brand separately 
# If you want to estimate the effect by brand, put the variable name after | mark.
fit.ml1 <- mlogit(BrandChoice ~ 1 | AD + Price, data=data.mlogit)
summary(fit.ml1)
#If you want to add more variables, you can keep adding other variables (e.g., Price + AD + Number_of_TVs_Used_by_HH + HH_AGE_5,,,, etc.)
#[IMPORTANT] For categorical variables, please create dummy variables FIRST and then add some dummy variables you are interested in.
#For example, if you are interested in AGE 45-55 and AGE 56-64, then you can add HH_AGE_4 and HH_AGE_5 in the model and see if there are any difference in effect across brands. 
# For this data for HW3, HH_AGE_2 and HH_AGE_3 are not working. Hence, please do not put these two variables into your model. 
