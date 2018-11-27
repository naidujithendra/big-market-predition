rm(list = ls(all.names = TRUE))

#load the packages

library(readr)  

library(plyr)   #for mapvalues()

library(dplyr)  #for fast data manipulation(functions like mutate,select,arrange,filter,...)

library(ggplot2)#for data visualisation

library(VIM)    #for KNN imputation

#read the data

mar_train <- read.csv(file.choose())

mar_test <- read.csv(file.choose())

str(mar_train)  #mar_train is a dataframe with 8523 obs.11 variables

str(mar_test)  #mar_test is a data frame with 5681 obs.11 variables

#check the summary

summary(mar_train)

summary(mar_test)

#dim of dataset

dim(mar_train)

dim(mar_test)

## add another column(Item_outlet_Sales)(the dependent variable), to the test dataset.

mar_test$Item_Outlet_Sales <- NA

#now create a saperate dataset

mar_data <- rbind(mar_train,mar_test)

dim(mar_data)

#check the missing values 

summary(is.na(mar_data))

#SOME DATA VISUALISTION
# we will first plot and infer from the HISTOGRAMS of each continuous variable

hist(mar_data$Item_Weight)

hist(mar_data$Item_Visibility)

hist(mar_data$Item_Outlet_Sales)

library(ggplot2)

ggplot(mar_data, aes(Outlet_Identifier, Item_Weight)) + geom_boxplot()

# we can see that only the weights of items relted to OUTO19 and OUTO27 are missing.

ggplot(mar_data, aes(Item_Type, Item_Weight)) + geom_boxplot() 

# we can see that the weights all types of items are there. 

#similarly for,

ggplot(mar_data, aes(Item_Fat_Content, Item_Weight)) + geom_boxplot() 

boxplot(mar_data$Item_Visibility)

#THERE ARE SO MANY OUTLIERS FOR Item_Visibility VARIABLE which is also evident from the fact that its hist. was right skewed.

ggplot(mar_data, aes(Item_Type,Item_Visibility)) + geom_boxplot()

# thus the dots above each item type repesent outliers in item visibility corresponding to that particular item.similarly,

ggplot(mar_data, aes(Outlet_Identifier,Item_Visibility)) + geom_boxplot() 

# its no point to show the boxplot of Item_Visibility against the Item_Identifier.

ggplot(mar_data, aes(Item_Outlet_Sales,Item_Visibility,col = Item_Type)) + geom_point()  

ggplot(mar_data, aes(Item_Outlet_Sales,Item_Visibility,col = Outlet_Type)) + geom_point()

#IMPUTING THE MISSING VALUES FOR DATA SET USING KNN IMPUTATION
#FOR kNN IMPUTATION WE WILL REQUIRE "VIM" LIBRARY WHICH WE HAVE ALREADY IMPORTED
#imputing the missing values for Item_Weight.
#we can also see that Outlet_Size has observations like "_", SO WE WILL FIRST CONVERT THESE VALUES TO 'NA' VALUES SO THAT WE CAN USE,KNN IMPUTATION METHOD SINCE THIS METHOD IS ALSO APPLICABLE FOR CATEGORICAL VARIABLES..

mar_data[mar_data$Outlet_Size =="","Outlet_Size"] <- NA

summary(mar_data)# all the empty values were replaced by NA

imputdata1 <- mar_data

imputdata1 <- kNN(mar_data, variable = c("Item_Weight","Outlet_Size"), k = 90)

# k is generally choosen to be squareroot of number of observations.

summary(imputdata1)

ncol(imputdata1) #you will see there are two additional logical columns that got created we have to remove them

imputdata1 <- subset(imputdata1,select = Item_Identifier:Item_Outlet_Sales)

summary(imputdata1)

plot(imputdata1$Item_MRP,imputdata1$Item_Weight)

mar_data <- imputdata1

#ALSO ONE THING WHICH IS TO BE NOTED IS THAT THE VARIABLE Item_Fat_Content contains same observations with different names which need to be tackled::

mar_data$Item_Fat_Content <- mapvalues(mar_data$Item_Fat_Content, from = c("LF","Low Fat","low fat","Regular"), to = c("lf","lf","lf","reg"))

levels(mar_data$Item_Fat_Content)

#DETECTING OUTLIERS:

dataoutlier <- mar_data

bench <- 0.09459 + 1.5*IQR(mar_data$Item_Visibility)

# 0.09459 is the third quartile

bench

#value comes out to be 0.1959837

dataoutlier$Item_Visibility[dataoutlier$Item_Visibility > bench] <- bench

boxplot(dataoutlier$Item_Visibility)

# as we can see all the outliers have been removed

mar_data <- dataoutlier

#Adding new level in Item_Fat_Content "None".

levels(mar_data$Item_Fat_Content) <- c(levels(mar_data$Item_Fat_Content), "None")

## Based on Item_Type, for "health and Hygiene", "Household" and "Others",

## we will change the Item_Fat_Content factor to "None".

mar_data[which(mar_data$Item_Type=="Health and Hygiene"), ]$Item_Fat_Content = "None"

mar_data[which(mar_data$Item_Type=="Household"), ]$Item_Fat_Content = "None"

mar_data[which(mar_data$Item_Type=="Others"), ]$Item_Fat_Content = "None"

mar_data$Item_Fat_Content <- as.factor(mar_data$Item_Fat_Content)

table(mar_data$Item_Fat_Content) # Viewing the variable

# Since we are only concerned with how old the outlet is, and not the establishment year

mar_data$Outlet_Year <- 2018 - mar_data$Outlet_Establishment_Year

table(mar_data$Outlet_Year)

mar_data$Outlet_Year <- as.factor(mar_data$Outlet_Year)

## Visualizing Item_MRP with ggplot

library(ggplot2)

ggplot(mar_data, aes(Item_MRP)) + geom_density(adjust = 1/5)

## It is obvious that we would be better off by converting Item_MRP to Categorical variable

#creating a new feature:

#lets create a new feature named "price" which is a categorical variable..

mar_data$price <- "low"

mar_data$price[mar_data$Item_MRP >200] <-"high"

mar_data$price[mar_data$Item_MRP>70 & mar_data$Item_MRP <=200] <- "medium"

summary(mar_data)

#we can see a new variable named price is created with the property that,whenever,

#MRP <=70,  price =  "low"

#MRP >70 and MRP <= 200,  price = "medium"

#MRP >200,  price ="High"

## We are done with the data cleaning and feature engineering.

# Dividing data into train and test

train <- mar_data[1:8523, ]

test <- mar_data[8524:14204, ]

#now we are ready to build and use our predictive model which is simply #####linear regression#####.

#one thing to be noted is that Item_Identifier and outlet_Identifier clearly have no role in predicting Item_Outlet_Sales ,so they are not significant variables.

##lets develop our linear model. 

model1 <- lm(Item_Outlet_Sales~., data = train[-c(1,7,8)])
summary(model1)

model2 <- lm(log(Item_Outlet_Sales)~., data = train[-c(1,7,8)])

summary(model2)

model3 <- lm(sqrt(Item_Outlet_Sales)~., data = train[-c(1,7,8)])

summary(model3)

par(mfrow=c(1,1))

plot(model3)

#Lets check RMSE values

library(Metrics)

rmse(train$Item_Outlet_Sales, model1$fitted.values) 

#RMSE value is 1126.891 (model which is suffering from heteroskedasticity)

rmse(train$Item_Outlet_Sales, exp(model2$fitted.values))

#New RMSE value is 1105.367 Thats Awesome! we improved a lot.

#Prediction on test_dataset

#test$Item_Outlet_Sales <- predict(model2 , newdata = BMStest[-c(1,7,8,12)])

test2 <- test[c(1:11,13,14)]

options(warn = -1)

predicted <- predict(model2,newdata = test2)

test2$Item_Outlet_Sales <-exp(predicted)

Item_Identifier <- test$Item_Identifier

Outlet_Identifier <- test$Outlet_Identifier

output.df <- as.data.frame(Item_Identifier)

output.df$Outlet_Identifier <- Outlet_Identifier 

output.df$Item_Outlet_Sales <- exp(predicted)

library(car)

some(output.df)

library(Metrics)

# make actuals_predicteds dataframe.

actuals_preds <- data.frame(cbind(actuals= train$Item_Outlet_Sales, predicteds=predicted))

correlation_accuracy <- cor(actuals_preds)

head(actuals_preds)

DMwR::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)

write.csv(predicted,file = "predicted.csv")
