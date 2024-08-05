library(MASS)
library(klaR)
library(ICS)
library(ROCR)
library(boot)
library(ipred)
set.seed(4630)

# Reading in the data
hotels_raw = read.csv("hotel_bookings_raw.csv")

##### Data Cleaning #####
hotels_raw$NightsStayed <- hotels_raw$stays_in_weekend_nights + hotels_raw$stays_in_week_nights
hotels_raw <- subset(hotels_raw, NightsStayed != 0)
# Log Transform Response
hotels_raw$LogNightsStayed <- log(hotels_raw$NightsStayed)

# Histogram for log transformed response variable
hist(hotels_raw$LogNightsStayed, main="Histogram of NightsStayed", xlab="LogNightsStayed")

# Subsetting by relevant columns for regression question
hotels = hotels_raw[, c("LogNightsStayed", "adr", "adults", "children", 
                        "babies", "lead_time", 
                        "CPI_HOTELS", "FUEL_PRCS", "DIS_INC")]

# Convert variables into proper types
hotels$FUEL_PRCS = as.numeric(hotels$FUEL_PRCS)
hotels$CPI_HOTELS = as.numeric(hotels$CPI_HOTELS)
hotels$DIS_INC = as.numeric(hotels$DIS_INC)

# Remove all rows with NA values
hotels = na.omit(hotels)

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### Shrinkage Methods ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####


## Parts A and B to be added in other document

# Part C
x<-model.matrix(LogNightsStayed~.,data=hotels)[,-1]
y<-hotels$LogNightsStayed

# ridge regression
ridge.r<-glmnet::glmnet(x,y,alpha=0, lambda=0)

# compare with OLS
result<-lm(LogNightsStayed~.,data=hotels)
cbind(coefficients(result), coefficients(ridge.r))

# Adjust threshold
ridge.r<-glmnet::glmnet(x,y,alpha=0, lambda=0, thresh = 1e-19)
cbind(coefficients(result), coefficients(ridge.r))

sample.data<-sample.int(nrow(hotels), floor(.50*nrow(hotels)), replace = F)
x.train<-x[sample.data,]
x.test<-x[-sample.data,]
y.train<-y[sample.data]
y.test<-y[-sample.data]

# Part D
## i)
set.seed(4630)
cv.out<-glmnet::cv.glmnet(x.train,y.train,alpha=0, thresh = 1e-19, nfolds=10)
bestlam<-cv.out$lambda.min
bestlam

## ii)
plot(cv.out)

## iii)
# 8 predictors are left, because ridge regression keeps all predictors

# iv)
# All 8 predictors were left in the model, none removed

## v)
ridge.mod<-glmnet::glmnet(x.train,y.train,alpha=0,lambda=bestlam, thresh = 1e-19)
out.ridge<-glmnet::glmnet(x,y,alpha=0,lambda=bestlam,thresh = 1e-19)
out.ols<-glmnet::glmnet(x,y,alpha=0, lambda=0, thresh = 1e-19)
ridge.pred<-predict(ridge.mod,s=bestlam,newx=x.test)
mean((ridge.pred-y.test)^2) # Test MSE for Ridge Regression

# Part E
## i)
set.seed(4630)
cv.out.lasso<-glmnet::cv.glmnet(x.train,y.train,alpha=1, thresh = 1e-19)
plot(cv.out.lasso)
bestlam.lasso<-cv.out.lasso$lambda.min
bestlam.lasso

## ii)
plot(cv.out.lasso)

## iii)
lasso.mod<-glmnet::glmnet(x.train,y.train,alpha=1,lambda=bestlam.lasso,thresh = 1e-19)
out.lasso<-glmnet::glmnet(x,y,alpha=1,lambda=bestlam, thresh = 1e-19)

cbind(coefficients(out.lasso), coefficients(out.ridge), coefficients(out.ols))

# With lasso, 4 of the 8 predictors are 0

## iv)
# adr, adults, children, and lead_time are left in the model

## v)
lasso.pred<-predict(lasso.mod,s=bestlam.lasso,newx=x.test)
mean((lasso.pred-y.test)^2) # Test MSE for Lasso Regression

# Part F
##fit OLS by setting lambda=0
ridge.mod.0<-glmnet::glmnet(x.train,y.train,alpha=1,lambda=0, thresh = 1e-19)
lasso.mod.0<-glmnet::glmnet(x.train,y.train,alpha=1,lambda=0, thresh = 1e-19)

ridge.pred.0<-predict(ridge.mod.0,s=0,newx=x.test)
mean((ridge.pred.0 - y.test)^2) # OLS Test MSE

lasso.pred.0<-predict(lasso.mod.0,s=0,newx=x.test)
mean((lasso.pred.0-y.test)^2) # OLS Test MSE (done a second way, same result)

# Part G
# Conclusion
























