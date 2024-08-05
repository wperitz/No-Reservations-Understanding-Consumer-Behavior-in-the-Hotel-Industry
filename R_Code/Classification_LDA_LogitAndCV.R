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
# Subset by relevant columns for LDA model
hotels = hotels_raw[, c("is_canceled", "adr", "arrival_date_week_number", 
                        "booking_changes", "days_in_waiting_list",
                        "lead_time", "previous_bookings_not_canceled", 
                        "previous_cancellations", "CPI_HOTELS", "FUEL_PRCS",
                        "DIS_INC")]

# Convert variables into proper types
hotels$FUEL_PRCS = as.numeric(hotels$FUEL_PRCS)
hotels$CPI_HOTELS = as.numeric(hotels$CPI_HOTELS)
hotels$DIS_INC = as.numeric(hotels$DIS_INC)
hotels$is_canceled <- as.factor(hotels$is_canceled)

# Remove all rows with NA values
hotels = na.omit(hotels)


##### Split into training and test data #####
sample.data<-sample.int(nrow(hotels), floor(.70*nrow(hotels)), replace = F)
train<-hotels[sample.data, ]
test<-hotels[-sample.data, ]


##### Assess MVN assumption for LDA #####
# MVN testing
cancelled<-train[which(train$is_canceled==1),]
not_cancelled<-train[which(train$is_canceled==0),]

##MVN tests for cancelled
ICS::mvnorm.kur.test(cancelled[,2:11])
ICS::mvnorm.skew.test(cancelled[,2:11])

##MVN tests for not cancelled
ICS::mvnorm.kur.test(not_cancelled[,2:11])
ICS::mvnorm.skew.test(not_cancelled[,2:11])

# Note: the assumption that the predictors follow a multi-variate normal 
# distribution for each class of the response variable is not met, so our
# future interpretation may not be reliable


##### Model Building: LDA #####

# 0 means "not canceled," 1 means "canceled" 
contrasts(train$is_canceled)

## Create LDA with train data
lda.hotels <- MASS::lda(is_canceled ~ ., data=train)
lda.hotels

##### Model Building: Logistic Regression #####

result_train<-glm(is_canceled ~ ., family=binomial, data=train)
summary(result_train)

##### Comparing Performance #####

# ROC curve and AUC for Logistic Regression
preds.logistic<-predict(result_train,newdata=test, type="response")
rates.logistic<-ROCR::prediction(preds.logistic, test$is_canceled)
roc_result.logistic<-ROCR::performance(rates.logistic,measure="tpr", x.measure="fpr")

plot(roc_result.logistic, main="Logistic Regression ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

auc.logistic<-ROCR::performance(rates.logistic, measure = "auc")
auc.logistic@y.values


# ROC curve and AUC for LDA
lda.test <- predict(lda.hotels,test)
preds.lda<-lda.test$posterior[,2]
rates.lda<-ROCR::prediction(preds.lda, test$is_canceled)
roc_result.lda<-ROCR::performance(rates.lda,measure="tpr", x.measure="fpr")

plot(roc_result.lda, main="LDA ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

auc.lda<-ROCR::performance(rates.lda, measure = "auc")
auc.lda@y.values

### K-fold CV for Logistic
# 5 fold
logistic.five.fold<-boot::cv.glm(test,result_train, K=5)
logistic.five.fold$delta

# 10 fold
logistic.ten.fold<-boot::cv.glm(test,result_train, K=10)
logistic.ten.fold$delta

### K-fold CV for LDA
cv.da <- function(object, newdata)
{
  return(predict(object, newdata = newdata)$class)
}
# 5 fold
ipred::errorest(is_canceled ~ ., data=test, model=lda,
               estimator="cv",
               est.para=control.errorest(k=5),
               predict=cv.da)$err

# 10 fold
ipred::errorest(is_canceled ~ ., data=test, model=lda,
                estimator="cv",
                est.para=control.errorest(k=10),
                predict=cv.da)$err

### Actual error rate
# Logistic
confusion.mat.logistic<-table(test$is_canceled,preds.logistic > 0.5)
confusion.mat.logistic

# LDA
confusion.mat.lda = table(test$is_canceled,lda.test$class)
confusion.mat.lda









