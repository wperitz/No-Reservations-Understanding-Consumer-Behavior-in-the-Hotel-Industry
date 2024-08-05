# read in data
hotels_raw = read.csv("/Users/andrewhunter/2023 Fall/STAT 4630/hotel_bookings_raw.csv")

# subset by relevant predictors and response variable
hotels = hotels_raw[, c("is_canceled", "adr", "arrival_date_week_number", 
                         "booking_changes", "days_in_waiting_list",
                         "lead_time", "previous_bookings_not_canceled", 
                         "previous_cancellations", "CPI_HOTELS", "FUEL_PRCS",
                         "DIS_INC", "is_repeated_guest", "deposit_type", "customer_type")]

# convert categorical variables to factors
hotels$is_canceled = factor(hotels$is_canceled)
hotels$is_repeated_guest = factor(hotels$is_repeated_guest)
hotels$deposit_type = factor(hotels$deposit_type)
hotels$customer_type = factor(hotels$customer_type)

# Convert numeric variables to proper type
hotels$FUEL_PRCS = as.numeric(hotels$FUEL_PRCS)
hotels$CPI_HOTELS = as.numeric(hotels$CPI_HOTELS)
hotels$DIS_INC = as.numeric(hotels$DIS_INC)

# Remove all rows with NA values
hotels = na.omit(hotels)

# split into training and testing data
sample.data<-sample.int(nrow(hotels), floor(.70*nrow(hotels)), replace = F)
train<-hotels[sample.data, ]
test<-hotels[-sample.data, ]

##store the response variable for test data.
##Use later to evaluate test error rate.
y.test<-test[,"is_canceled"]

# recursive binary splitting tree
tree.class.train<-tree::tree(is_canceled~., data=train)
summary(tree.class.train)
plot(tree.class.train)
text(tree.class.train, cex=0.6, pretty=0)

# test predictions
tree.pred.test<-predict(tree.class.train, newdata=test, type="class")

# confusion matrix
conf.mat = table(y.test, tree.pred.test)
conf.mat

# error rate
1 - mean(tree.pred.test==y.test)
1 - (conf.mat[1,1] + conf.mat[2,2]) / (conf.mat[1,1] + conf.mat[1,2] + conf.mat[2,1] + conf.mat[2,2])

# false positive rate
conf.mat[1,2] / (conf.mat[1,2] + conf.mat[1,1])

# false negative rate
conf.mat[2,1] / (conf.mat[2,1] + conf.mat[2,2])

# new threshold
pred.probs<-predict(tree.class.train, newdata=test)
conf.mat.new = table(y.test, pred.probs[,2]>0.40)
conf.mat.new
1 - (conf.mat.new[1,1] + conf.mat.new[2,2]) / (conf.mat.new[1,1] + conf.mat.new[1,2] + conf.mat.new[2,1] + conf.mat.new[2,2])

# false positive rate
conf.mat.new[1,2] / (conf.mat.new[1,2] + conf.mat.new[1,1])

# false negative rate
conf.mat.new[2,1] / (conf.mat.new[2,1] + conf.mat.new[2,2])

# pruned tree
set.seed(2)
cv.class<-tree::cv.tree(tree.class.train, K=10, FUN=prune.misclass)
trees.num.class<-cv.class$size[which.min(cv.class$dev)]
trees.num.class

##fit tree with size chosen by pruning
prune.class<-tree::prune.misclass(tree.class.train, best=trees.num.class)
plot(prune.class)
text(prune.class, cex=0.75, pretty=0)

# random forest
set.seed(2222)
rf.class<-randomForest::randomForest(is_canceled~., data=train, mtry=4, importance=TRUE)

#prediction on test data
pred.rf<-predict(rf.class, newdata=test)

##confusion matrix for test data
conf.mat.2 = table(y.test, pred.rf)
conf.mat.2

# test error
1 - mean(pred.rf==y.test)

# false positive rate
conf.mat.2[1,2] / (conf.mat.2[1,2] + conf.mat.2[1,1])

# false negative rate
conf.mat.2[2,1] / (conf.mat.2[2,1] + conf.mat.2[2,2])

# variable importance
randomForest::importance(rf.class)
randomForest::varImpPlot(rf.class)
