# Removing All Environment Variables #
rm(list = ls(all = T ))
# Setting the working Directory #
setwd("C:\\Users\\Saiteja\\Desktop\\Inter data")

credit_data = read.csv("Final_credit")

#Fixing what appears to be a typo in the field header PAY_0
names(credit_data)[names(credit_data) == "PAY_0"] <- "PAY_1"  

names(credit_data)

#View(credit_data)
#reducing the levels in columns EDUCATION#

Vec = credit_data$EDUCATION

for (i in  seq(1:30000)) {
  if(Vec[i]>=4 ){
    Vec[i] = 4
  }
  
}
for (i in  seq(1:30000)) {
  if(Vec[i] == 0 ){
    Vec[i] = 1
  }
  
}
unique(Vec)
table(Vec)

credit_data$EDUCATION <- NULL
credit_data$EDUCATION = Vec 
# Importing required libraries
library(data.table)
library(dplyr)


# Changing default_payment column to a factor of 2 levels
#credit_data$default.payment.next.month = ifelse(credit_data$default.payment.next.month == 1,'Yes','No')

# Merging Education level 5 & 6 with 4
#credit_data$EDUCATION[credit_data$EDUCATION > 4] <- 4

## 1st Experimental Design
# Creating table for Chi2 test
credit_table <- table(credit_data$default.payment.next.month, credit_data$EDUCATION, dnn = c("Default", "Education_level"))

# Converting the table to matrix for easier manipulation
credit_table <- as.matrix(credit_table)

# Function for calculating Chi2 value
chi2_test <- function(mat){
  
  col_total <- apply(mat, 2, sum)
  row_total <- apply(mat, 1, sum)
  grand_total <- sum(row_total)
  
  exp_val <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  chi2_val <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  
  for (i in 1:nrow(mat)) {
    
    exp_val[i,] <- (col_total*row_total[i])/grand_total
    chi2_val[i,] <- ((mat[i,]-exp_val[i,])^2)/exp_val[i,]
  }
  
  chi2 <- sum(chi2_val)
  
  return(chi2)
  
}

# Calculating Chi2 value
chi2 <- chi2_test(credit_table)
chi2 #160.8265
# Performing Chi2 test using the built-in 'chisq.test' function
chisq.test(credit_table)

# Calculating Cramer's V
sample_number <- nrow(credit_data)
df <- (nrow(credit_table) - 1)

cramers_v <- sqrt(chi2 / (sample_number * df))
cramers_v #0.07321805
str(credit_data)
newd1 = credit_data
library(caret)
set.seed(12345)
Split1 <- createDataPartition(newd1$default.payment.next.month, p= 0.70, list= FALSE)
train_newd1 <- newd1[ Split1,]
test_newd1 <- newd1[-Split1,]

#### MODELLING ###

log1 <- glm(train_newd1$default.payment.next.month~.,data = train_newd1,family=binomial)

prob_train <- predict(log1,train_newd1,type = "response")

prob_test <- predict(log1,test_newd1,type = "response")

## Plotting the roc curve and calculating the auc
library(ROCR) 
pred <- prediction(prob_train, train_newd1$default.payment.next.month)
perf <- performance(pred, measure="tpr", x.measure="fpr")

#Plot the ROC curve using the performance measures (TPR and FPR)

plot(perf, main = "log1", col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
print(auc)

## Here fpr is imp so choosing the cutoff at 0.25

pred_class <- ifelse(prob_train>0.25,1,0)
pred_test <- ifelse(prob_test>0.25,1,0)
table(train_newd1$default.payment.next.month)

### Training metrics
table(train_newd1$default.payment.next.month,pred_class)
confusionMatrix(pred_class,train_newd1$default.payment.next.month,positive = "1")


### Testing metrics
table(test_newd1$default.payment.next.month,pred_test)
confusionMatrix(pred_test,test_newd1$default.payment.next.month,positive = "1")

## Decision Trees using CART ##
library(rpart)
dtCart=rpart(default.payment.next.month ~.,data=train_newd1,method="class")    
plot(dtCart,main="Classification Tree for engines",margin=0.15,uniform=TRUE)
text(dtCart,use.n=T)
summary(dtCart)


library(rpart.plot)
rpart.plot(dtCart,fallen.leaves = T)


###Model Evaluation- Error Metrics ###
a=table(train_newd1$default.payment.next.month, predict(dtCart, newdata=train_newd1, type="class"))
a
(a[2,2])/(a[2,1]+a[2,2])*100



#Starting with cp=0 and observing the tree 
dtCart=rpart(default.payment.next.month ~.,data=train_newd1,method="class", cp=0.001)
printcp(dtCart)

#Checking with different values of cp parameter
dtCart_final =rpart(default.payment.next.month~.,data=train_newd1,method="class",control = rpart.control(cp =0.0085616))    

plot(dtCart,main="Classification Tree ",margin=0.15,uniform=TRUE)



text(dtCart,use.n=T)
summary(dtCart)

a=table(train_newd1$default.payment.next.month, predict(dtCart, newdata=train_newd1, type="class"))
a
(a[2,2])/(a[2,1]+a[2,2])*100

a=table(test_newd1$default.payment.next.month, predict(dtCart, newdata=test_newd1, type="class"))
a
(a[2,2])/(a[2,1]+a[2,2])*100

## ANNOVA ##
dtCart=rpart(default.payment.next.month~.,data=train_newd1,method="anova")
plot(dtCart,main="Classification Tree for Engines",margin=0.15,uniform=TRUE)
text(dtCart,use.n=T)
summary(dtCart)

library(DMwR)
regr.eval(train_newd1$default.payment.next.month,predict(dtCart, train_newd1))

### Random Forest ##

library(randomForest)
Random_d1 <- randomForest( default.payment.next.month ~ ., data=train_newd1, keep.forest=TRUE, ntree=200,mtry=5)
print(Random_d1)
confusionMatrix(Random_d1$predicted,train_newd1$default.payment.next.month,positive = '1')
Random_d1$predicted
Random_d1$importance

str(Random_d1)
#### SVM ####

## Spliting training set into two parts for svm ##
set.seed(1234)
Split0 <- createDataPartition(newd1$default.payment.next.month, p=0.70, list= FALSE)
svm_train <- newd1[ Split0,]
svm_test <- newd1[-Split0,]

#svm_test_real
library(e1071)
tuned <- tune.svm(default.payment.next.month ~., data = svm_train, gamma = 10^(-6:-1), cost = 10^(1:2)) # tune
str(train_newd1)
summary (tuned) # to select best gamma and cost
testr = predict(tuned,svm_test)

svmfit <- svm (default.payment.next.month ~ ., data = svm_train, kernel = "radial", cost = 100, gamma=0.001) # radial svm, scaling turned OFF
print(svmfit)
#plot(svmfit, svm_train)
compareTable <- table (svm_test$Target, predict(svmfit, svm_test))  # comparison table
mean(svm_test$default.payment.next.month != predict(svmfit, svm_test)) # 13.79% misclassification error
confusionMatrix(svm_test,predict(svmfit, svm_test))##Improved 87.53 accuracy





