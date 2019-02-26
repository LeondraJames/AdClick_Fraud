library(lubridate)
library(caret)
library(dplyr)
library(DMwR)
library(ROSE)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)
library(data.table)
library(e1071)
library(gridExtra)
library(knitr)
library(caTools)

train <-fread('C:/Users/leojames/Documents/Myself/HarvardX/train_sample.csv', stringsAsFactors = FALSE, data.table = FALSE)
test_valid <-fread('C:/Users/leojames/Documents/Myself/HarvardX/test.csv', stringsAsFactors = FALSE, data.table = FALSE)

#Explore
str(train)
str(test_valid)

head(train)
table(train$is_attributed)

#Missing Data?
colSums(is.na(train)) #none

##Data cleaning & feature engineering
train$attributed_time=NULL #remove attribute since it's not in test_valid set

#Reformat features
train$click_time<-as.POSIXct(train$click_time,
                             format = "%Y-%m-%d %H:%M",tz = "America/New_York")

train$year <- year(train$click_time)
train$month <- month(train$click_time)
train$days <- weekdays(train$click_time)
train$hour <- hour(train$click_time)

train$click_time=NULL #no longer needed after creating fields for each time type


#Number of unique observations per column
apply(train,2, function(x) length(unique(x)))

train$month=NULL #only 1 month present: feature no longer needed
train$year=NULL #only 1 year present: feature no longer needed

#Format appropriate columns as factors
train$is_attributed=as.factor(train$is_attributed)
train$days=as.factor(train$days)

##EDA & Visualization
#Application ID v. Downloads plots
p1 <- ggplot(train,aes(x=app,fill=is_attributed)) +
  geom_density()+
  facet_grid(is_attributed~.) +
  scale_x_continuous(breaks = c(0,50,100,200,300,400)) +
  ggtitle("Application ID v. Downloads - Density plot") +
  xlab("App ID") +
  labs(fill = "is_attributed") +
  theme_bw()


p2 <- ggplot(train,aes(x=is_attributed,y=app,fill=is_attributed) )+
  geom_violin() +
  ggtitle("Application ID v. Downloads - Violin plot") +
  xlab("App ID") +
  labs(fill = "is_attributed") +
  theme_bw()

p3 <- ggplot(train,aes(x=is_attributed,y=app,fill=is_attributed)) +
  geom_boxplot() +
  ggtitle("Application ID v. Downloads - Boxplot") +
  xlab("App ID") +
  labs(fill = "is_attributed") + 
  theme_bw()

#Show Application ID v. Downloads plots
grid.arrange(p1,p2,p3, nrow = 2, ncol = 2)


#OS Version v. Downloads plots
p4 <- ggplot(train,aes(x=is_attributed,y=os,fill=is_attributed)) +
  geom_boxplot() +
  ggtitle("OS Version v. Downloads - Boxplot") +
  xlab("OS version") +
  labs(fill = "is_attributed") +
  theme_bw()

p5 <- ggplot(train,aes(x=os,fill=is_attributed)) +
  geom_density()+facet_grid(is_attributed~.) +
  scale_x_continuous(breaks = c(0,50,100,200,300,400)) +
  ggtitle("OS Version v. Downloads - Density plot")+
  xlab("Os version") +
  labs(fill = "is_attributed") +
  theme_bw()

p6 <- ggplot(train,aes(x=is_attributed,y=os,fill=is_attributed)) +
  geom_violin() +
  ggtitle("OS Version v. Downloads - Violin plot") +
  xlab("Os version") +
  labs(fill = "is_attributed") +
  theme_bw()

#Show OS Version v. Downloads plots
grid.arrange(p4,p5,p6, nrow = 2, ncol = 2)

#Downloads v. IP Address plots
p7 <- ggplot(train,aes(x=is_attributed,y=ip,fill=is_attributed))+
  geom_boxplot()+
  ggtitle("Downloads v. IP Address - Boxplot")+
  xlab("Ip Adresss of click") +
  labs(fill = "is_attributed")+
  theme_bw()  

p8 <- ggplot(train,aes(x=ip,fill=is_attributed))+
  geom_density()+facet_grid(is_attributed~.)+
  scale_x_continuous(breaks = c(0,50,100,200,300,400))+
  ggtitle("Downloads v. IP Address - Density plot")+
  xlab("Ip Adresss of click") +
  labs(fill = "is_attributed")+
  theme_bw()


p9 <- ggplot(train,aes(x=is_attributed,y=ip,fill=is_attributed))+
  geom_violin()+
  ggtitle("Downloads v. IP Address <- Violin plot")+
  xlab("Ip Adresss of click") +
  labs(fill = "is_attributed")+
  theme_bw()

grid.arrange(p7,p8, p9, nrow=2,ncol=2)

#Likely an important predictor for our modeling#

#Downloaded v. Device Type plots

p10 <- ggplot(train,aes(x=device,fill=is_attributed))+
  geom_density()+facet_grid(is_attributed~.)+
  ggtitle("Downloaded v. Device Type - Density plot")+
  xlab("Device Type ID") +
  labs(fill = "is_attributed")+
  theme_bw()


p11 <- ggplot(train,aes(x=is_attributed,y=device,fill=is_attributed))+
  geom_boxplot()+
  ggtitle("Downloaded v. Device Type - Box plot")+
  xlab("Device Type ID") +
  labs(fill = "is_attributed")+
  theme_bw()


p12 <- ggplot(train,aes(x=is_attributed,y=device,fill=is_attributed))+
  geom_violin()+
  ggtitle("Downloaded v. Device Type - Violin plot")+
  xlab("Device Type ID") +
  labs(fill = "is_attributed")+
  theme_bw()

grid.arrange(p10,p11, p12, nrow=2,ncol=2)

#Dowloaded v Channel ID plots
p13<- ggplot(train,aes(x=channel,fill=is_attributed))+
  geom_density()+facet_grid(is_attributed~.)+
  ggtitle("Dowloaded v Channel ID - Density plot")+
  xlab("Channel of mobile") +
  labs(fill = "is_attributed")+
  theme_bw()


p14<- ggplot(train,aes(x=is_attributed,y=channel,fill=is_attributed))+
  geom_boxplot()+
  ggtitle("Dowloaded v Channel ID - Boxplot")+
  xlab("Channel of mobile") +
  labs(fill = "is_attributed")+
  theme_bw()

p15 <- ggplot(train,aes(x=is_attributed,y=channel,fill=is_attributed))+
  geom_violin()+
  ggtitle("Dowloaded v Channel ID - Violin plot")+
  xlab("Channel of mobile") +
  labs(fill = "is_attributed")+
  theme_bw()

grid.arrange(p13,p14, p15, nrow=2,ncol=2)

#Time of day anaysis
p16 <- ggplot(train,aes(x=hour,fill=is_attributed))+
  geom_density()+facet_grid(is_attributed~.)+
  ggtitle("Time v. Download - Density plot")+
  xlab("Hour") +
  labs(fill = "is_attributed")+
  theme_bw()

p17<- ggplot(train,aes(x=is_attributed,y=hour,fill=is_attributed))+
  geom_boxplot()+
  ggtitle("Time v. Download - Boxplot")+
  xlab("Hour") +
  labs(fill = "is_attributed")+
  theme_bw()

p18 <- ggplot(train,aes(x=is_attributed,y=channel,fill=is_attributed))+
  geom_violin()+
  ggtitle("Time v. Download - Violin plot")+
  xlab("Hour") +
  labs(fill = "is_attributed")+
  theme_bw()

grid.arrange(p16,p17, p18, nrow=2,ncol=2)

#Day of Week v. Downloads

p19 <- ggplot(train,aes(x=days,fill=is_attributed))+
  geom_density()+
  facet_grid(is_attributed~.)+
  ggtitle("Day of Week v. Downloads")+
  xlab("Os version") +
  labs(fill = "is_attributed")+
  theme_bw()


p20 <- ggplot(train,aes(x=days,fill=is_attributed))+
  geom_density(col=NA,alpha=0.35)+
  ggtitle("days v/s click")+
  xlab("Day of Week v. Downloads") +
  ylab("Total Count") +
  labs(fill = "is_attributed") +
  theme_bw()

grid.arrange(p19,p20, ncol=2)

##Modeling & Cross Validation
#Cross Validation
set.seed(1)
cv.10 <- createMultiFolds(train$is_attributed, k = 10, times = 10)
myControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                     index = cv.10)

#Decision Tree Model, using all features
set.seed(1)
dt <- caret::train(x = train[,-6], y = train[,6], method = "rpart", tuneLength = 30,
                   trControl = myControl)

pred <- predict(dt$finalModel,train, type = "class")
confusionMatrix(pred,train$is_attributed)

#Decision Tree #1 Performance
dt.misclass <- mean(train$is_attributed != pred)
dt.accuracy <- 1 - dt.misclass
dt.f1 <- F_meas(data = pred, reference = factor(train$is_attributed))

model.results <- data.frame(method = "Decision Tree: All Feat.", 
                            misclass = dt.misclass, 
                            accuracy = dt.accuracy,
                            f1_Score = dt.f1) #stores results

#Decision Tree Model, using select features only
#Remove below features with low differentiation
train$days=NULL
train$os=NULL
train$device=NULL

set.seed(1)
dt.2 <- caret::train(x = train[,-4], y = train[,4], method = "rpart", tuneLength = 30,
                    trControl = myControl)

pred.2 <- predict(dt.2$finalModel,data = train,type="class")
confusionMatrix(pred.2,train$is_attributed) #better specificity

#Decision Tree 2 Performance Accessment
dt.2.misclass <- mean(train$is_attributed != pred.2)
dt.2.accuracy <- 1 - dt.2.misclass
dt.2.f1 <- F_meas(data = pred.2, reference = factor(train$is_attributed))

model.results <- bind_rows(model.results, 
                           data.frame(method = "Decision Tree: Select Features",
                                      misclass = dt.2.misclass, 
                                      accuracy = dt.2.accuracy,
                                      f1_Score = dt.2.f1))#add results to table

#Since the results of both decision trees were similar, regard recall / specificity
spec <- specificity(pred, train$is_attributed, positive = "1")
spec2 <- specificity(pred.2, train$is_attributed, positive = "1")
data.frame("Specificity:Tree1" = spec, "Specificity:Tree2" = spec2)


#Data partitioning - train set
set.seed(1)
train_index <- createDataPartition(train$is_attributed,times=1,p=0.7,list=FALSE)
train <- train[train_index,]
test <- train[-train_index,]

#Rebalance the data since there is a low prevelance of downloads
set.seed(1)
smote.train = SMOTE(is_attributed ~ ., data  = train)   
table(smote.train$is_attributed)  


#Retry Decision Tree Model on select features, rebalanced data
#First, cross validation
set.seed(1)
cv.10 <- createMultiFolds(smote.train$is_attributed, k = 10, times = 10)
myControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                     index = cv.10)

#Decision Tree Model, select features + rebalanced data
set.seed(1)
dt.3 <- caret::train(x = smote.train[,-4], y = smote.train[,4], method = "rpart", tuneLength = 30,
                   trControl = myControl)

rpart.plot(dt.3$finalModel, extra = 3, fallen.leaves = F, tweak = 1.5, gap = 0, space = 0)

pred.3 <- predict(dt.3$finalModel,newdata=test,type="class")

#Performance Evaluation
confusionMatrix(pred.3,test$is_attributed)

dt.3.misclass <- mean(test$is_attributed != pred.3)
dt.3.accuracy <- 1 - dt.3.misclass
dt.3.f1 <- F_meas(data = pred.3, reference = factor(test$is_attributed))

model.results <- bind_rows(model.results, 
                           data.frame(method = "Decision Tree: + Rebalanced",
                                      misclass = dt.3.misclass, 
                                      accuracy = dt.3.accuracy,
                                      f1_Score = dt.3.f1))#add results to table


#Random Forest Model, select features + rebalanced data
set.seed(1)
cv.10 <- createMultiFolds(smote.train$is_attributed, k = 10, times = 10)
myControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                     index = cv.10)

set.seed(1)
set.seed(1)
rf<- caret::train(x = smote.train[,-4], y = smote.train[,4], method = "rf", tuneLength = 3,
             ntree = 100, trControl = myControl)

pred.rf <- predict(rf,newdata = test)

#Performance Evaluation

confusionMatrix(pred.rf,test$is_attributed)

dt.4.misclass <- mean(test$is_attributed != pred.rf)
dt.4.accuracy <- 1 - dt.4.misclass
dt.4.f1 <- F_meas(data = pred.rf, reference = factor(test$is_attributed))

model.results <- bind_rows(model.results, 
                           data.frame(method = "Random Forest",
                                      misclass = dt.4.misclass, 
                                      accuracy = dt.4.accuracy,
                                      f1_Score = dt.4.f1))#add results to table


#Linear Support Vector Machine (SVM)
set.seed(1)
svm.model <- tune.svm(is_attributed~.,data=smote.train, kernel="linear", cost=c(0.1,0.5,1,5,10,50))

best.linear.svm <- svm.model$best.model

pred.svm.lin <- predict(best.linear.svm,newdata=test,type="class")

#Performance Evaluation
confusionMatrix(pred.svm.lin,test$is_attributed)

dt.5.misclass <- mean(test$is_attributed != pred.svm.lin)
dt.5.accuracy <- 1 - dt.5.misclass
dt.5.f1 <- F_meas(data = pred.svm.lin, reference = factor(test$is_attributed))

model.results <- bind_rows(model.results, 
                           data.frame(method = "Linear Support Vector Machine",
                                      misclass = dt.5.misclass, 
                                      accuracy = dt.5.accuracy,
                                      f1_Score = dt.5.f1))#add results to table

#Radial Kernal Support Vector Machine (SVM)
set.seed(1)
svm.model.2 <- tune.svm(is_attributed~.,data=smote.train,kernel="radial",gamma=seq(0.1,5))
summary(svm.model.2)

best.radial.svm <- svm.model.2$best.model

pred.svm.rad <- predict(best.radial.svm,newdata = test)

#Performance Evaluation

confusionMatrix(pred.svm.rad,test$is_attributed)

dt.6.misclass <- mean(test$is_attributed != pred.svm.rad)
dt.6.accuracy <- 1 - dt.6.misclass
dt.6.f1 <- F_meas(data = pred.svm.rad, reference = factor(test$is_attributed))

model.results <- bind_rows(model.results, 
                           data.frame(method = "Radial Kernal Support Vector Machine",
                                      misclass = dt.6.misclass, 
                                      accuracy = dt.6.accuracy,
                                      f1_Score = dt.6.f1))#add results to table

#Final results
kable(model.results) #First 2 models were trained and tested on same (train) data. 
#The random forest was the best model that generalized to the test set. Now, let's validate on unseen data.




#Random Forest Validation 
#Prep full train dataset
new_train <-fread('C:/Users/leojames/Documents/Myself/HarvardX/train_sample.csv', 
                  stringsAsFactors = FALSE, 
                  data.table = FALSE)

new_train$attributed_time=NULL #remove attribute since it's not in test_valid set
new_train$click_time<-as.POSIXct(new_train$click_time,
                             format = "%Y-%m-%d %H:%M",tz = "America/New_York")
new_train$hour <- hour(new_train$click_time)
new_train$click_time=NULL 
new_train$os=NULL
new_train$device=NULL
new_train$is_attributed=as.factor(new_train$is_attributed)

#Now, the new train set and the original are identical. Time to rebalance new_train.
smote.train.full <- SMOTE(is_attributed ~ ., data  = new_train)   
table(smote.train.full$is_attributed) 

#ROC Curve
colAUC(smote.train[,-4],smote.train[,4], plotROC = TRUE)


#Prep validation dataset
test_valid$click_time<-as.POSIXct(test_valid$click_time,
                             format = "%Y-%m-%d %H:%M",tz = "America/New_York")

test_valid$hour <- hour(test_valid$click_time)

test_valid$click_time=NULL
test_valid$click_id=NULL
test_valid$os=NULL
test_valid$device=NULL


#Cross Validation
cv.10 <- createMultiFolds(smote.train.full$is_attributed, k = 10, times = 10)
myControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                          index = cv.10)

#Model Random Forest on full train set
set.seed(1)
set.seed(1)
rf.valid<- caret::train(x = smote.train.full[,-4], y = smote.train.full[,4], method = "rf", tuneLength = 3,
                  ntree = 100, trControl = myControl)

pred.valid <- predict(rf.valid,newdata = test_valid)

test_valid$is_attributed <- pred.valid #add results to test_valid set

#Performance Evaluation

confusionMatrix(pred.valid,test_valid$is_attributed)

dt.7.misclass <- mean(test_valid$is_attributed != pred.valid)
dt.7.accuracy <- 1 - dt.7.misclass
dt.7.f1 <- F_meas(data = pred.valid, reference = factor(test_valid$is_attributed))
