#Machine Learning for Research
install.packages("rpart.plot")
install.packages("rattle")
install.packages("caret")
install.packages("gbm")
install.packages("rio")
install.packages("data.table_1.10.5.zip",repos=NULL)
install.packages("e1071")
install.packages("C50")
install.packages("LiblineaR")
install.packages("kernlab")
install.packages("Ranger")
install.packages("xgboost")
install.packages("readxl")
install.packages("magrittr")
install.packages("varhandle")
install.packages("pROC")

remove.packages("data.table")
install.packages("data.table", type = "source",
                 repos = "http://Rdatatable.github.io/data.table")

require(data.table)
test.data.table()

library(pROC)
library(varhandle)
library(magrittr)
library(readxl)
library(kernlab)
library(LiblineaR)
library(C50)
library(e1071)
library(rio)
library(effects)
library(plyr)
library(dplyr)
library(ggplot2)
library(car)
library(caret)
library(gbm)
library(data.table)
library(rattle) # see: https://zhiyzuo.github.io/installation-rattle/
library(rpart.plot)
library(data.table)
library(xgboost)
library(ranger)

setwd("E:/Fellows/Data")
#------------------# 
#     Options      #
#------------------#

numrows <-6628
numcolumns <-100

#fread(inputFileName, header=TRUE, sep=",", nrows=numrows)[,1:numcolumns]

#------------------# 
#    Prep File     #
#------------------#


#----Amazon File---#

AmazonFile <- "Full Amazon Data.csv"
Amazon_data <- fread(AmazonFile, header=TRUE, sep=",")
str(Amazon_data)
colnames(Amazon_data)

#Filter down to columns we are interested in
Amazon_data_filtered <- Amazon_data %>% select(ReviewVerifiedPurchase,ReviewBadges,
                                               ReviewVineVoice,
                                               ReviewManufacturerComment) 
#Replace NA with 0
Amazon_data_filtered[is.na(Amazon_data_filtered)]<-0

#Factor necessary columns
Amazon_data_filtered$ReviewVerifiedPurchase%<>% factor
Amazon_data_filtered$ReviewVineVoice%<>% factor
Amazon_data_filtered$ReviewBadges%<>% factor
Amazon_data_filtered$ReviewManufacturerComment%<>% factor
str(Amazon_data_filtered)

#Combine with other necessary Amazon columns
other_Amazon_cols<-Amazon_data %>% select(ProductTitle,ProductID,
                                          ReviewHelpfulCount,ReviewCommentCount)
str(other_Amazon_cols)
to_be_merged<-cbind(other_Amazon_cols,Amazon_data_filtered)
dim(to_be_merged)




#--Review N-gram File--#
trigger = TRUE

if(trigger){
  remove('dt.merge','dt.merge.filt','dt.clean')
}

inputFileName <- "3dprinters.csv"
dat_pre <- fread(inputFileName, header=TRUE, sep=",", nrows=numrows)[,1:numcolumns]
dim(dat_pre)
table(dat_pre$Class)

# Factor the classes
dat_pre$Class <- factor(dat_pre$Class)

# Set up DV column
names(dat_pre)[grep("Class", names(dat_pre))] <- "DV"
dat_pre$DV <- factor(dat_pre$DV)
table(dat_pre$DV)

# Factor all data columns
dat_pre[, (names(dat_pre)):= lapply(.SD, factor), .SDcols=names(dat_pre)]
str(dat_pre)

# Remove any columns with singular factor
usefulCols <- names(which(sapply(dat_pre[, -c("DV"), with=FALSE], function (x) length(levels(x))) == 2))
dt.clean <- dat_pre[, c("DV", usefulCols), with = FALSE]
dim(dat_pre)
str(dat_pre)

# Rename columns to IVx
names(dat_pre)
backupNames <- names(dat_pre)
names(dat_pre)[1] # Make sure this is DV
names(dat_pre)[2:ncol(dat_pre)] <- paste("IV", 1:(ncol(dat_pre)-1), sep = "")
names(dat_pre)


#--Merge Amazon N-grams w/ General Data--#
dt.merge <- cbind(to_be_merged,dat_pre)


# Take out printers that do not fit criteria
dt.merge.filt <-dt.merge %>% filter(
  ProductID != "B075PFB7NX" || ProductID != "B01MUADN1Q" || ProductID != "B00YEV1A46" ||
    ProductID != "B071NZSF33" || ProductID != "B01MSN953A" || ProductID != "B073SV8CDH"
  || ProductID != "B01N5D2ZIB" || ProductID != "B074KYN3P8" || Product != "B0716C9RZG" ||
    ProductID != "B072C5QHPQ" || ProductID != "B074QLQSQV" || ProductID != "B017LIYSP6" ||
    ProductID != "B071SD7B4Z" || ProductID != "B071JMB4ZG" || ProductID != "B01LXDU5CW" ||
    ProductID != "B01K6RADUI" || ProductID != "B01M4OCVZG" || ProductID != "B01MYO02BA" ||
    ProductID != "B01IXVXV9Y" || ProductID != "B01EWGJAS0" || ProductID != "B01KD6T7Z4")

str(dt.merge.filt)

# Narrow to 5-star and 1-star reviews
dt.merge.filt$DV <-unfactor(dt.merge.filt$DV)
dt.clean <- dt.merge.filt[dt.merge.filt$DV == 5 | dt.merge.filt$DV == 1,]
dt.clean$DV <-factor(dt.clean$DV)


#Drop ProductID and ProductTitle Columns prior to ML
drops <- c("ProductID","ProductTitle")
dt.clean <- dt.clean[,!names(dt.clean) %in% drops]
str(dt.clean)

#------------------# 
#   Preprocessing  #
#------------------#

set.seed(100)
table(dt.clean$DV)

# Create training and testing set (80%/20% of our data, respectively):
trainIndex <- createDataPartition(
  dt.clean$DV, p = .8,
  list = FALSE,
  times = 1)

d_train <- dt.clean[ trainIndex,]
d_test  <- dt.clean[-trainIndex,]
dim(d_train) # 80% train
dim(d_test) # 20% test

nrow(d_train)/nrow(dt.clean) # should be ~80%
nrow(d_test)/nrow(dt.clean) # should be ~20%

# Check class balance
table(d_train$DV)
table(d_test$DV)

# Downsample for class imbalance
d_trainDS <- data.table(downSample(x = d_train, y = d_train$DV))[, -c("DV"), with = FALSE]
table(d_trainDS$DV)

# Check for missing values in data:
nrow(d_trainDS$DV)
nrow(na.omit(d_trainDS$DV))

#Generic Preprocessing (removes zero variance columns)
d_trainDS.pre <- preProcess(d_trainDS, method = c("zv"))


d_train.clean <-d_trainDS.pre
d_test.clean <-d_test
d_train.clean$DV


#------------------# 
#    Fit Models    #
#------------------#

class(d_train.clean) # First check if data is a data.table

# (1) Stochastic gradient boosting
d.train.gbm <-train(
  x = d_train.clean[, -c("DV"), with = FALSE],
  y = d_train.clean$DV,
  method = "gbm",
  trControl = trainControl(method = "cv", number = 2),
  verbose = FALSE)


# (2) Recursive partitioning trees (rpart)
d.train.rpart <-train(
  x = d_train.clean[, -c("DV"), with = FALSE],
  y = d_train.clean$DV,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 2))


# (3) Support Vector Machine
d.train.svmLin2 <-train(
  x = d_train.clean[, -c("DV"), with = FALSE],
  y = d_train.clean$DV,
  method = "svmLinear2")

d.train.svmLin2 <-train(
  DV~.,
  data = d_train.clean,
  method = "svmLinear2",
  trControl = trainControl(method = "cv", number = 2))
# 
# d.train.svmLin3 <-train(
#   x = d_train.clean[, -c("DV"), with = FALSE],
#   y = d_train.clean$DV,
#   method = "svmLinear3")

d.train.svmLin3 <-train(
  DV~.,
  data = d_train.clean,
  method = "svmLinear3",
  trControl = trainControl(method = "cv", number = 2))

# #(4) Single C5.0 Rule-set
# d.train.singlerule <-train(
#   x = d_train.clean[,-c("DV"),with = FALSE],
#   y = d_train.clean$DV,
#   method = "C5.0Rules",
#   trControl = trainControl(method = "cv", number = 2))
# 

d.train.singlerule <-train(
  DV~.,
  data = d_train.clean,
  method = "C5.0Rules",
  trControl = trainControl(method = "cv", number = 2))

# #(5) Single C5.0 Tree
# d.train.singletree <-train(
#   x = d_train.clean[,-c("DV"),with = FALSE],
#   y = d_train.clean$DV,
#   method = "C5.0Tree",
#)

d.train.singletree <-train(
  DV~.,
  data = d_train.clean,
  method = "C5.0Tree",
  trControl = trainControl(method = "cv", number = 2))


#(6) C5.0 Model
d.train.c50 <-train(
  DV~.,
  data = d_train.clean,
  method = "C5.0",
  trControl = trainControl(method = "cv", number = 2))


#(7a) Ranger
d.train.ranger <-train(
  x = d_train.clean[,-c("DV"),with = FALSE],
  y = d_train.clean$DV,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 2))

#(8) xgb tree model
d.train.xgb <- train(
  DV~., data = d_train.clean,
  method = 'xgbTree',
  trControl = trainControl(method= 'cv', number = 2))


#(9) glm model
# d.train.glm <-train(
#   x = d_train.clean[,-c("DV"), with = FALSE],
#   y = d_train.clean$DV,
#   method = "glm",
#   trControl = trainControl(method = "cv"))

#(10) Logistic regression model
d.train.logr1 <-train(
  DV~., data = d_train.clean,
  method = 'glm')


#-----------------------# 
# Model fitting results #
#-----------------------#

#(1)
d.train.gbm

#(2)
d.train.rpart

#(3)
d.train.svmLin2
d.train.svmLin3

#(4)
d.train.singlerule

#(5)
d.train.singletree

#(6)
d.train.c50

#(6a)
d.train.c50all

#(7)
d.train.ranger

#(8)
d.train.xgb

#(9)
# d.train.glm

#(10)
d.train.logr1

# Plot important variables for both gbm, rpart models:
topN <-10

plot(varImp(d.train.gbm), topN)
plot(varImp(d.train.rpart), topN)
plot(varImp(d.train.svmLin2),topN)
plot(varImp(d.train.svmLin3),topN)
plot(varImp(d.train.singlerule),topN)
plot(varImp(d.train.singletree),topN)
plot(varImp(d.train.c50),topN)
plot(varImp(d.train.ranger),topN) #ERROR: No importance values (Email dobolyi)
plot(varImp(d.train.xgb),topN)
# plot(varImp(d.train.glm),topN)
plot(varImp(d.train.logr1),topN)


# Predict test data: (postResample(Predictions.treeboost, d_test.clean$DV) will tell you acc, Kappa)
Predictions.gbm <- predict(d.train.gbm, newdata = d_test.clean[,-c("DV")])
Predictions.rpart <- predict(d.train.rpart, newdata = d_test.clean[,-c("DV")])
Predictions.svmLin2 <- predict(d.train.svmLin2, newdata = d_test.clean[,-c("DV")])
Predictions.svmLin3 <-predict(d.train.svmLin3, newdata = d_test.clean[,-c("DV")])
Predictions.singlerule <-predict(d.train.singlerule, newdata = d_test.clean[,-c("DV")])
Predictions.singletree <-predict(d.train.singletree, newdata = d_test.clean[,-c("DV")])
Predictions.c50 <-predict(d.train.c50, newdata = d_test.clean[,-c("DV")])
Predictions.ranger <-predict(d.train.ranger, newdata = d_test.clean[,-c("DV")])
Predictions.xgb <-predict(d.train.xgb, newdata = d_test.clean[,-c("DV")])
# Predictions.glm <-predict(d.train.glm, newdata = d_test.clean[,-c("DV")])
Predictions.logr1 <-predict(d.train.logr1, newdata = d_test.clean[,-c("DV")])

head(Predictions.gbm)
head(Predictions.rpart)
head(Predictions.svmLin2)
head(Predictions.svmLin3)
head(Predictions.singlerule)
head(Predictions.singletree)
head(Predictions.c50)
head(Predictions.ranger)
head(Predictions.xgb)
# head(Predictions.glm)
head(Predictions.logr1)


# Compare model performance using confusion matrices (i.e., compare true survival values to model predicted survival values; see ?confusionMatrix):
confusionMatrix(d_test.clean$DV, Predictions.gbm)
confusionMatrix(d_test.clean$DV, Predictions.rpart)
confusionMatrix(d_test.clean$DV, Predictions.svmLin2)#
confusionMatrix(d_test.clean$DV, Predictions.svmLin3)
confusionMatrix(d_test.clean$DV, Predictions.singlerule)
confusionMatrix(d_test.clean$DV, Predictions.singletree)
confusionMatrix(d_test.clean$DV, Predictions.c50)
confusionMatrix(d_test.clean$DV, Predictions.ranger)
confusionMatrix(d_test.clean$DV, Predictions.xgb)
# confusionMatrix(d_test.clean$DV, Predictions.glm)
confusionMatrix(d_test.clean$DV, Predictions.logr1)




#------------------# 
#  Compare models  #
#------------------#

# Compare model performance statistics (accuracy, kappa) visually:
compareModels <- resamples(list(gbm=d.train.gbm, 
                                rpart=d.train.rpart,
                                svmLin2 = d.train.svmLin2,
                                svmLin3 = d.train.svmLin3,
                                singlerule = d.train.singlerule,
                                singletree = d.train.singletree,
                                # c50 = d.train.c50,
                                ranger = d.train.ranger,
                                xgb = d.train.xgb
))


#######Different numbers of resamples in each model? (svmLin2 = d.train.svmLin2)

bwplot(compareModels)
dotplot(compareModels)


# Predict test data probabilities:
PredictionProbs.gbm <- predict(d.train.gbm, newdata = d_test.clean[, -"DV", with = FALSE], type = "prob")
PredictionProbs.rpart <- predict(d.train.rpart, newdata = d_test.clean[, -"DV", with = FALSE], type = "prob")

# # Compare models via ROC:
gbm.ROC <- roc(
  predictor = PredictionProbs.gbm[, 2],
  response = d_test.clean$DV,
  levels = levels(d_test.clean$DV))

rpart.ROC <- roc(
  predictor = PredictionProbs.rpart[, 2],
  response = d_test.clean$DV,
  levels = levels(d_test.clean$DV))

auc(gbm.ROC) #0.7892
auc(rpart.ROC)#0.7631

plot(gbm.ROC, print.auc = TRUE, col = "blue")
plot(rpart.ROC, print.auc = TRUE, col = "red", print.auc.y = .45, add = TRUE)


# Plot the rpart model as a tree:
fancyRpartPlot(d.train.svmLin2$finalModel)
