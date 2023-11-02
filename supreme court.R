## CART model in R
stevens = read.csv('stevens.csv')
str(stevens)
## spliting the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio =0.7)

##dividing into training and test set
Train = subset(stevens,spl == TRUE)
Test = subset(stevens, spl == FALSE)

## installing the rpart package and rpart
## plotting package
install.packages('rpart')
library(rpart)
install.packages('rpart.plot')
library(rpart.plot)

## model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = 'class', minbucket=25)
prp(StevensTree)

PredictCART = predict(StevensTree, newdata = Test, type = 'class')

table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)

## ROCR curve
library(ROCR)
PredictROC = predict(StevensTree, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred,'tpr','fpr')
plot(perf)

## AUC
as.numeric(performance(pred, "auc")@y.values)

#######################################
StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = 'class', minbucket=5)
prp(StevensTree2)


##random forest models
install.packages('randomForest')
library(randomForest)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)

PredictForest = predict(StevensForest, newdata = Test)

table(Test$Reverse, PredictForest)
(72+52)/(52+25+21+72)



#####################################################################
set.seed(200)
spl = sample.split(stevens$Reverse, SplitRatio =0.7)
Train = subset(stevens,spl == TRUE)
Test = subset(stevens, spl == FALSE)
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = 'class', minbucket=25)
prp(StevensTree)
PredictCART = predict(StevensTree, newdata = Test, type = 'class')
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)



######################################################################
## cross validation
## to gey cp
install.packages('caret')
library('caret')

install.packages('e1071')
library('e1071')

install.packages('ggplot2')
library('ggplot2')

install.packages('class')
library('class')

numFolds = trainControl(method = 'cv', number = 10)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))

train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = 'rpart', trControl = numFolds, tuneGrid = cpGrid)

StevensTreeCv = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = 'class', cp=0.01)
PredictCV = predict(StevensTreeCv, newdata = Test, type = 'class')
table(Test$Reverse, PredictCV)
(43+65)/(43+34+28+65)

