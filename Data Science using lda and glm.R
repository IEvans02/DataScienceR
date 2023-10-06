cancerdata = read.csv(’breast-cancer-wisconsin.csv’, header=T, na.strings="?")
#Remove duplicated data
cancerdata = cancerdata[!duplicated(cancerdata),]
set.seed(42)
train = sample(c(TRUE,FALSE),size=nrow(cancerdata),replace=TRUE, prob=c(0.8,0.2))
traindata = cancerdata[train,]
testdata = cancerdata[!train,]
#Remove the first column with ’Sample_code’
traindata = traindata[,-1]
#Set the variable Class as factor
traindata$Class = as.factor(traindata$Class)

#Fit glm
fit.lg = glm(Class ~ ., family=binomial, data=traindata)
summary(fit.lg)
lg_prob = predict(fit.lg, testdata, type="response")
ypred = ifelse(lg_prob>0.5, 4, 2)

table(testdata$Class, ypred)
#Misclassification Error
mean(testdata$Class!=ypred, na.rm=T)

library(MASS)
fit.lda = lda(Class ~ . , data=traindata)
fit.lda

#Predict for the test data
ypred.lda = predict(fit.lda, testdata)$class
table(testdata$Class, ypred.lda)
#Misclassification Error Rate
mean(testdata$Class!=ypred.lda, na.rm=T)