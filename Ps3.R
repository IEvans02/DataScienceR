bankdata = read.csv("bank.csv", header=T, sep=";", stringsAsFactors = TRUE)
set.seed(42)
train = sample(c(TRUE,FALSE),size=nrow(bankdata),replace=TRUE, prob=c(0.7,0.3))
traindata = bankdata[train,]
testdata = bankdata[!train,]

#Create a classification tree
library(tree)
bank.tr = tree(y ~ ., data=traindata)
plot(bank.tr)
text(bank.tr, pretty=0)

bank.tr.cv = cv.tree(bank.tr, FUN = prune.misclass, K=5)
plot(bank.tr.cv$size, bank.tr.cv$dev, type="b",
     xlab = "Size of the tree", ylab="Deviance")

bank.prune = prune.misclass(bank.tr, best=4)
plot(bank.prune)
text(bank.prune, pretty=0)

# Training misclassification error
train.pred = predict(bank.prune, traindata, type="class")
table(train.pred, traindata$y)
mean(train.pred != traindata$y)

# Test misclassification error
test.pred = predict(bank.prune, testdata, type="class")
table(test.pred, testdata$y)
mean(test.pred != testdata$y)

# Use Bagging
library(randomForest)
set.seed(42)

bank.bag=randomForest(y~.,data = traindata,mtry=ncol(traindata)-1)
# Test misclassification error
test.pred = predict(bank.bag, testdata, type="class")
table(test.pred, testdata$y)
mean(test.pred != testdata$y)

#
importance(bank.bag)

#Random Forest work
rferror = NULL
for(m in 1:16){
  set.seed(42)
  bank.rf = randomForest(y~.,data = traindata,mtry=m)
  test.pred = predict(bank.rf, testdata, type="class")
  rferror = c(rferror, mean(test.pred != testdata$y))
}
plot(1:16, rferror, xlab="m", type="b", ylab="Test Error")

bank.rf = randomForest(y~.,data = traindata, mtry=5)
test.pred = predict(bank.rf, testdata, type="class")
mean(test.pred != testdata$y)
#Testing the Gini index of variables
importance(bank.rf)
#plot of importance

varImpPlot(bank.rf, main="Variable Importance Plot")
importance(bank.rf)