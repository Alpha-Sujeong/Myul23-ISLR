# perfect separate: Maximal Margin Classifier, maximal margin hyperplane
# Support Vectors (points), Support Vector Classifier
# more flexible: Support Vector Machine
# kernel: linear kernel(alike Person correlation), polynomial kernel, radial kernel etc.
# bigger tuning parameter, many support vectors, wider margin, higher bias => low variance
# More than two classes, can use SVM
# almost (perfect) separate data set is uesful on SVM? 
# otherwise, Logistic Regression.
# but SVM exists perfect zero point.


# SVM function by examples
library(ISLR)
library(e1071) # svm(), tune()


# set.seed(1)
X = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1, 10), rep(1, 10))
X[y == 1,] = X[y == 1,] + 1

plot(X, col = (3 - y))


dat = data.frame(X, y = as.factor(y))
svmfit = svm(y ~., dat, kernel = "linear", cost = 10, scale = F)
# default is radial
# don't do standardization

plot(svmfit, dat)
# plot.svm() no longer exist, maybe

svmfit$index
summary(svmfit)


# changing tuning parameter
# set.seed(1)
tune.out = tune(svm, y ~., data = dat, kernel = "linear",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# tune(): 10-fold cross-validation
summary(tune.out)
# 1 or 100 cost is best on my sampling data

bestmod = tune.out$best.model
summary(bestmod)

# test the made support vector classifier model
xtest = matrix(rnorm(20*2), ncol = 2)
ytest = sample(c(-1, 1), 20, rep = T)
xtest[ytest == 1,] = xtest[ytest == 1,] + 1
testdat = data.frame(xtest, y = as.factor(ytest))
# made confusion and conflict, because of colnames to upper alike sentense.

ypred = predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)


# test cost = 0.01
svmfit = svm(y ~., dat, kernel = "linear", cost = 0.01, scale = F)
ypred = predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)


# almost linearly separable, hyperplane
X[y == 1,] = X[y == 1,] + .5
plot(X, col = (y + 5)/2, pch = 19)

dat = data.frame(X, y = as.factor(y))
svmfit = svm(y ~., dat, kernel = "linear", cost = 1e5)
summary(svmfit)
# set gamma = .5, but don't changed
# still use many support vectors, like 8
# set and use very randomly seed?

svmfit = svm(y ~., dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)



# SVM: Support Vector Machine
# almost non-linear
# set.seed(1)
X = matrix(rnorm(200*2), ncol = 2)
X[1:100,] = X[1:100,] + 2
y = c(rep(1, 150), rep(2, 50))
dat = data.frame(X, y = as.factor(y))

plot(X, col = y)
# col = c("blue", "green"), pretty good


train = sample(200, 100)
svmfit = svm(y ~., dat[train,], gamma = 1, cost = 1)
# default is radial kernel
# naturally, exist difference with use custom gamma and default gamma

plot(svmfit, dat[train,])
summary(svmfit)

svmfit = svm(y ~., dat[train,], gamma = 1, cost = 1e5)
plot(svmfit, dat[train,], col = c("azure", "lightblue"))


# change tuning parameter
# set.seed(1)
tune.out = tune(svm, y ~., data = dat[train,],
                ranges = list(cost = c(.1, 1, 10, 100, 10^3),
                gamma = c(.5, 1, 2, 3, 4)))
summary(tune.out)

table(true = dat[-train, 'y'],
      pred = predict(tune.out$best.model, newdata = dat[-train,]))
# misclassifing ratio is 28%



# ROC Curves
library(ROCR)


rocplot = function(pred, truth, ...) {
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

# best is gamma = 2, cost = 1 by book
svmfit.opt = svm(y ~., dat[train,], gamma = 2, cost = 1,
                 decision.values = T)
fitted = attributes(predict(svmfit.opt, dat[train,],
                            decision.values = T))$decision.values

par(mfrow = c(1,2))
rocplot(fitted, dat[train, 'y'], main = "Training Data")

svmfit.flex = svm(y ~., dat[train,], gamma = 50, cost = 1,
                  decision.values = T)
fitted = attributes(predict(svmfit.flex, dat[train,],
                            decision.values = T))$decision.values
rocplot(fitted, dat[train, 'y'], add = T, col = "red")


# test data
fitted = attributes(predict(svmfit.opt, dat[-train,],
                            decision.values = T))$decision.values
rocplot(fitted, dat[-train, 'y'], main = "Test Data")

fitted = attributes(predict(svmfit.flex, dat[-train,],
                            decision.values = T))$decision.values
rocplot(fitted, dat[-train, 'y'], add = T, col = "red")



# SVM with Multiple classes
# set.seed(1)
X = rbind(X, matrix(rnorm(50*2), ncol = 2))
y = c(y, rep(0, 50))
X[y == 0, 2] = X[y == 0, 2] + 2
dat = data.frame(X, y = as.factor(y))

par(mfrow = c(1,1))
plot(X, col = (y + 1))

svmfit = svm(y ~., dat, cost = 10, gamma = 1)
plot(svmfit, dat, col = c("lightblue", "azure", "lightgreen"))



# Application to Gene Expression Data
# functions are very normal functioning
names(Khan)
dim(Khan$xtrain); dim(Khan$xtest)
length(Khan$ytrain); length(Khan$ytest)
table(Khan$ytrain); table(Khan$ytest)

dat = data.frame(Khan$xtrain, y = as.factor(Khan$ytrain))

out = svm(y ~., dat, kernel = "linear", cost = 10)
summary(out)
table(out$fitted, dat$y)

dat.te = data.frame(Khan$xtest, y = as.factor(Khan$ytest))
pred.te = predict(out, newdata = dat.te)
table(pred.te, dat.te$y)

