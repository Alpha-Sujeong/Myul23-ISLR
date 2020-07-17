# Decision Tree
# Bagging: Bootstrap 기법을 이용해 만든 (자기복제형) tree를 평균적으로 구성함.
# Random Forest: bagging보단 나아간 방법, subset m 또한 randomly
# Boosting: bagging하려고 나눈 데이터에서 첫번째로 tree를 구성하고, 나머진 validation set처럼 사용.


# 예제를 통한 (decision) tree 함수
library(ISLR)
library(tree)


# 전처리 중입니다.
High = ifelse(Carseats$Sales <= 8, "No", "Yes")
Carseats = data.frame(Carseats, High)

tree = tree(High ~.-Sales, Carseats)
summary(tree)

# 와. 함수는 알아듣겠는데 설명으로 쓰인 식을 못 알아듣겠는데.
plot(tree)
text(tree, pretty = 0)
# node label display, pretty = 0: category name whole display

tree # asterisk means it is a terminal node


# set.seed(2)
train = sample(1:nrow(Carseats), 200)
test = Carseats[-train,]
High.test = High[-train]

tree.carseats = tree(High ~.-Sales, Carseats, subset = train)
tree.pred = predict(tree.carseats, test, type = "class")
table(tree.pred, High.test)
# sum(diag(table(tree.pred, High.test)))/200


# 역시 함수가 좋아, cv가 바로 연결되어 있잖아.
# set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)

# names(cv.carseats)
cv.carseats
# 값을 보면 dev가 제일 작을 때는 3, 4번째 항
# 이때의 tree size(the number of terminal nodes)는 14와 9
# interpret을 위해 tree는 작을수록 좋기 때문에 9를 선택

par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
plot(cv.carseats$k, cv.carseats$dev, type = 'b')

prune.carseats = prune.misclass(tree.carseats, best = 9)
# par(mfrow = c(1, 1))
plot(prune.carseats)
text(prune.carseats, pretty = 0)
# 앞서 했던 그래프보단 정갈해졌음.

tree.pred = predict(prune.carseats, test, type = "class")
table(tree.pred, High.test)
# sum(diag(table(tree.pred, High.test)))/200


# 번외
prune.carseats = prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred = predict(prune.carseats, test, type = "class")
table(tree.pred, High.test)
# sum(diag(table(tree.pred, High.test)))/200
# 와 같으면 안 되는데, 더 작은 예측력이 나와야 하는데 왜 같은 값.



# Fitting Regression Trees
library(MASS) # data: Boston


# set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)

tree.boston = tree(medv ~., Boston, subset = train)
summary(tree.boston)
# 흠. 함수가 바뀐 것인가 train randomly가 너무 너무 잘 된 것인가.

plot(tree.boston)
text(tree.boston, pretty = 0)
# plot이 굉장히 다르게 나온 것 같군요. 뭐라는지 못 알아듣겠어요.

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')
# 흠 제껀 size가 늘수록 dev가 감소하는 모양인데요.

# 일단 조용히 따라해봅니다.
prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)
# 살짝 가지치기 되었습니다.

yhat = predict(tree.boston, newdata = Boston[-train,])
boston.test = Boston[-train, "medv"]

plot(yhat, boston.test)
abline(0, 1)
# 흠 솔직히 이 그림 좀 너무하단 생각이 드는데..
# 변수 변환을 하거나 더미 변수를 쓰거나 둘 중 하나는 해야 할 것 같은데.
mean((yhat-boston.test)^2)



# Bagging and Random Forests
library(randomForest)


# set.seed(1)
bag.boston = randomForest(medv ~., Boston, subset = train, mtry = 13, importance = T)
bag.boston
# mtry = 13: 13개의 예측변수를 사용합니다.
yhat.bag = predict(bag.boston, newdata = Boston[-train,])

plot(yhat.bag, boston.test)
abline(0, 1)

mean((yhat.bag - boston.test)^2)


bag.boston = randomForest(medv ~., Boston, subset = train, mtry = 13, ntree = 25)
yhat.bag = predict(bag.boston, newdata = Boston[-train,])
mean((yhat.bag - boston.test)^2)


# p/3
# set.seed(1)
rf.boston = randomForest(medv ~., Boston, subset = train, mtry = 6, importance = T)
yhat.rf = predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf - boston.test)^2)
# seed에 영향인지 mtry를 작게 한 건데, 원래 randomforest보다 매우 약간 크게 나오네요

importance(rf.boston)
# 대강 비슷하게 나오는 게 seed에 문제인 것인가
# randomforest의 내부 split 함수가 약간 바뀐 문제인 것 같기도.
varImpPlot(rf.boston)
# 흠. 참 알아보기 쉽게 나오네요.



# Boosting
library(gbm)


# set.seed(1)
boost.boston = gbm(medv ~., Boston[train,], distribution = "gaussian",
                   n.trees = 5000, interaction.depth = 4)
summary(boost.boston)
# 중요도 확인하게 sorting되어 나옴.

# par(mfrow = c(1, 2))
# 흠 먹질 않아요.
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2)
# 강의 때도 그랬지만, randomforest보다 더 나은 값으로 나오는데.


boost.boston = gbm(medv ~., Boston[train,], distribution = "gaussian",
                   n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
# default: shrinkage = 0.001
yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2)
# 축소 모수? 이름이 뭐더라
# 그거 값을 늘렸는데 mse 추정도 늘어버렸다. 분명이 줄어든다고 배운 것 같은데.
