# Decision Tree
# Bagging: Bootstrap ����� �̿��� ���� (�ڱ⺹����?) tree�� ��������� ������.
# Random Forest: bagging���� ���ư� ���, subset m ���� randomly
# Boosting: bagging�Ϸ��� ���� �����Ϳ��� ù��°�� tree�� �����ϰ�, ������ validation setó�� ���.


# ������ ���� (decision) tree �Լ�
library(ISLR)
library(tree)


# ��ó�� ���Դϴ�.
High = ifelse(Carseats$Sales <= 8, "No", "Yes")
Carseats = data.frame(Carseats, High)

tree = tree(High ~.-Sales, Carseats)
summary(tree)

# ��. �Լ��� �˾Ƶ�ڴµ� �������� ���� ���� �� �˾Ƶ�ڴµ�.
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


# ���� �Լ��� ����, cv�� �ٷ� ����Ǿ� ���ݾ�.
# set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)

# names(cv.carseats)
cv.carseats
# ���� ���� dev�� ���� ���� ���� 3, 4��° ��
# �̶��� tree size(the number of terminal nodes)�� 14�� 9
# interpret�� ���� tree�� �������� ���� ������ 9�� ����

par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
plot(cv.carseats$k, cv.carseats$dev, type = 'b')

prune.carseats = prune.misclass(tree.carseats, best = 9)
# par(mfrow = c(1, 1))
plot(prune.carseats)
text(prune.carseats, pretty = 0)
# �ռ� �ߴ� �׷������� ����������.

tree.pred = predict(prune.carseats, test, type = "class")
table(tree.pred, High.test)
# sum(diag(table(tree.pred, High.test)))/200


# ����
prune.carseats = prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred = predict(prune.carseats, test, type = "class")
table(tree.pred, High.test)
# sum(diag(table(tree.pred, High.test)))/200
# �� ������ �� �Ǵµ�, �� ���� �������� ���;� �ϴµ� �� ���� ��.



# Fitting Regression Trees
library(MASS) # data: Boston


# set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)

tree.boston = tree(medv ~., Boston, subset = train)
summary(tree.boston)
# ��. �Լ��� �ٲ� ���ΰ� train randomly�� �ʹ� �ʹ� �� �� ���ΰ�.

plot(tree.boston)
text(tree.boston, pretty = 0)
# plot�� ������ �ٸ��� ���� �� ������. ������� �� �˾Ƶ�ھ��.

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')
# �� ���� size�� �ü��� dev�� �����ϴ� ����ε���.

# �ϴ� ������ �����غ��ϴ�.
prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)
# ��¦ ����ġ�� �Ǿ����ϴ�.

yhat = predict(tree.boston, newdata = Boston[-train,])
boston.test = Boston[-train, "medv"]

plot(yhat, boston.test)
abline(0, 1)
# �� ������ �� �׸� �� �ʹ��ϴ� ������ ��µ�..
# ���� ��ȯ�� �ϰų� ���� ������ ���ų� �� �� �ϳ��� �ؾ� �� �� ������.
mean((yhat-boston.test)^2)



# Bagging and Random Forests
library(randomForest)


# set.seed(1)
bag.boston = randomForest(medv ~., Boston, subset = train, mtry = 13, importance = T)
bag.boston
# mtry = 13: 13���� ���������� ����մϴ�.
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
# seed�� �������� mtry�� �۰� �� �ǵ�, ���� randomforest���� �ſ� �ణ ũ�� �����׿�

importance(rf.boston)
# �밭 ����ϰ� ������ �� seed�� ������ ���ΰ�
# randomforest�� ���� split �Լ��� �ణ �ٲ� ������ �� ���⵵.
varImpPlot(rf.boston)
# ��. �� �˾ƺ��� ���� �����׿�.



# Boosting
library(gbm)


# set.seed(1)
boost.boston = gbm(medv ~., Boston[train,], distribution = "gaussian",
                   n.trees = 5000, interaction.depth = 4)
summary(boost.boston)
# �߿䵵 Ȯ���ϰ� sorting�Ǿ� ����.

par(mfrow = c(1, 2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2)
# ���� ���� �׷�����, randomforest���� �� ���� ������ �����µ�.


boost.boston = gbm(medv ~., Boston[train,], distribution = "gaussian",
                   n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
# default: shrinkage = 0.001
yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2)
# ��� ���? �̸��� ������
# �װ� ���� �÷ȴµ� mse ������ �þ���ȴ�. �и��� �پ��ٰ� ��� �� ������.