# classify를 위해서 Bayes' Theoream을 이용, Bayes' Classifier를 구해야 함.
# 이 때문에 LR, LDA, QDA, KNN 방법을 비교
# LR: Normal, response: Yes or No
# LDA: Gaussian(Multiple Normal), response: 2 cases
# QDA: Normal, no Linear, response: 2개 이상
# KNN: 1이거나 class level을 정해야 함, 선형일 때를 제외하고는 수준을 정했을 때가 이상적인 방법이긴 한데 다른 방법론에 비해 악, 최악이 걸릴 수준이 가능성이 높아서..


# 예제를 통한 response classify prediction 함수 모음
library(ISLR) # Smarket: Stock Market Data
summary(Smarket)

pairs(Smarket)
cor(Smarket[,-9])

attach(Smarket)

plot(Volume)


# Logistic Regression
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial)
summary(glm.fits)
# coef(glm.fits;  summary(glm.fits)$coef; summary(glm.gits)$coef[,4]

glm.probs = predict(glm.fits, type = "response")
glm.probs[1:10]

# 질적 반응변수 direction에 대한 종합적 평가
contrasts(Direction)

glm.pred = rep("Down", 1250)
glm.pred[glm.probs > 0.5] = "Up"

table(glm.pred, Direction)
mean(glm.pred == Direction) # 전체 중, 맞게 분류한 확률, 52.2%

# year을 기준으로 train 데이터 정해놓고 만들기
train = (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005) # sum(!train)
Direction.2005 = Direction[!train]

glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)  # 48%

# 결론: 변수를 다 넣는 것보다 2개만 넣은 게 더 잘 예측되더라.
glm.fits = glm(Direction ~ Lag1 + Lag2, family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)  # 56%

predict(glm.fits, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")


# LDA, Linear Discriminant Analysis
library(MASS)
lda.fit = lda(Direction ~ Lag1 + Lag2, subset = train)
lda.fit # 파이1 = 0.492, 파이2 = 0.508

lda.pred = predict(lda.fit, Smarket.2005)
# names(lda.pred)

lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)  # 56%

# 모델에서 사후 확률은 마켓 성장률 감소와 연관이 있다
sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < 0.5)

# lda.pred$posterior[1:20,1]
# lda.class[1:20]

sum(lda.pred$posterior[,1] > 0.9)


# QDA, Quadratic Discriminant Analysis
qda.fit = qda(Direction ~ Lag1 + Lag2, subset = train)
qda.fit

qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005) # 59.9%


# KNN, K-Nearest Neighbors
library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

# set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)  # 50%

knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)  # 53.6%



# Caravan data in ISLR on KNN
attach(Caravan)
# summary(Purchase) # No: 0.0598

standardized.X = scale(Caravan[,-86])
var(Caravan[,1]); var(Caravan[,2])
# var(standardized.X[,1]);  var(standardized.X[,2])

# test data set 만드는 중
test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]

# set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)  # 오류: 11.8%
mean(test.Y != "No")      # !No: 5.9%
# table(knn.pred, test.Y)

knn.pred = knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)   # Yes 맞춘 확률: 19.2%

knn.pred = knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)   # Yes 맞춘 확률: 26.7%


# 구매 가능성에 대한 기준점이 0.5가 아니라
# 0.25를 넘으면 구매할 가능성이 높아라고 얘기하고 싶은 것.
glm.fits = glm(Purchase ~., data = Caravan, family = binomial, subset =-test)
glm.probs = predict(glm.fits, Caravan[test,], type = "response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, test.Y)   # 말했다시피 0.5를 넘는 값은 사실상 존재하지 않음. 따라서 0

glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.25] = "Yes"
table(glm.pred, test.Y)   # Yes 맞춘 확률: 33.3%
