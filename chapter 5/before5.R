# Validation set Approach: 원래 데이터를 랜덤하게 같은 양의 두 그룹으로 나눈 것. training(test와 같은 크기에 validation 포함), test으로 불리며 training으로 선정된 데이터에 너무 의존함.
# Cross-Validation: 이 때문에 CV를 반복하는 게 낫다 생각하게 됨.
# LOOCV (Leave-One-Out Cross-Validation): n개에서 계속 하나를 제거해서 CV하고 모델 평가값을 구하기를 반복.
# k-fold CV (k-Fold Cross-Validation): 전체를 동일 갯수의 k로 나누어 그 중 하나를 validation, test로 나머지를 training으로 해서 모델 평가값을 구하기를 반복.
# Bootstrap: 주어진 데이터를 랜덤 복원으로 뽑은 (각기 다른) 데이터를 만들어 이를 샘플 삼아 반복.


# 예제를 통한 resampling to training and test(validation) 함수 모음
library(ISLR) # data: Portfolio
library(boot) # cv.glm()

attach(Auto)

# set.seed(1)
train = sample(392, 196)

lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)


# set.seed(2)
train = sample(392, 196)

lm.fit = lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)


# Leave-One-Out Cross-Validation
glm.fit = glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)

lm.fit = lm(mpg ~ horsepower, data = Auto)


glm.fit = glm(mpg ~ horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta
# 기본적인 CV가 2개로 나누기 때문에 2개?

cv.error = rep(0, 5)
for (i in 1:5) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}; rm(i)
cv.error


# k-Fold Cross-Validation
# set.seed(17)
cv.error.10 = rep(0, 10)
for (i in 1:10) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}; rm(i)
cv.error.10
# 두번째는 bias-corrected version이라는 거 보니까 혹시 몰라서 끝값을 절삭하고 구한 듯?


# Bootstrap
# 모수의 정확성 추정
alpha.fn = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X, Y))/(var(X) + var(Y) - 2*cov(X, Y)))
}
alpha.fn(Portfolio, 1:100)

# set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))

boot(Portfolio, alpha.fn, R = 1000)


# (선형) 모델 정확성 추정
boot.fn = function(data, index) return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
boot.fn(Auto, 1:392)

# set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(Auto, sample(392, 392, replace = T))

boot(Auto, boot.fn, 1000)
summary(lm(mpg ~ horsepower, data = Auto))$coef


boot.fn = function(data, index) coefficients(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index))
# set.seed(1)
boot(Auto, boot.fn, 1000)
summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef
