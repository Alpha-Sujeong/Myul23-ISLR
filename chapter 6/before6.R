# Best Subset Selection을 위해 subset selection, regularization, dimension reduction
# subset selection: foward stepwise selection, backward stepwise selection, hybrid(or stepwise) selection
# Cp(단순 scale의 추가가 AIC라 유사함), BIC, Adjusted-R^2로 비교하는데, BIC를 제일 선호함.
# regularization: ridge regression, the lasso
# dimension reduction: pcr(pca regression, unsupervised) and pls
# PLS (Partial Least Squares): supervised, first principal component is scaling sum


# 예제를 통한 subset selection 함수 모음
library(ISLR) # data: Hitters

# fix(Hitters)
# names(Hitters)

Hitters = na.omit(Hitters)
dim(Hitters)
# sum(is.na(Hitters$Salary))

# attach(Hitters)


# best subset selection, sse로 판단, lm과 비슷, summary로 확인.
library(leaps)

regfit.full = regsubsets(Salary~., Hitters, nvmax = 19)
# 모형 변수 갯수 19개로 고정
reg.summary = summary(regfit.full)
names(reg.summary)

# 변수 추가로 증가하는 rsq 한 눈에 보기
reg.summary$rsq

# 와 함수와 하려는 거 참았다..
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = 'l')

plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R-squared", type = 'l')
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)],
       col = "red", cex = 2, pch = 20)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = 'l')
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)],
       col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = 'l')
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)],
       col = "red", cex = 2, pch = 20)

# plot의 옵션으로 regsubsets를 쉽게 표현할 수 있음.
# par(mfrow = c(2,2))
# plot(regfit.full, scale = "r2")
# plot(regfit.full, scale = "adjr2")
# plot(regfit.full, scale = "Cp")
# plot(regfit.full, scale = "bic")

coef(regfit.full, which.min(reg.summary$bic))



# Foward, Backward Stepwise Selection (FS, BE)
regfit.fwd = regsubsets(Salary~., Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
# 아 이거 최근에 부른 변수 부르는 별칭 있는데, 그거 쓰면 편한데.

regfit.bwd = regsubsets(Salary~., Hitters, nvmax = 9, method = "backward")
summary(regfit.bwd)

# 그냥 각 subset selection에서 어떤 순서대로 들어왔는지 보려고 찍는 거.
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)


# choose model using validation set or cross-validation
# set.seed(1)
train = sample(c(T, F), nrow(Hitters), rep = T)
# Hitters의 행수만큼 t,f 반복으로 생성
test = (!train)

regfit.best = regsubsets(Salary~., Hitters[train,], nvmax = 19)

test.mat = model.matrix(Salary~., Hitters[test,])
val.errors = rep(NA, 19)
for(i in 1:19) {
  coefi = coef(regfit.best, id = i)
  pred = test.mat[, names(coefi)] %*% coefi
  val.errors[i] = mean((Hitters$Salary[test] - pred)^2)
}; rm(i)
val.errors
coef(regfit.best, which.min(val.errors))
# 어후 생각보다 beta_0랑 몇 개 회귀계수가 차이가 심하네, 그리고 errors의 최대값이 변해서 보여주는 회귀계수 수도 자꾸 변함.

# 이 정도면 그냥 predict 함수 만들어줘라.
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars] %*% coefi
}

regfit.best = regsubsets(Salary~., Hitters, nvmax = 19)
coef(regfit.best, which.min(val.errors))

# select model when different size
# set.seed(1)
k = 10
folds = sample(1:k, nrow(Hitters), replace = T)
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for(i in 1:k) {
  best.fit = regsubsets(Salary~., Hitters[folds != i,], nvmax = 19)
  for(j in 1:19) {
    pred = predict(best.fit, Hitters[folds == i,], id = j)
    cv.errors[i, j] = mean((Hitters$Salary[folds == i] - pred)^2)
  }
}; rm(i); rm(j)

mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors

par(mfrow = c(1,1))
plot(mean.cv.errors, type = 'b')
# 11개가 최솟점이긴 하네.

# 11개가 최소라 coef 확인하겠다는 건데, 굳이 그럴 필요가.
# reg.best = regsubsets(Salary~., Hitters, nvmax = 19)
# coef(reg.best, 11)


# Ridge Reggresion and the Lasso
library(glmnet)

x = model.matrix(Salary~., Hitters)[, -1]
# model의 예측변수들을 자동으로 수치화(더미화 포함)해서 matrix로 표현.
y = Hitters$Salary

# 와 역시 프로그래밍이 짱이야 alpha = 0이면 ridge고, 1이면 lasso야?
# 대단하다, 진짜 어지간히 하나로 만들고 싶었나보다.

grid = 10^seq(10, -2, length = 100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)
# 원래라면 함수가 자동으로 lambda를 설정했겠지만, 지금은 강제적으로 grid 영역 안에서 찾으라고 설정함.
# 또 glmnet 함수는 자동으로 표준화해서 사용하므로, 이 기능이 필요없다면 standardize = F를 하자.

# dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))
# 확실히 lambda가 커지니까 coef가 작아지긴 하네.

predict(ridge.mod, s = 50, type = "coefficients")[1:20,]


# set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]

ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)

# MSE hat
mean((mean(y[train]) - y.test)^2)

ridge.pred = predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred - y.test)^2)

ridge.pred = predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test)^2)

ridge.pred = predict(ridge.mod, s = 0, newx = x[test,])
# 원래라면 s를 완벽하게 0에 맞추는 옵션이었던 거 같은데
# x와 y에 조정을 해야 하는지 안된다고 하네.
mean((ridge.pred - y.test)^2)

lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, type = "coefficients")[1:20,] # exact = T

# set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
# 최적 ridge의 lambda 범위를 모아다 최솟값을 가져온 건데, 왜 seed가 필요할까.
# 내부적으로 lambda를 찾을 때 resampling를 사용하는 건가.

ridge.pred = predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2)

# lambda 구역을 설정하지 않은 적합을 해보자.
out = glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]
# 정말 ridge는 변수를 버릴 생각을 하지 않는구나.


# The Lasso
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)
# 와우. 누가 선 긋다가 실수한 것처럼 여기저기 막 나가네.

# set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)

bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2)
# 확실히 ridge랑 비슷한 값인 건만 알겠음.

out = glmnet(x, y , alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef
# 오 확실히 0인 건 있네. 랜덤성에 의해 12개가 0인 건 아니지만.



# PCR and PLS Regression
library(pls)

# set.seed(2)
pcr.fit = pcr(Salary~., data = Hitters, scale = T, validation = "CV")
# 조건이 많은 lm, 얘는 data를 써줘야 인식하는 약간 poor한 함수다.
summary(pcr.fit)
# 리스트 첫번째 항목은 EDA에서 배운 CV가 맞는가?
# variation 보는 게 리스트 두번 항목이였다.

validationplot(pcr.fit, val.type = "MSEP")
# 오홍 pca 때의 그 e.v 보는 그 함수 같네.

# set.seed(1)
pcr.fit = pcr(Salary~., data = Hitters, subset = train, scale = T, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

# pc가 7개일 때 90%을 넘은 걸 보고 7을 선택한 듯 보임.
pcr.pred = predict(pcr.fit, x[test,], ncomp = 7)
mean((pcr.pred - y.test)^2)

pcr.fit = pcr(y ~ x, scale = T, ncomp = 7)
summary(pcr.fit)


# PLS
# set.seed(1)
pls.fit = plsr(Salary~., data = Hitters, subset = train, scale = T, validation = "CV")
# 얘도 data는 data=으로 써야 하는 poor한 함수군요.
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

# validation 그림이 pc를 2로 하는 게 적정임을 보여줌.
pls.pred = predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test)^2)

pls.fit = plsr(Salary~., data = Hitters, scale = T, ncomp = 2)
summary(pls.fit)
