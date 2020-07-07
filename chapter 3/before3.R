# 예제를 통한 regression 함수 모음
# packages
library(MASS)
library(ISLR) # (data) Boston, Carseats
library(car)

# write.csv(Boston, "Boston.csv", row.names = T, quote = F)
# fix(Boston)	# 새 창으로 데이터 형태 및 상태 확인

lm.fit = lm(medv ~ lstat, data = Boston)		# attach(Boston);	lm.fit = lm(medv ~ lstat)
summary(lm.fit);	summary(lm.fit)$r.sq;	summary(lm.fit)$sigma
# names(lm.fit);	coef(lm.fit);	confint(lm.fit): 회귀변수 2.5, 97.5 백분위수 값

predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "prediction")

plot(lstat, medv)

abline(lm.fit)
# abline(lm.fit, lwd = 3);	abline(lm.fit, lwd = 3, col = "red")
# plot(lstat, medv, col = "red");	plot(lstat, medv, pch = 20);
# plot(lstat, medv, pch = "+");	plot(1:20, 1:20, pch=1:20)
par(mfrow = c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))	# 외적 표준화잔차

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit));	# 데이터 중 가장 큰 값을 반환.

lm.fit = lm(medv ~ lstat + age, data=Boston)
summary(lm.fit)
# lm.fit = lm(medv ~, data = Boston);	summary(lm.fit)

# library(car)
vif(lm.fit)

lm.fit1 = lm(medv ~.-age, data=Boston);		# lm.fit1 = update(lm.fit, ~.-age)
summary(lm.fit1)

summary(lm(medv ~ lstat*age, data=Boston)

# special formula mark ^, I()를 통해 값의 제곱임을 표현
lm.fit2 = lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)

lm.fit = lm(medv ~ lstat)
anova(lm.fit, lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5 = lm(medv ~ poly(lstat, 5))
summary(lm.fit5)

summary(lm(mdev ~ log(rm), data = Boston)

# write.csv(Carseats, "Carsearts.csv", row.names = T, quote = F)

lm.fit = lm(Sales ~.+Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

# attach(Carseats)
contrasts(ShelveLoc)		# 더미 변수 생성기
