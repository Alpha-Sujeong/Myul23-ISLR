# 선형을 일부 버리되, 설명력을 높이자는 계획이었으나
# polynomial: 다항 회귀, 해석력 감소
# Step Function: 구간 linear, 구간 poly, 일부 불연속, 미분 불가능으로 poly의 경우에는 한 단계 낮추거나 함.
# Spline


# 예제를 통한 Non-Linear Model 함수 모음
library(ISLR)
attach(Wage)


# Polynomial Regression and Step Functions
fit = lm(wage ~ poly(age, 4))
coef(summary(fit))

age.grid = seq(range(age)[1], range(age)[2])
preds = predict(fit, newdata = list(age = age.grid), se = T)
# 표준 정규분포에서 2차? 시그마 구간 구하듯 함.
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

# 다른 거랑 비교하려고 2칸을 만들어놓은 건가...
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
# 설마 oma는 padding을 의미하는 건가.
plot(age, wage, xlim = range(age), cex = .5, col = "darkgrey")
title("Degree -4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)


# 추가) orthogonal vs. non-orthogonal(정확한 명칭 기억나지 않음.)
fit2 = lm(wage ~ poly(age, 4, raw = T))
# not orthogonal polynomials
coef(summary(fit2))

# 이런 식으로도 non-orthogonal하게 만들 수 있음.
fit2a = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4))
coef(fit2a)

# 딱히 이쁘게 나오진 않는데.
fit2b = lm(wage ~ cbind(age, age^2, age^3, age^4))
coef(fit2b)

preds2 = predict(fit2, newdata = list(age = age.grid), se = T)
max(abs(preds$fit - preds2$fit))


# R도 이름 i를 인식 못할 테니, 다 써야지 뭐.
fit.1 = lm(wage ~ age)
fit.2 = lm(wage ~ poly(age, 2)) # quadratic
fit.3 = lm(wage ~ poly(age, 3)) # cubic
fit.4 = lm(wage ~ poly(age, 4))
fit.5 = lm(wage ~ poly(age, 5))

anova(fit.1, fit.2, fit.3, fit.4, fit.5)
coef(summary(fit.5))
# anova method works whether or not we used orthogonal polynomials

fit.1 = lm(wage ~ education + age)
fit.2 = lm(wage ~ education + poly(age, 2))
fit.3 = lm(wage ~ education + poly(age, 3))
anova(fit.1, fit.2, fit.3)


fit = glm(I(wage > 250) ~ poly(age, 4), family = binomial)
# family 옵션을 통해 y에 대한 예측을 binomial화 할 수 있다.
preds = predict(fit, newdata = list(age = age.grid), se = T)

# 로짓을 로지스틱으로 수동 변형
pfit = exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands = exp(se.bands.logit) / (1 + exp(se.bands.logit))

preds = predict(fit, newdata = list(age = age.grid), type = "response", se = T)

plot(age, I(wage > 250), xlim = range(age), type = 'n', ylim = c(0, .2))
points(jitter(age), I((wage > 250)/5), cex = .5, pch = 'l', col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = 'blue', lty = 3)
# jitter 함수가 뭐였는지 정확히 기억나지는 않는데... rug plot이랑 관련 있음.


# cut 함수를 이용한 자동 knot 지점 만들기
table(cut(age, 4))
fit = lm(wage ~ cut(age, 4))
coef(summary(fit))



# Splines
library(splines)

fit = lm(wage ~ bs(age, knots = c(25, 40, 60)))
# bs: basis functions의 knots 정하기 on full data
pred = predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2*pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2*pred$se, lty = "dashed")

dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")
# (auto or) compute는 정말 최고야.


fit2 = lm(wage ~ ns(age, df = 4))
# ns: natural spline
pred2 = predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)


fit = smooth.spline(age, wage, df = 16)
fit2 = smooth.spline(age, wage, cv =T)
fit2$df

plot(age, wage, xlim = range(age), cex = .5, col = "darkgrey")
title("Smoothing Spline")
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"),
       lty = 1, lwd = 2, cex = .8)


fit = loess(wage ~ age, span = .2)
fit2 = loess(wage ~ age, span = .5)

plot(age, wage, xlim = range(age), cex = .5, col = "darkgrey")
title("Local Regression")
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)
# locfit library
