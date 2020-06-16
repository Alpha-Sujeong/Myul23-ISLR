# Local regression: s에 따라서 가중치를 주어 부분 함수를 구성하는 것.
# Nearest-Neighbor와 유사, glm? s를 정한다는데 모르겠다.
# GAMs (Generalized Additive Models)
# 각 예측변수별로 반응변수와의 관계를 찾아 그냥 더해서 만드는 모형
# 단점: important interactions will miss


# 예제를 통한 GAMs 함수
library(ISLR)
library(splines) # ns()
library(gam) # s(), plot.Gam()
attach(Wage)

# 사실 처음에 그냥 더해서 만들어진다는 걸 너무 강조해서 단순으로 구한 계수까지 구해서 선형 결합하는 건 줄 알고 진짜 식겁했다.
# 다행히 모형만을 더하는 개념이었다. 그래서 이런 식으로도 만들 수 있다.
gam1 = lm(wage ~ ns(year, 4) + ns(age, 5) + education)
# Natural Spline을 이용했는데, GAMs에선 Smoothing Spline을 사용함.

par(mfrow = c(1,3))
plot.Gam(gam1, se = T, col = "red")
# 이렇게 하면 lm으로도 만들 수 있단 사실.

gam.m3 = gam(wage ~ s(year, 4) + s(age, 5) + education)
plot(gam.m3, se = T, col = "blue")


gam.m1 = gam(wage ~ s(age, 5) + education)
gam.m2 = gam(wage ~ year + s(age, 5) + education)
anova(gam.m1, gam.m2, gam.m3, test = 'F')
# year가 선형으로 있는 게 낫지만, 과연 선형인가에 대한 증거가 없음.

summary(gam.m3)
# preds = predict(gam.m2, newdata = Wage)


# local regression의 이용
gam.lo.i = gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education)

# library(akima)
# 2차원 표면 plot
plot(gam.lo.i)


gam.lr = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial)
plot(gam.lr, se = T, col = "green")
table(education, I(wage > 250))
# 정말 고졸 전은 예측이 하나도 성공하지 않았구나.

gam.lr.s = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial,
               subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se = T, col = "green")
# R이 y축을 자동으로 해주니까 year의 se 모양이 아주 X가 다 됐는데.
