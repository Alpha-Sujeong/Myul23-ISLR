# 동봉된 문제.. 귀찮아서 풀다 말았음..
# 1) sales = TV + radio	# sales = 2.939 + 0.046TV + 0.189radio + e
# 2) KNN classifier is one of classifies to groups, but KNN regression is finding relations of dependent and independent variables.

# 3)	 salary = 50 + 20GPA + 0.07IQ + 35Gender + 0.01GPA&IQ + -10GPA&Gender + e
# (a) know GPA's mean
# (b) 207.5
# (c) 단위가 달라서 회귀식에서 의미가 없다고 얘기할 순 없지만, GPA와 IQ 간의 상관계수 값이 작다고는 말할 수 있을 듯.

# 4)
# (a) 기본적으로 독립 변수의 수가 많으면 설명력은 조금이라도 커지게 되어있음. 따라서, 의미없는 변수라도 어딘가 들어맞는다면 회귀 설명력은 높아질 것. 따라서, cubic regression의 RSS가 더 높을 것.
# (b)
# (c) 당연히 에러의 비율을 줄이는 비선형 분석이 설명력은 조금 더 높아지지 않나....? 비선형은 noise가 얼마인지 생각해야 하지만, 그것보단 적더라도 noise 또한 영향으로 받아들이니까.
# (d)

# 5) b^2(이번 x) / yj	의미상 hat matrix가 되어야 하는데
# 6) y - yhat = b1(x - xhat) 이거 관련해서 뭐가 있는데 기억이 나지 않아요.
# 7) linear regression has yhat and xhat as factor and point, 근데 b 구할 때 이미 0이라 선언한 격인데?

# 8) (data) Auto, write.csv(Auto, "auto", row.names = T, quote = F);	summary(lm(mpg ~ horsepower, data=Auto))
# (a) 회귀계수 p-value도 모형 p-value도 괜찮으니 관계가 있는 것처럼 보인다.	선형은 아니지만 꽤 강한 관계가 있는 것으로 보여짐.
# 점분석까지 해본 건 아니지만, scale graph를 보면 positive에 가까워 보인다.
# 39.935861-0.157845*98;	quantile(predict(lm.fit, interval="confidence"), c(.05, .95));	quantile(predict(lm.fit, interval="prediction"), c(.05, .95))
(b) abline(lm.fit)	(c) par(mfrow=c(2,2));	plot(lm.fit)

# 9)
(a) pairs(Auto)	(b) cor(Auto[0:7])
# lm.fit = lm(mpg ~.-name, data=Auto);		summary(lm.fit)
(c) 모형 적합도는 살아있다.	cylinders, horsepower, acceleration 변수가 의미 없어 보임.	수치형 자료로 아주 의미 있는 값을 가짐.
(d) par(mfrow=c(2,2));	plot(lm.fit)		323, 326, 327이 특이치로 보임.	14번이 영향력 있는 관측치처럼 보임.
(e) 다중공산성이 보여서 displacement 말고는 뺐는데요? 	음. 좋아요
(f) acceleration은 선형을 위해 변환을 취해야 할 듯. X^2 정도? 근데 log가 젤 낫다네...

# 10)
(a) lm(Sales ~ Price + Urban + US, data=Carseats)		(b) coef(lm(Sales ~ Price + Urban + US, data=Carseats)
(c) Carseats$Urban = ifelse(Carseats$Urban == "Yes", 1, 0);	Carseats$US = ifelse(Carseats$US == "Yes", 1, 0)
(d) Urban만은 0인 거 같기도...		(e) lm(Sales ~ Price + US, data=Carseats)
(f) Adjusted-R은 (e)가 더 높긴 한데, 기본 설명력은 같다니...
(g) quantile(predict(lm.fit, interval="confidence"), c(0.025, 0.975))	(h) 26, 210, 368

# 11)
(a) lm.fit = lm(y ~ x +0)
