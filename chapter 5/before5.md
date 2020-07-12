5\. Resampling Methods
================

  - Validation set Approach: 원래 데이터를 랜덤하게 같은 양의 두 그룹으로 나눈 것.
    training(test와 같은 크기에 validation 포함), test으로 불리며 training으로 선정된 데이터에
    너무 의존함.
  - Cross-Validation: 이 때문에 CV를 반복하는 게 낫다 생각하게 됨.
  - LOOCV (Leave-One-Out Cross-Validation): n개에서 계속 하나를 제거해서 CV하고 모델
    평가값을 구하기를 반복.
  - k-fold CV (k-Fold Cross-Validation): 전체를 동일 갯수의 k로 나누어 그 중 하나를
    validation, test로 나머지를 training으로 해서 모델 평가값을 구하기를 반복.
  - Bootstrap: 주어진 데이터를 랜덤 복원으로 뽑은 (각기 다른) 데이터를 만들어 이를 샘플 삼아 반복.

### pre-requires

``` r
library(boot)   # cv.glm()
data(Auto, package = "ISLR")
data(Portfolio, package = "ISLR")
```

store unique data sets.

``` r
write.csv(Auto, "Auto.csv", row.names = F, quote = F)
write.csv(Portfolio, "Portfolio.csv", row.names = F, quote = F)
```

-----

### 1\. Validation set Approach

``` r
attach(Auto)
train = sample(392, 196)
```

``` r
lm.fit = lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
```

    ## [1] 25.41874

``` r
lm.fit2 = lm(mpg ~ poly(horsepower, 2), subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
```

    ## [1] 19.26041

``` r
lm.fit3 = lm(mpg ~ poly(horsepower, 3), subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
```

    ## [1] 19.22012

-----

### 2\. Leave-One-Out Cross-Validation

``` r
glm.fit = glm(mpg ~ horsepower)
coef(glm.fit)
```

    ## (Intercept)  horsepower 
    ##  39.9358610  -0.1578447

``` r
glm.fit = glm(mpg ~ horsepower)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta
```

    ## [1] 24.23151 24.23114

2 as basic cv divide 2?

``` r
cv.error = rep(0, 5)
for (i in 1:5) {
  glm.fit = glm(mpg ~ poly(horsepower, i))
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}; rm(i)
cv.error
```

    ## [1] 24.23151 19.24821 19.33498 19.42443 19.03321

-----

### 3\. K-Fold Cross-Validation

``` r
## set.seed(17)
cv.error.10 = rep(0, 10)
for (i in 1:10) {
  glm.fit = glm(mpg ~ poly(horsepower, i))
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}; rm(i)
cv.error.10
```

    ##  [1] 24.21382 19.15645 19.33689 19.49826 19.44817 19.18035 18.55281 19.07091
    ##  [9] 19.15979 19.53444

because of being called as 2nd bias-corrected ver, maybe Trimmed Mean?

-----

### 4\. Bootstrap

##### parameter accuracy estimation

``` r
alpha.fn = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X, Y))/(var(X) + var(Y) - 2*cov(X, Y)))
}
```

example

``` r
alpha.fn(Portfolio, 1:100)
```

    ## [1] 0.5758321

``` r
## set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))
```

    ## [1] 0.5317554

``` r
boot(Portfolio, alpha.fn, R = 1000)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = Portfolio, statistic = alpha.fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##      original        bias    std. error
    ## t1* 0.5758321 -0.0002038364  0.09042522

##### (Linear) model accuracy estimation

``` r
boot.fn = function(data, index) return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
boot.fn(Auto, 1:392)
```

    ## (Intercept)  horsepower 
    ##  39.9358610  -0.1578447

``` r
## set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
```

    ## (Intercept)  horsepower 
    ##  39.8473283  -0.1551322

``` r
boot.fn(Auto, sample(392, 392, replace = T))
```

    ## (Intercept)  horsepower 
    ##  41.0831260  -0.1692689

``` r
boot(Auto, boot.fn, 1000)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = Auto, statistic = boot.fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##       original       bias    std. error
    ## t1* 39.9358610  0.024541138 0.869562544
    ## t2* -0.1578447 -0.000209205 0.007513429

``` r
summary(lm(mpg ~ horsepower))$coef
```

    ##               Estimate  Std. Error   t value      Pr(>|t|)
    ## (Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
    ## horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81

model fix

``` r
boot.fn = function(data, index) coefficients(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index))
```

``` r
## set.seed(1)
boot(Auto, boot.fn, 1000)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = Auto, statistic = boot.fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##         original        bias     std. error
    ## t1* 56.900099702  4.944985e-02 2.0326333655
    ## t2* -0.466189630 -9.967915e-04 0.0324842735
    ## t3*  0.001230536  4.273351e-06 0.0001174206

``` r
summary(lm(mpg ~ horsepower + I(horsepower^2)))$coef
```

    ##                     Estimate   Std. Error   t value      Pr(>|t|)
    ## (Intercept)     56.900099702 1.8004268063  31.60367 1.740911e-109
    ## horsepower      -0.466189630 0.0311246171 -14.97816  2.289429e-40
    ## I(horsepower^2)  0.001230536 0.0001220759  10.08009  2.196340e-21
