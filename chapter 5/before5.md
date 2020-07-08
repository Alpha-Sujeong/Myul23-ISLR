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

store unique data sets.

``` r
write.csv(Auto, "Auto.csv", row.names = F, quote = F)
write.csv(Portfolio, "Portfolio.csv", row.names = F, quote = F)
```

-----

### 1\. Validation set Approach

``` r
lm.fit = lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
```

    ## [1] 27.44976

``` r
lm.fit2 = lm(mpg ~ poly(horsepower, 2), subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
```

    ## [1] 22.73279

``` r
lm.fit3 = lm(mpg ~ poly(horsepower, 3), subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
```

    ## [1] 22.69013

-----

### 2\. Leave-One-Out Cross-Validation

``` r
glm.fit = glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)
```

    ## (Intercept)  horsepower 
    ##  39.9358610  -0.1578447

``` r
glm.fit = glm(mpg ~ horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta
```

    ## [1] 24.23151 24.23114

2 as basic cv divide 2?

``` r
cv.error = rep(0, 5)
for (i in 1:5) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
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
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}; rm(i)
cv.error.10
```

    ##  [1] 24.22516 19.19274 19.75959 19.24482 18.98441 19.09542 19.27776 19.67650
    ##  [9] 19.19299 19.57180

because of being called as 2nd bias-corrected ver, maybe Trimmed Mean?

-----

### 4\. Bootstrap

##### parameter accuracy estimation

example

``` r
alpha.fn(Portfolio, 1:100)
```

    ## [1] 0.5758321

``` r
## set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))
```

    ## [1] 0.4981721

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
    ##      original      bias    std. error
    ## t1* 0.5758321 0.005416105  0.08911611

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
    ##  38.8444605  -0.1513271

``` r
boot.fn(Auto, sample(392, 392, replace = T))
```

    ## (Intercept)  horsepower 
    ##  39.0509455  -0.1497487

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
    ##       original        bias    std. error
    ## t1* 39.9358610  0.0094165758 0.878756221
    ## t2* -0.1578447 -0.0002203993 0.007524357

``` r
summary(lm(mpg ~ horsepower))$coef
```

    ##               Estimate  Std. Error   t value      Pr(>|t|)
    ## (Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
    ## horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81

model fix

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
    ## t1* 56.900099702  1.082937e-01 2.0840149861
    ## t2* -0.466189630 -1.795473e-03 0.0333259606
    ## t3*  0.001230536  7.086850e-06 0.0001206793

``` r
summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef
```

    ##                     Estimate   Std. Error   t value      Pr(>|t|)
    ## (Intercept)     56.900099702 1.8004268063  31.60367 1.740911e-109
    ## horsepower      -0.466189630 0.0311246171 -14.97816  2.289429e-40
    ## I(horsepower^2)  0.001230536 0.0001220759  10.08009  2.196340e-21
