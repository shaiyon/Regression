# 1. Using the divusa dataset in the faraway package with divorce as the response and the other variables as predictors, implement the following variable selection methods to determine the "best" model:

library(faraway)
library(leaps)

## (a) Stepwise regression with AIC

model <- lm(divorce ~ year + unemployed + femlab + marriage + birth + military, 
            data=divusa)
reduced <- lm(divorce ~ 1, data=divusa)

step(reduced, scope = list(lower = reduced, upper = model))

### This metric suggests that a 5 parameter model that only excludes unemployed is the best model.

## (b) Best subsets regression with adjusted R squared 

mod <- regsubsets(subset(divusa, select=-c(divorce)), divusa$divorce)
summary.mod <- summary(mod)
summary.mod$which
summary.mod$adjr2

### This metric suggests that that the 3 parameter model is the best, as the adjusted R squared stops incresaing significantly with more parameters. But a case could be made for the 4 parameter model, depending on one considers a 0.01 increase in adjusted R squared to be significant.

## (c) Best subsets regression with adjusted Mallow's Cp

summary.mod$which
summary.mod$cp

### This metric suggests that the 5 parameter model that only excludes unemployed is the best model.

bestModel <- lm(divorce ~ year + femlab + marriage + birth + military, data=divusa)
summary(bestModel)

### While the 3 parameter model provides a high adjusted R squared value, the Mallow's Cp metric is still high, implying that there is significant bias introduced into the predicted response by the model being underspecified. Thus, the 5 parameter model including all predictors except unemployed is the "best" model.


# 2. Refer to the "Job proficiency" data posted on Gauchospace.

job <- read.csv("Job proficiency.csv")

## (a) Obtain the overall scatterplot matrix and the correlation matrix of the X variables. Draw conclusions about the linear relationship between Y and the predictors.

pairs(job)
cor(job)

### x3 and x4 have an immediately noticeable and strong positive linear relationship with the response. x1 has a slight positive linear relationships with y, while x2 does not seem to have a significant linear relationship with y.

## (b) Using only the first order terms as predictors, find the four best subset regression models according to the R squared criterion.

mod <- regsubsets(subset(job, select=-c(y)), job$y)
summary.mod <- summary(mod)
summary.mod$which
summary.mod$rsq

## (c) Since there is relatively little difference in R squared for the four best subset models, what other criteria would you use to help in the selection of the best models? Discuss.

### The best subset model based on adjusted R squared, the stepwise AIC method, Mallow's Cp metric + more could all help select the most optimal model.


# 3. Refer again to "Job proficiency" data from problem 2.

## (a) Using stepwise regression, find the best subset of predictor variables to predict job proficiency Use alpha limit of 0.05 to add or delete a variable.

baseline <- lm(job$y ~ 1)
add1(baseline, ~. + job$x1 + job$x2 + job$x3 + job$x4, test='F')

### Add x3 to the model, as it has the highest F value.

model <- update(baseline, ~. + job$x3)
add1(model, ~. + job$x1 + job$x2 + job$x4, test='F')

### Add x1 to the model, as it has the highest F value.

model <- update(model, ~. + job$x1)
add1(model, ~. + job$x2 + job$x4, test='F')

### Add x4 to the model, as the p value is low.

model <- update(model, ~. + job$x4)
add1(model, ~. + job$x2, test='F')

### Do not add x2 to the model, as the p value is high. Therefore, the model containing all the predictors except x2 is the final model.

model <- lm(y ~ x1 + x3 + x4, data=job)
summary(model)

## (b) How does the best subset obtained in part (a) compare with the best subset from part (b) of Q2?

### It is consistent with the findings in Q2, (b), as the change in R squared is very minimal when the last predictor (x2) is added to the model. This suggests that x2 does not add much predictive power to the model, and should be disregarded.


# 4. Refer to the "Brand preference" data posted on Gauchospace.

brand <- read.csv("brand preference.csv")

## (a) Obtain the studentized deleted residuals and identify any outlying Y observations.

model <- lm(y ~ x1 + x2, data=brand)
rs <- rstudent(model)
rs

plot(rs, main="Rough residual plot")
abline(0,0, col="red")

### There does not seem to be any significant outliers in the residuals.

## (b) Obtain the diagonal elements of the Hat matrix, and provide an explanation for any pattern in these values.

hat <- hatvalues(model)
hat

### There are 4 0.2375 values surrounding 8 0.1375 values. The diagonals measure the separation the values have to the mean, so it checks out that the first and last 4 are greater than the middle. 

## (c) Are any of the observations high leverage point?

p <- sum(hat)
n <- length(brand$y)

which(hat > (3*p)/n)

### No.

# 5. The data below shows, for a consumer finance company operating in six cities, the number of competing loan companies operating in the city (X) and the number per thousand of the company's loans made in that city that are currently delinquent (Y):

xi <- c(4, 1, 2, 3, 3, 4)
yi <- c(16, 5, 10, 15, 13, 22)
n <- length(xi)

## (a) The appropriate X matrix.

X <- matrix(c(rep(1, n), xi), nrow=n)
X

## (b) Vector b of estimated coefficients.

tXX <- matrix(c(n, sum(xi), sum(xi), sum(xi**2)), nrow=2)
tXY <- matrix(c(sum(yi), sum(xi*yi)), ncol=1)

b <- solve(tXX) %*% tXY
b

## (c) The Hat matrix H.

hat <- X %*% solve(tXX) %*% t(X)
hat


# 6. In stepwise regression, what advantage is there in using a relatively large alpha value to add variables? Comment briefly.

### A large value for alpha encourages more predictors to be added to the model than less, leading to the model having potentially increased predictive power. The statistician can manually add or remove borderline valuable predictors based on judgement rather than the stepwise regression removing them automatically.











