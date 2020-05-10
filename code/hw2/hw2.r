
# Question 1

wblake <- read.csv("wblake.csv")

## (a)

n <- length(wblake$Age)
x <- wblake$Age
y <- wblake$Length
xbar <- mean(x)
ybar <- mean(y)

Sxx <- sum((x - xbar)^2)
Syy <- sum((y - ybar)^2)
Sxy <- sum((x - xbar)*(y - ybar))

b1 <- Sxy/Sxx
b0 <- ybar - b1*xbar  
yhat <- b0 + b1*x

e <- y - yhat
sigma2hat <- sum(e^2)/(n-2)
sigmahat <- sqrt(sigma2hat)

ssto <- sum((y-ybar)^2)
sse <- sum((y-yhat)^2)
ssr <- sum((yhat-ybar)^2)
r2 <- ssr/ssto

plot(x, y, xlab="Age", ylab="Length")
abline(b0, b1)
cat("yhat:", b0, "+", b1, "* x", " standard error:", sigmahat, " variance:", sigma2hat, " r2:", r2)

### The R squared is very high, suggesting that Age accounts for a lot (81.65%) of the variation in Length. b1 is positive, and the graph shows an increasing slope, so as age increases, length also increases.

## (b)

se_b1 <- sigmahat / sqrt(Sxx)
t_pct <- qt(p = 0.99,df = n-2)
ci_B1_99 <- b1 + c(-1, 1) * t_pct * se_b1
cat("99% Confidence interval for B1:", ci_B1_99)

### This interval is small, so we can say with relative certainty that for each year older a small mouth bass is, it will be approximately 30 mm longer.

## (c)

ynew <- yhat[x=1]
se_age1 <- sigmahat * sqrt(1 + 1/n + (1-xbar)^2/Sxx)
t_pct <- qt(p = 0.995, df = n-2)
ci_age1_99 <- ynew + c(-1, 1) * t_pct * se_age1
cat("Ynew:", ynew, " 99% Prediction interval for Age=1:", ci_age1_99)

### This is a large interval, from tiny (25 mm) to medium sized fish (170mm). This large interval is due to the high variance of length and the small significance level, making pinpointing one estimated value with certainty difficult.


# Question 2

heights <- read.csv("Heights.csv")

## (a)

n <- length(heights$mheight)
x = heights$mheight
y = heights$dheight
xbar <- mean(x)
ybar <- mean(y)

Sxx <- sum((x - xbar)^2)
Syy <- sum((y - ybar)^2)
Sxy <- sum((x - xbar)*(y - ybar))
b1 <- Sxy/Sxx
b0 <- ybar - b1*xbar  
yhat <- b0 + b1*x

e <- y - yhat
sigma2hat <- sum(e^2)/(n-2)
sigmahat <- sqrt(sigma2hat)

ssto <- sum((y-ybar)^2)
sse <- sum((y-yhat)^2)
ssr <- sum((yhat-ybar)^2)
r2 <- ssr/ssto

plot(x, y, xlab="mheight", ylab="dheight")
abline(b0, b1)
cat("yhat:", b0, "+", b1, "* x", " standard error:", sigmahat, " variance:", sigma2hat, " r2:", r2)

## (b)

### Bo is the population intercept and B1 is the population coefficient for mheight. This means that when mheight is 0, dheight will be the value of Bo, but as height cannot be zero or very close to it, this result cannot have significant insights extracted from it. However, the value of B1 implies a positive slope of the regression line, and that for every 1 value of mheight, dheight will increase by B1.  

## (c)

ynew <- yhat[x=64]
se_mh64 <- sigmahat * sqrt(1 + 1/n + (1-xbar)^2/Sxx)
t_pct <- qt(p = 0.995, df = n-2)
ci_mh64_99 <- ynew + c(-1, 1) * t_pct * se_age1
cat("Ynew:", ynew, " 99% Prediction interval for mheight=64:", ci_mh64_99)

# Question 3

## (d)

n <- 100
x <- seq(0, 1, length=n)
y <- 1 + 2*x + rnorm(n)
X <- cbind(1, x)
Y <- matrix(y)
solve(t(X)%*%X)%*%t(X)%*%Y
lm(y ~ x)$coefficients

# Question 4

library(alr4)

## (a)

### Points above the line have had an increase in the price of rice relative to a typical worker's wages from 2003 to 2009, and points below the line have had a decrease in price.

## (b)

diff <- UBSprices$rice2009 - UBSprices$rice2003
maxprice <- UBSprices[which(diff == max(diff)),]
minprice <- UBSprices[which(diff == min(diff)),]

cat("Largest increase in rice price:", row.names(maxprice), " Largest decrease in rice price:", row.names(minprice))

## (c)

### The data does not seem to be distributed in a way that can be modeled lienarly (e.g.: normal). It clusters in the bottom left of the graph and fans out as x axis increases, 

## (d)

### The graph using the log scales shows that the data isn't clustered in the same way as the first graph, and is more normally distributed around the regression line. The residuals will be lower as a result, leading to a better model with better predictive power.














