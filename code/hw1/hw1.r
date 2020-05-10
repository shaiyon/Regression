# PSTAT 126 Homework 1 - Shaiyon Hariri


# Question 1

htwt <- read.csv("Htwt.csv")

# (a)
# Predictor: height
# Response: weight

# (b)
plot(x=htwt$ht, y=htwt$wt, xlab="Height", ylab="Weight", main="Htwt Scatterplot")
# There are some outliers, but the data seems to follow a linear trend of weight increasing as height increases. So yes.

# (c)
x <- htwt$ht
y <- htwt$wt
xbar <- mean(x)
ybar <- mean(y)

Sxx <- sum((x - xbar)^2)
Syy <- sum((y - ybar)^2)
Sxy <- sum((x - xbar)*(y - ybar))

b1 <- Sxy/Sxx
b0 <- ybar - b1*xbar  

cat("xbar:", xbar, " ybar:", ybar, " Sxx:", Sxx, " Syy:", Syy, " Sxy:", Sxy)

abline(b0, b1)


# Question 2

UBSprices <- read.csv("UBSprices.csv")

# (a)
plot(x=UBSprices$bigmac2003, y=UBSprices$bigmac2009, xlab="Big Mac 2003", ylab="Big Mac 2009", main="Big Mac Scatterplot")
# There is a large cluster of points in the bottom left corner, and it fans out and becomes more scarce as you traverse the axes. 
# This clustering suggests that the data may have an exponential trend, and a normalizing function would be useful for modeling.

# (b)
plot(x=log(UBSprices$bigmac2003), y=log(UBSprices$bigmac2009), xlab="Big Mac 2003", ylab="Big Mac 2009", main="Big Mac Log Scatterplot")
# Compared to the plot in (a), the data is much more evenly distributed across the plot, and a regression line would give us more insight in this situation.

# (c)
x = log(UBSprices$bigmac2003)
y = log(UBSprices$bigmac2009)
xbar <- mean(x)
ybar <- mean(y)

Sxx <- sum((x - xbar)^2)
Syy <- sum((y - ybar)^2)
Sxy <- sum((x - xbar)*(y - ybar))

b1 <- Sxy/Sxx
b0 <- ybar - b1*xbar

abline(b0, b1)


# Question 3

library(faraway)

# (a)
plot(x=prostate$lcavol, y=prostate$lpsa, xlab="lcavol", ylab="lpsa", main="Prostate Scatterplot")

xbar <- mean(prostate$lcavol)

lpsa_on_lcavol <- lm(formula=lpsa~lcavol, data=prostate)
lcavol_on_lpsa <- lm(formula=lcavol~lpsa, data=prostate)

# (b)
abline(coef(lpsa_on_lcavol)["(Intercept)"], coef(lpsa_on_lcavol)["lcavol"])
# Solve equation for x
abline(-coef(lcavol_on_lpsa)["(Intercept)"]/coef(lcavol_on_lpsa)["lpsa"], 1/coef(lcavol_on_lpsa)["lpsa"])
# The lines intersect at (xbar, ybar), as every regression line must pass through that point


# Question 4

heights <- read.csv("Heights.csv")

# (a)
model <- lm(formula=dheight~mheight, data=heights)
plot(x=heights$mheight, y=heights$dheight, xlab="mheight", ylab="dheight", main="Heights Scatterplot")
abline(model)

# (b)
x = heights$mheight
y = heights$dheight
xbar <- mean(x)
ybar <- mean(y)

Sxx <- sum((x - xbar)^2)
Syy <- sum((y - ybar)^2)
Sxy <- sum((x - xbar)*(y - ybar))

rxy <- Sxy/sqrt(Sxx*Syy)
cat("rxy: ", rxy)
# The correlation coefficient implies a moderate positive linear relationship between mheight and dheight


