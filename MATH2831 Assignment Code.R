# // Title: Squid size analysis
# // Author: Faiyam Islam
# // Student ID: 5258151

# Setting up working directory -------------------------------------------------

getwd()
# set up working directory
setwd("~/Downloads")
getwd()

# Q1(I)(a)

# library package used for regsubsets
library(leaps) 

squid <- read.table('squid.txt', header = T) 
best.subsets = regsubsets(y~., data = squid) 

# summary statistics of squid.txt
summary(best.subsets)

# Q1(I)(b)

# Best model according to PRESS and Mallows' Cp 

sm = summary(best.subsets) 
sm$cp
which.min(sm$cp)
sm$adjr2
which.max(sm$adjr2)

PRESS <= function(linear.model) {
  pr <- residuals(linear.model)/(1 - hatvalues(linear.model)) + 
    sum(pr^2)
}

PRESS(lm(y~x5, squid))
PRESS(lm(y~x4+x5, squid))
PRESS(lm(y~x2+x4+x5, squid))
PRESS(lm(y~x1+x2+x4+x5, squid))
PRESS(lm(y~., squid)) # model containing all the predictors

# Q1(II)(a)(i)

# Forward Selection 
null <- lm(y~1, data = squid) # linear model without any predictors
full <- lm(y~., data = squid) # linear model with all the predictors
stepAIC(null, scope = list(lower = null, upper = full), direction = 'forward')

# Q1(II)(b)(i)

# Backward Selection 
stepAIC(full, direction = 'backward')

# Q1(II)(c)

# Stepwise selection 
stepAIC(lm(y~x1, data = squid),
        scope = list(lower = null, upper = full), direction = 'both')

# Q1(III)(a)

squid_model <- lm(y~., data = squid) 

par(mfrow=c(2,2))
plot(squid_model)
par(mfrow=c(1,1))

# Q1(III)(b)

par(mfrow=c(2,2))
plot(lm(y~x4+x5, squid))
par(mfrow=c(1,1))

summary(lm(y~x4+x5, squid))

# END OF CODE ------------------------------------------------------------------