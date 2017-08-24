library(MASS)
library(ISLR)
fix(Boston)
names(Boston)
attach(Boston)
lm.fit=lm(medv~lstat, data = Boston) #linear regression equation functon lm(response~predictor, data)
lm.fit
summary(lm.fit) #pulls summary of the defined regression
names(lm.fit)
coef(lm.fit) #returns the estimated coefficients of the regression

##obtains the confidence interval for the estimated coefficients 
    confint(lm.fit) 

##produce confidence and prediction intervals for predicting the response for several given values of the predictor
    predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
    predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")
    
## Plot response and predictor 
    plot(lstat,medv)
    abline(lm.fit)
    
## reduce width of regression line by factor of 3, pch changes plotting symbols
    abline(lm.fit,lwd=3)
    abline(lm.fit,lwd=3,col="red")
    plot(lstat,medv,col="red")
    plot(lstat,medv,pch=20)
    plot(lstat,medv,pch="+")
    plot(1:20,1:20,pch=1:20)
    
##Examine diagnostic plots, par plots multiple plots in same display
    par(mfrow=c(2,2))
    plot(lm.fit)
    
##residuals plot 
    plot(predict(lm.fit), residuals(lm.fit))
    plot(predict(lm.fit), rstudent(lm.fit))
    
##plot leverage statistics, which max tells which observations had the largest leverage
    plot(hatvalues(lm.fit))
    which.max(hatvalues(lm.fit))
    
## Multiple Linear Regression
    
    lm.fit=lm(medv~lstat+age,data=Boston)
    summary(lm.fit)
    lm.fit=lm(medv~.,data=Boston) ## selects all variables in data frame 
    summary(lm.fit)
    summary(lmfit)$ #returns specific value
    
    install.packages("CARBayesdata")
    library(car)

#calculates  variance inflation factor   
    vif(lm.fit)
    lm.fit1=lm(medv~.-age,data=Boston)
    
    summary(lm.fit1)
    lm.fit1=update(lm.fit, ~.-age) #updates the regression to exclude age
    
# Interaction Terms
    
    summary(lm(medv~lstat*age,data=Boston)) # *adds predictors individually and their interactions, : adds just interactions
    
# Non-linear Transformations of the Predictors, addresses non-linearity
    
    lm.fit2=lm(medv~lstat+I(lstat^2))
    summary(lm.fit2)
    lm.fit=lm(medv~lstat)
    anova(lm.fit,lm.fit2) ##quanitifies the extent to which the quadratic fit is superior to the linear model
    
#shows evidence of nonlinearity, uses polynomial and log transforms
    par(mfrow=c(2,2))
    plot(lm.fit2)
    lm.fit5=lm(medv~poly(lstat,5))
    summary(lm.fit5)
    summary(lm(medv~log(rm),data=Boston))
    
    
    
    # Qualitative Predictors, 
    
    fix(Carseats)
    names(Carseats)
    lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
    summary(lm.fit)
    attach(Carseats)
    contrasts(ShelveLoc) #Dummy variables are created for qualititive predictors
    
    # Writing Functions
    
    LoadLibraries
    LoadLibraries()
    LoadLibraries=function(){
        library(ISLR)
        library(MASS)
        print("The libraries have been loaded.")
    }
    LoadLibraries
    LoadLibraries()