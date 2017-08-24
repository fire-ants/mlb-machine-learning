## Tests fit for: Logistic Regression, LDA, QDA, and KNN

    library(pitchRx)
    library(dplyr)      
    library(stringr)
    library(ggplot2)    
##  library(ISLR)
    
    names(joined)
    dim(joined)
    summary(joined)
    ##pairs(joined)  ## plots all data points against each other correlations
    ##cor(joined)  ## plots all  correlations
    ##cor(joined[,-9]) ## excludes non-numeric terms
    attach(joined)
    plot() ## insert specific variable to plot

    ## redefine hitter_val as good or bad outcome
    joined.temp<-mutate(joined,Outcome=ifelse(hitter_val<1, 1,0))
    
    ## exclude non relevant columns and create test and train data sets
    nzv <- nearZeroVar(joined.temp)
    joined.temp2 <- joined.temp[,-nzv]
    
    mostlyNA <- sapply(joined.temp2, function(x) mean(is.na(x))) > 0.95
    joined.temp2 <- joined.temp2[,mostlyNA == F]
    
    joined.temp2<- joined.temp2[-c(1:9,17,19,20,23,24,25)]
    
    joined.temp2$p_throws <- as.factor(joined.temp2$p_throws)
    joined.temp2$inning_side <- as.factor(joined.temp2$inning_side)
    
    n = nrow(joined.temp2)
    joined.index = sample(1:n, size = round(0.7*n), replace=FALSE)
    joined.train = joined.temp2[joined.index ,]
    joined.test = joined.temp2[-joined.index ,]
    
    
    
# Logistic Regression

    ##glm.fits=glm(type~tfs+tfs_zulu+id+px+pz+pitch_type+count+zone+nasty+p_throws+inning_side,data=joined.train,family=binomial, subset=joined.train)
    
    glm.fits=glm(Outcome~px+pz+pitch_type+count+zone+nasty+p_throws+inning_side,data=joined.train,family=binomial)
    ##glm.fits=glm(type~px+pz+pitch_type+count+zone+nasty+p_throws+inning_side+data=joined,family=binomial, subset=1:2120)
    
    summary(glm.fits) ##  summary(glm.fits)$coef
    coef(glm.fits)    ##  summary(glm.fits)$coef[,4]
    
    
    glm.probs=predict(glm.fits,type="response") ## predicts the outcome
    ##contrasts(joined.temp$Outcome)
    
    glm.pred=rep(1,2120)
    glm.pred[glm.probs>.5]=0
    table(glm.fits,Outcome)
    
    
    mean(glm.pred==type)
    mean(glm.pred!=Direction.2005)
    
    
    ##predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")




# Linear Discriminant Analysis

    library(MASS)
    lda.fit=lda(type~px+pz+pitch_type+zone+nasty+p_throws,data=joined.train)
    lda.fit
    plot(lda.fit)
    lda.pred=predict(lda.fit, joined.train)
    names(lda.pred)
    lda.class=lda.pred$class
    table(lda.class,joined.train)
    mean(lda.class==joined.train)
    sum(lda.pred$posterior[,]>=)
    sum(lda.pred$posterior[,]<)
    lda.pred$posterior[]
    lda.class[1:20]
    sum(lda.pred$posterior[,1]>.9)

# Quadratic Discriminant Analysis

    qda.fit=qda(type~px+pz+pitch_type+zone+nasty+p_throws,data=joined.train)
    qda.fit
    qda.class=predict(qda.fit,joined.train)$class
    table(qda.class,joined.train)
    mean(qda.class==joined.train)

# K-Nearest Neighbors

library(class)
    type.train=type[joined.train]
    set.seed(1)
    knn.pred=knn(joined.train,joined.test,train.Direction,k=1)
    table(knn.pred,type.train)
    knn.pred=knn(joined.train,joined.test,type.train,k=3)
    table(knn.pred,type.train)
    mean(knn.pred==type.train)

    
    ## battles testing
    library(caret)
    library(rpart)
    library(knitr)
    library(randomForestSRC)
    library(randomForest)
    library(e1071)
    set.seed(21)
    
    ##joined.test$hitter_val<-as.factor(joined.test$hitter_val)
    
    fitControlRF <- trainControl(method="cv", number = 5, verboseIter = FALSE)
    mdlRF <- train(hitter_val ~ ., data=joined.train, method = "rf", na.action = na.omit)
    predRF <- predict(mdlRF, newdata = joined.test, type ="prob")
    cmRF <- confusionMatrix(predRF,joined.test$hitter_val)
    

    