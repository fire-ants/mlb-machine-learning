## load required libraries

    #install.packages('mi')
    #install.packages('extracat')

    library(pitchRx)
    library(plyr)
    library(dplyr)      
    library(stringr)
    library(ggplot2)
    library(boot)
    library(tree)
    library(ISLR)
    library(randomForest)
    library(gbm)
    library(caret)
    library(rpart)
    library(rpart.plot)
    library(rattle)					# Fancy tree plot
   	library(RColorBrewer)				# Color selection for fancy tree plot
    library(party)					# Alternative decision tree algorithm
    library(partykit)				# Convert rpart object to BinaryTree
    library(knitr)
    library(gbm)
    library(ipred)
    library(e1071)
    library(MASS)
    library(tidyr)
    library(dplyr)
    library(GGally) #for parallel coordinate plot



set.seed(2121)

## manipulate data
## redefine hitter_val as good or bad outcome
    joined.temp<-mutate(joined,GoodBadQuant=ifelse(hitter_val<1, 1,0))
    joined.temp<-mutate(joined.temp,GoodBadQual=ifelse(hitter_val<1, "Good","Bad"))
    

## exclude non relevant columns and create test and train data sets
    #nzv <- nearZeroVar(joined.temp)
    #joined.temp2 <- joined.temp[,-nzv]
  
  ### Colin Edit ###  
    
    visna(joined, tp = TRUE, col = "blue")
    
    ggplot(joined, aes(x = zone, y = hitter_val)) + geom_tile()
    library(GGally)
    joined <- within(joined, goodbad <- factor(ifelse(hitter_val > 0, 1, 0)))
    Rpitch <- joined %>% filter(p_throws = "R")
    Rpitch <- joined %>% filter(p_throws == "R")
    Lpitch <- joined %>% filter(p_throws =="L")
    RpitchRh <- Rpitch %>% filter(stand == "R")
    RpitchLh <- Rpitch %>% filter(stand == "L")
    RpitchLh <- Rpitch %>% filter(stand == "L")
    LpitchLh <- Lpitch %>% filter(stand == "L")
    LpitchRh <- Lpitch %>% filter(stand == "L")
    LpitchRh <- Lpitch %>% filter(stand == "R")

    RpRh_pcp <- ggparcoord(data = RpitchRh[order(RpitchRh$goodbad),], columns = c(10,11,14:18,20:26,28:30,32,34,35,88), groupColumn = "goodbad", title = "RpRh PCP v Pitcher Outcome", alphaLines = 0.010)
    RpLh_pcp <- ggparcoord(data = RpitchLh[order(RpitchLh$goodbad),], columns = c(10,11,14:18,20:26,28:30,32,34,35,88), groupColumn = "goodbad", title = "RpLh PCP v Pitcher Outcome", alphaLines = 0.012)
    LpRh_pcp <- ggparcoord(data = LpitchRh[order(LpitchRh$goodbad),], columns = c(10,11,14:18,20:26,28:30,32,34,35,88), groupColumn = "goodbad", title = "LpRh PCP v Pitcher Outcome", alphaLines = 0.021)
    LpLh_pcp <- ggparcoord(data = LpitchLh[order(LpitchLh$goodbad),], columns = c(10,11,14:18,20:26,28:30,32,34,35,88), groupColumn = "goodbad", title = "LpLh PCP v Pitcher Outcome", alphaLines = 0.054)
    RpRh_pcp
    RpLh_pcp
    LpLh_pcp
    LpRh_pcp
    
    # convert to factor variables
    joined.temp3$zone <- as.factor(joined.temp3$zone)
    joined.temp3$p_throws <- as.factor(joined.temp3$p_throws)
    joined.temp3$inning_side <- as.factor(joined.temp3$inning_side)
    joined.temp3$GoodBadQual <- as.factor(joined.temp3$GoodBadQual) 
    
    n = nrow(joined.temp3)
    joined.index = sample(1:n, size = round(0.7*n), replace=FALSE)
    joined.train = joined.temp3[joined.index ,]
    joined.test = joined.temp3[-joined.index ,]
    
    #seperate data frame into RHP and LHP
    sub.joined.RHP<-joined.temp3%>%filter(p_throws=="R")
    sub.joined.LHP<-joined.temp3%>%filter(p_throws=="L")

#Fit classification tree using rpart function
    #GoodBadTree = rpart(GoodBadQual~pitch_type+end_speed+count+zone+nasty+inning_side, data=sub.joined.RHP, method = "class", control=rpart.control(minsplit=5, cp=0.001))
    GoodBadTree = rpart(GoodBadQual~pitch_type+end_speed+count+zone+nasty+inning_side, data=sub.joined.RHP, method = "class", control=rpart.control(minsplit=5, cp=0.0123457))
    printcp(GoodBadTree)# display the results 
    plotcp(GoodBadTree) # visualize cross-validation results 
    summary(GoodBadTree) # detailed summary of splits
    
    # plot tree 
    rpart.plot(GoodBadTree, uniform=TRUE, main ="Outcomes for Altuve", cex=0.6)
    
    #Calcuate Tree accuracy
    GoodBadTree.pred=predict(GoodBadTree,sub.joined.RHP,type="class")
    table(GoodBadTree.pred,sub.joined.RHP$GoodBadQual)
    mean(GoodBadTree.pred==sub.joined.RHP$GoodBadQual)

    #prune tree, plot pruned tree and check accuracy
    #prune.GoodBadTree=prune(GoodBadTree, cp = 0.0123457)
    #rpart.plot(prune.GoodBadTree, uniform=TRUE, main="Pruned Outcomes for Altuve", cex = 0.6)
    #pruneGoodBadTree.pred=predict(prune.GoodBadTree,sub.joined.RHP,type="class")
    #table(pruneGoodBadTree.pred,sub.joined.RHP$GoodBadQual)
    #mean(pruneGoodBadTree.pred==sub.joined.RHP$GoodBadQual)
    
    
    #define function to generate rules for classification tree end nodes 
    GoodBadTree.Rules <- function(model)
    {
        if (!inherits(model, "rpart")) stop("Not a legitimate rpart tree")
        #
        # Get some information.
        #
        frm     <- model$frame
        names   <- row.names(frm)
        ylevels <- attr(model, "ylevels")
        ds.size <- model$frame[1,]$n
        #
        # Print each leaf node as a rule.
        #
        for (i in 1:nrow(frm))
        {
            if (frm[i,1] == "<leaf>")
            {   cat("\n")
                cat(sprintf(" Rule number: %s ", names[i]))
                cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
                            ylevels[frm[i,]$yval], frm[i,]$n,
                            round(100*frm[i,]$n/ds.size), frm[i,]$yval2[,5]))
                GoodBadrule <- path.rpart(model,nodes=as.numeric(names[i]), print.it = TRUE)
                GoodBadrule2 <- unname(unlist(GoodBadrule)[-1])
                cat(sprintf("   %s\n", sep=""))
                
            }
        }
    }
     
    # call function
    GoodBadTree.Rules(GoodBadTree)
    
    # alternate method to summarize tree node results 
    GoodBadruleNF <- unname(unlist(path.rpart(GoodBadTree, nodes=as.numeric(row.names(GoodBadTree$frame)))))[-1]
    #GoodBadruleNF2 <- str_replace_all(GoodBadruleNF, pattern = ",", replacement = "_") #replaces commas
    GoodBadnode_data <- with(sub.joined.RHP, sub.joined.RHP[eval(parse(text=paste(GoodBadruleNF, collapse=" & "))), ])
    
