## load required libraries

    install.packages('mi')
    install.packages('extracat')

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
    #library(MASS)
    library(tidyr)
    library(dplyr)
    library(GGally) #for parallel coordinate plot
    library(extracat)

set.seed(2121)

## manipulate data
## redefine hitter_val as good or bad outcome.. note mod to hitterval logic here as excluding hitter_val btw 0 and 1 [since removed] 
    #joined_hvrefined <- joined %>% filter(hitter_val >=1 | hitter_val <0)

    joined_classic <- joined %>% mutate(hv_binary = ifelse(hitter_val<0, 1, 0))
    
    #joined.temp<-mutate(joined_hvrefined,GoodBadQuant=ifelse(hitter_val<0, 1,0))
    #joined.temp<-mutate(joined.temp,GoodBadQual=ifelse(hitter_val<0, "Good Outcome","Bad Outcome"))
 

## exclude non relevant columns and create test and train data sets
    #nzv <- nearZeroVar(joined.temp)
    #joined.temp2 <- joined.temp[,-nzv]
  
  ### Colin Edit ###  
    
    #visualize missing values using visna (aka "visualize na") from extracat package
    visna(joined_classic, tp = TRUE, col = "blue")
    
    #joined.temp$GoodBadQual <- as.factor(joined.temp$GoodBadQual)
    
    #create subsets of pitcher stance and batter stance 
    Rh <- joined.temp %>% filter(stand = "R")
    Lh <- joined.temp %>% filter(stand = "L")
    
    Rpitch <- joined.temp %>% filter(p_throws == "R")
    Lpitch <- joined.temp %>% filter(p_throws =="L")
    
    RhRp <- Rh %>% filter(p_throws == "R")
    RhLp <- Rh %>% filter(p_throws == "L")
    LhRp <- Lh %>% filter(p_throws == "R")
    LhLp <- Lh %>% filter(p_throws == "L")

    #Primary Component Plots... need to update table names per above convention
    PCP <- ggparcoord(data = joined_classic[order(joined_classic$hv_binary, decreasing = FALSE),], columns = c(40,46,30,16,17,32,88), groupColumn = "hv_binary", title = "Factors v Pitcher Outcome", alpha = .01) PCP_cat <- ggparcoord(data = joined.temp[order(joined.temp$GoodBadQual, decreasing = TRUE),], columns = c(40,46,30,32,88), groupColumn = "GoodBadQual", title = "Categorical Factors v Pitcher Outcome")
    RpRh_pcp <- ggparcoord(data = RpitchRh[order(RpitchRh$GoodBadQual, decreasing = TRUE),], columns = c(16,17,30,32,40,46,88), groupColumn = "GoodBadQual", title = "RpRh PCP v Pitcher Outcome")
    RpLh_pcp <- ggparcoord(data = RpitchLh[order(RpitchLh$GoodBadQual, decreasing = TRUE),], columns = c(16,17,30,32,40,46,88), groupColumn = "GoodBadQual", title = "RpLh PCP v Pitcher Outcome")
    LpRh_pcp <- ggparcoord(data = LpitchRh[order(LpitchRh$GoodBadQual, decreasing = TRUE),], columns = c(16,17,30,32,40,46,88), groupColumn = "GoodBadQual", title = "LpRh PCP v Pitcher Outcome")
    LpLh_pcp <- ggparcoord(data = LpitchLh[order(LpitchLh$GoodBadQual, decreasing = TRUE),], columns = c(16,17,30,32,40,46,88), groupColumn = "GoodBadQual", title = "LpLh PCP v Pitcher Outcome")
    PCP
    RpRh_pcp
    RpLh_pcp
    LpLh_pcp
    LpRh_pcp
    
    

    
    #export features of interest, with hv_binary label 1 if <0, else 0
    var.interest <- joined_classic %>% select(id, tfs_zulu, inning_side.x, inning.x, pitcher_name, p_throws, batter_name, stand, count, pitch_type, zone, hitter_val, hv_binary)
    write.csv(var.interest, file = "data_export_hv_binary_distinct_300_days.csv")

    #End Colin Edit
    
    
    
    #Random tree classification in R follows
    
    
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
    
