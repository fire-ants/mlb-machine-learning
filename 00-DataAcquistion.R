## load libraries

#install packages pitchRx, dplyr, stringr, lubridate

library(pitchRx)    ## thank you Carson Sievert!!!
library(dplyr)      ## thank you Hadley Wickham
library(stringr)
library(lubridate)

setwd("/db")
my_dbProd <- src_sqlite("pitchRxProd.sqlite3", create = FALSE)

#Create R dataframes from my_dbProd
pitch16 <- select(tbl(my_dbProd, "pitch"), gameday_link, num, des, type, tfs, tfs_zulu, id, end_speed, pitch_type, count, zone)
atbat16 <- select(tbl(my_dbProd, "atbat"), gameday_link, num, pitcher, batter, b_height, pitcher_name, p_throws, batter_name, stand, atbat_des, event, inning, inning_side)

#Jason's quantitative method for scoring plays
get_quant_score <- function(des) {
    score <- (
        as.integer(str_detect(des, "Called Strike")) * -(1/3) +
            as.integer(str_detect(des, "Foul")) * -(1/3) +
            as.integer(str_detect(des, "In play, run")) * 1.0 +
            as.integer(str_detect(des, "In play, out")) * -1.0 +
            as.integer(str_detect(des, "In play, no out")) * 1.0 +
            as.integer(str_detect(des, "^Ball$")) * 0.25 +
            as.integer(str_detect(des, "Swinging Strike")) * -(1/2.5) +
            as.integer(str_detect(des, "Hit By Pitch")) * 1.0 +
            as.integer(str_detect(des, "Ball In Dirt")) * 0.25 +
            as.integer(str_detect(des, "Missed Bunt")) * -(1/3) +
            as.integer(str_detect(des, "Intent Ball")) * 0.25
    )
    return(score)
}

#can this be added to above script? 
fix_quant_score <- function(event) {
    score <- (
        as.integer(str_detect(event, "Groundout")) * -2 +
            as.integer(str_detect(event, "Forceout")) * -2 +
            as.integer(str_detect(event, "Field Error")) * -2 
    )
    return(score)
}

#qualitative modifier based on play-by-play announcement text
get_qual_score <- function(des) {
    score <- (
        as.integer(str_detect(des, "homer")) * 2 +
            as.integer(str_detect(des, "line")) * 1.5 +
            as.integer(str_detect(des, "sharp")) * 1.5 +
            as.integer(str_detect(des, "grounds")) * -1 +
            as.integer(str_detect(des, "flies")) * -1 +
            as.integer(str_detect(des, "soft")) * -2 +
            as.integer(str_detect(des, "pop")) * -2 +
            as.integer(str_detect(des, "triples")) * 1.5 +
            as.integer(str_detect(des, "doubles")) * 1.0 +
            as.integer(str_detect(des, "error")) * 0.5
    )
    return(score)
}

# join filtered atbats to all pitches
pitchesJoin <- collect(inner_join(pitch16, atbat16))

# score Qual and Quant mutate
joined <- pitchesJoin %>% mutate(quant_score_des = get_quant_score(des),
                                 fix_quant_score = fix_quant_score(event) * (des == 'In play, run(s)'),
                                 quant_score = quant_score_des + fix_quant_score,
                                 qual_score = get_qual_score(atbat_des) * (type == 'X'),
                                 hitter_val = quant_score + qual_score)

#This portion of script prepares data for output into .csv for ingestion into python for building logistic regression model

# convert to factor variables
joined$pitch_type <- as.factor(joined$pitch_type) 
joined$des <- as.factor(joined$des) 
joined$type <- as.factor(joined$type)
joined$count <- as.factor(joined$count) 
joined$event <- as.factor(joined$event) 
joined$p_throws <- as.factor(joined$p_throws)
joined$zone <- as.factor(joined$zone)
joined$stand <- as.factor(joined$stand)
joined$inning <- as.factor(joined$inning)
joined$inning_side <- as.factor(joined$inning_side)

# dimensionality reduciton -- convert FS and FT to SInkers
levels(joined$pitch_type)[levels(joined$pitch_type)=="FS"] <- "SI"
levels(joined$pitch_type)[levels(joined$pitch_type)=="FT"] <- "SI"
levels(joined$pitch_type)[levels(joined$pitch_type)=="FC"] <- "SL"
levels(joined$pitch_type)[levels(joined$pitch_type)=="KC"] <- "KN"

# Decide Good (1) or Bad (0) -- if the hitterval score is negative, we award this as "1" aka, good for pitcher
joined.classic <- joined %>% mutate(hv_binary = ifelse(hitter_val < 0, 1, 0))

#categorical data prep - create zone and pitch type pairs for one-hot encoding -- credit to Benita Mordi for this idea!!
joined.classic <- joined.classic %>% mutate(ptz=paste(pitch_type,zone, sep = "_"))

#reduce feature space - remove infrequent pitch types
joined.classic.pitchedit <- joined.classic %>% filter(pitch_type != c('EP','FO','PO','SC'))

#export features of interest, with hv_binary label 1 if <0, else 0
  var.interest <- joined.classic.pitchedit %>% select(12,13,28,29)
#write.csv(var.interest, file = paste(format(Sys.Date(), "HVal-%Y-%m-%d"), "csv", sep = "."))
write.csv(var.interest, file = "rawdata_ML.csv")

