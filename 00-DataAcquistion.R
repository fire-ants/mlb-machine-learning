## load libraries

library(pitchRx)    ## thank you Carson Sievert!!!
library(dplyr)      ## thank you Hadley Wickham
library(stringr)
library(lubridate)

## Use dplyer to create SQLite database
#library(dplyr)
#my_db2016 <- src_sqlite("pitchRx2016.sqlite3", create = TRUE)
my_dbProd <- src_sqlite("pitchRxProd.sqlite3", create = TRUE)

Today <- Sys.Date()
ThirtyDaysAgo <- Today - 30

#confirm empty
#my_db2016
my_dbProd


## scrape 2016 game data and store in the database
#library(pitchRx)
#scrape(start = "2016-04-03", end = "2016-11-02", suffix = "inning/inning_all.xml", connect = my_db1$con)
#scrape(start = "2016-04-01", end = "2016-10-31", suffix = "inning/inning_all.xml", connect = my_db2016$con)
scrape(start = ThirtyDaysAgo, end = Today, suffix = "inning/inning_all.xml", connect = my_dbProd$con)


# To speed up execution time, create an index on these three fields.
library("dbConnect", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

dbSendQuery(my_dbProd$con, "CREATE INDEX url_atbat ON atbat(url)") 
dbSendQuery(my_dbProd$con, "CREATE INDEX url_pitch ON pitch(url)")
dbSendQuery(my_dbProd$con, "CREATE INDEX pitcher_index ON atbat(pitcher_name)")
dbSendQuery(my_dbProd$con, "CREATE INDEX des_index ON pitch(des)")

pitch16 <- select(tbl(my_dbProd, "pitch"), gameday_link, num, des, type, tfs, tfs_zulu, id, end_speed, pitch_type, count, zone)
atbat16 <- select(tbl(my_dbProd, "atbat"), gameday_link, num, pitcher, batter, b_height, pitcher_name, p_throws, batter_name, stand, atbat_des, event, inning, inning_side)

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

fix_quant_score <- function(event) {
    score <- (
        as.integer(str_detect(event, "Groundout")) * -2 +
            as.integer(str_detect(event, "Forceout")) * -2 +
            as.integer(str_detect(event, "Field Error")) * -2 
    )
    return(score)
}

## begin subsetting
#mlbID <- '514888'

#hitters <- c('514888','453568','457759','519317','458015','547180','592450','545361','457705','502671','518626','502517','518934','471865','592178','519346','460075')
#hitters <- c('514888')

#TargetedAtBats <- filter(atbat16, batter == mlbID)
#TargetedAtBats <- atbat16

# join filtered atbats to all pitches
pitchesJoin <- collect(inner_join(pitch16, atbat16))

# score Qual and Quant mutate
joined <- pitchesJoin %>% mutate(quant_score_des = get_quant_score(des),
                                 fix_quant_score = fix_quant_score(event) * (des == 'In play, run(s)'),
                                 quant_score = quant_score_des + fix_quant_score,
                                 qual_score = get_qual_score(atbat_des) * (type == 'X'),
                                 hitter_val = quant_score + qual_score)

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

# convert FS and FT to SInkers 
levels(joined$pitch_type)[levels(joined$pitch_type)=="FS"] <- "SI"
levels(joined$pitch_type)[levels(joined$pitch_type)=="FT"] <- "SI"
levels(joined$pitch_type)[levels(joined$pitch_type)=="FC"] <- "SL"
levels(joined$pitch_type)[levels(joined$pitch_type)=="KC"] <- "KN"

# Decide Good (1) or Bad (0)
joined.classic <- joined %>% mutate(hv_binary = ifelse(hitter_val < 0, 1, 0))

#create zone and pitch type pairs
joined.classic <- joined.classic %>% mutate(pitch_type_zone=paste(pitch_type,zone, sep = "_"))

#remove infrequent pitch types
#joined.classic %>% filter(pitch_type == c(EP,FO,PO,SC))

#view missing data
#visna(joined.classic, tp = TRUE, col = "blue")

#create subsets of pitcher stance and batter stance 
Rh <- joined.classic %>% filter(stand == "R")
Lh <- joined.classic %>% filter(stand == "L")

Rpitch <- joined.classic %>% filter(p_throws == "R")
Lpitch <- joined.classic %>% filter(p_throws =="L")

RhRp <- Rh %>% filter(p_throws == "R")
RhLp <- Rh %>% filter(p_throws == "L")
LhRp <- Lh %>% filter(p_throws == "R")
LhLp <- Lh %>% filter(p_throws == "L")

#Primary Component Plots... need to update table names per above convention
#PCP <- ggparcoord(data = joined_classic[order(joined_classic$hv_binary, decreasing = FALSE),], columns = c(40,46,30,16,17,32,88), groupColumn = "hv_binary", title = "Factors v Pitcher Outcome", alpha = .01) PCP_cat <- ggparcoord(data = joined.temp[order(joined.temp$GoodBadQual, decreasing = TRUE),], columns = c(40,46,30,32,88), groupColumn = "GoodBadQual", title = "Categorical Factors v Pitcher Outcome")
RpRh_pcp <- ggparcoord(data = RhRp[order(RhRp$hv_binary, decreasing = FALSE),], columns = c(8,9,11,14,27,28), groupColumn = "hv_binary", title = "RpRh PCP v Pitcher Outcome")
#RpLh_pcp <- ggparcoord(data = RpitchLh[order(RpitchLh$GoodBadQual, decreasing = TRUE),], columns = c(16,17,30,32,40,46,88), groupColumn = "GoodBadQual", title = "RpLh PCP v Pitcher Outcome")
#LpRh_pcp <- ggparcoord(data = LpitchRh[order(LpitchRh$GoodBadQual, decreasing = TRUE),], columns = c(16,17,30,32,40,46,88), groupColumn = "GoodBadQual", title = "LpRh PCP v Pitcher Outcome")
#LpLh_pcp <- ggparcoord(data = LpitchLh[order(LpitchLh$GoodBadQual, decreasing = TRUE),], columns = c(16,17,30,32,40,46,88), groupColumn = "GoodBadQual", title = "LpLh PCP v Pitcher Outcome")

#create data for Swinging Strikes outside the strike zone. 
SS_NonSZ_Rh <- Rpitch %>% filter (des == "Swinging Strike" & zone == c(11,12,13,14))
SS_NonSZ_Lh <- Lpitch %>% filter (des == "Swinging Strike" & zone == c(11,12,13,14))

