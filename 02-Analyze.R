## Conduct analysis

# filter AtBats for only the targeted hitter
#TargetBatter <- "514888"    ## Altuve
#TargetBatter <- "545361"    ## Trout
#batsTargetBatter <- filter(atbat072017, batter == TargetBatter)

#TargetBatter <- tbl_df(batsTargetBatter)
Pitcher_data <- tbl_df(pitch30days2017)
Batter_data <- tbl_df(atbat30days2017)

#TargetBatter <- rename(TargetBatter, tfs_zulu = start_tfs_zulu)
#Batter_data <- rename(Batter_data, start_tfs_zulu = tfs_zulu)
Batter_data <- rename(Batter_data, start_tfs_zulu = tfs_zulu)

joined_test11.7 <- inner_join(Pitcher_data, Batter_data, by = c('gameday_link'))


# Mike Trout
#batsTrout <- filter(atbat072017, batter == "545361")
# Jose Altuve 
#batsAltuve <- filter(atbat072017, batter == "1839905")

#FBs <- filter(pitch11, pitch_type == "FF" | pitch_type == "FC")

# inner join all pitches to specific batter
#pitchesTrout <- collect(inner_join(pitch072017, batsTrout))
#pitchesTargetBatter <- collect(inner_join(pitch072017, batsTargetBatter))

#Joining, by = c("url", "inning_side", "inning", "next_", "num", "gameday_link", "event_num", "play_guid")

# 3028 pitches thrown to Trout
# nrow(pitchesTrout)
nrow(pitchesTargetBatter)
nrow(joined_raw)


joined <- joined_raw %>% mutate(quant_score_des = get_quant_score(des),
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

#condense pitch types to fewer dimensions
levels(joined$pitch_type)[levels(joined$pitch_type)=="FS"] <- "SI"     
levels(joined$pitch_type)[levels(joined$pitch_type)=="FT"] <- "SI"     
levels(joined$pitch_type)[levels(joined$pitch_type)=="FC"] <- "SL"     
levels(joined$pitch_type)[levels(joined$pitch_type)=="KC"] <- "KN"

###  Commenceth thou Machine Learning.   Predict how to minimize HitterVal, or alternatively, how to minimize type = X
#  Interesting variables from pitch
# pitch_type, count, zone, nasty

# Interesting variables from atbat
# p_throws, inning_side
