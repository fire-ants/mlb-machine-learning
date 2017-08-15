## Conduct analysis

# Mike Trout
batsTrout <- filter(atbat16, batter == "545361")
#FBs <- filter(pitch11, pitch_type == "FF" | pitch_type == "FC")

# inner join all pitches to specific batter
pitchesTrout <- collect(inner_join(pitch16, batsTrout))

# 3028 pitches thrown to Trout
nrow(pitchesTrout)


joined <- pitchesTrout %>% mutate(quant_score_des = get_quant_score(des),
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

###  Commenceth thou Machine Learning.   Predict how to minimize HitterVal, or alternatively, how to minimize type = X
#  Interesting variables from pitch
# tfs, tfs_zulu, id, px, pz, pitch_type, count, zone, nasty

# Interesting variables from atbat
# p_throws, inning_side