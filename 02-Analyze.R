## Conduct analysis

# Mike Trout
batsTrout <- filter(atbat16, batter == "518626")
#FBs <- filter(pitch11, pitch_type == "FF" | pitch_type == "FC")

# inner join all pitches to specific batter
pitchesTrout <- collect(inner_join(pitch16, batsTrout))

# add new columns for HitterVal
#joined <- pitchesTrout %>% mutate(quant_score = get_quant_score(des),
#                                  qual_score = get_qual_score(atbat_des) * (type == 'X'),
#                                  hitter_val = quant_score + qual_score)

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
