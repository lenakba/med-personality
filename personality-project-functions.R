
#------------------- some figure theme objects
fig_width = 8
fig_height = 5
fig_width_broad = 10
fig_height_mbti = 6
# color theme
uio_soft_colors = c("#2EC483", "#86A4F7", "#FB6666", "#FEA11B", "#B2B3B7")
text_size=18
#---------- functions used in other scripts, some multiple times
# for calculating basic percentages
calc_prop = function(d, var){
  var = enquo(var)
  
  d_prop = d %>% 
    count(!!var) %>% 
    filter(!is.na(!!var)) %>% 
    mutate(denom = sum(n), prop = n/denom, perc = prop*100)
  d_prop
}

# add Big 5 subscale totals
calc_big5_tot = function(d){
  rev_vars = c("big5.cold", "big5.quiet", "big5.unorganized", "big5.rude",
               "big5.shy","big5.uncareful", "big5.not_artistic") 
  extrovert_vars = c("big5.chatty", "big5.quiet", "big5.outgoing", "big5.shy")
  openness_vars = c("big5.original", "big5.wild_imagination", "big5.speculate", "big5.not_artistic")
  agreeableness_vars = c("big5.cold", "big5.helpful", "big5.rude", "big5.considerate")
  conscientiousness_vars = c("big5.thorough", "big5.unorganized", "big5.planner", "big5.uncareful")
  
  d = d %>% mutate_at(rev_vars, ~7-.+1) 
  d = d %>% 
    mutate(big5_extrovert_tot = rowSums(across(all_of(extrovert_vars))),
           big5_openness_tot = rowSums(across(all_of(openness_vars))),
           big5_agreeableness_tot = rowSums(across(all_of(agreeableness_vars))),
           big5_conscientiousness_tot = rowSums(across(all_of(conscientiousness_vars))))
  
  d = d %>% 
    mutate(big5_extrovert_mean = rowMeans(across(all_of(extrovert_vars))),
           big5_openness_mean = rowMeans(across(all_of(openness_vars))),
           big5_agreeableness_mean = rowMeans(across(all_of(agreeableness_vars))),
           big5_conscientiousness_mean = rowMeans(across(all_of(conscientiousness_vars))))
  
  neuroticism_vars = c("big5.depressed", "big5.relaxed_rev", "big5.worried_a_lot", "big5.nervous")
  d = d %>% mutate(big5.relaxed_rev = 7-big5.relaxed+1) 
  d = d %>% mutate(big5_neuroticism_tot = rowSums(across(all_of(neuroticism_vars))))
  d = d %>% mutate(big5_neuroticism_mean = rowMeans(across(all_of(neuroticism_vars))))
  d
}

extract_mbti_dichotomies = function(d_mbti){
        d_mbti %>% mutate(mbti_extrovert = ifelse(str_detect(mbti_personality_text, "E"), "E", "I"),
                          mbti_intuitive = ifelse(str_detect(mbti_personality_text, "N"), "N", "S"),
                          mbti_feeling = ifelse(str_detect(mbti_personality_text, "F"), "F", "T"),
                          mbti_judging = ifelse(str_detect(mbti_personality_text, "J"), "J", "P"))
}

# fills the identification variables in the codebook
# adds labels to categorical variables that only have labels for the first and last value
clean_codebook = function(cb){
  fillable_vars = c("table_id", "variable_id", "type")
  cb = cb %>% fill(all_of(fillable_vars))
  cb = cb %>% mutate(cat_value_label = ifelse(
    type == "categorical" &
      !is.na(cat_value) &
      is.na(cat_value_label),
    cat_value,
    cat_value_label
  ))
  cb
}

# Comparing with Plaisant is more complicated. 
# We must first do a linear interpolation.
# x5= (x7–1)(4/6) + 1
interpolate = function(x7){
  x5 = ((x7-1)*(4/6)) + 1
  x5
}