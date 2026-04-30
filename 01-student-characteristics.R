library(tidyverse)

folder_wd = "N:/durable/projects/personality"
setwd(folder_wd)

source(paste0(folder_wd, "/personality-project-functions.R"))

# read full data
# four students have 2 responses
d_big5_anon_w_dups = read_csv2("big5_anon_med_w_duplicate_students.csv")
d_mbti_anon = read_csv2("mbti_anon_med.csv")

# calculate number of responses for appendix table
# note that for big 5, 4 students responded in H-2025 that were also in V-2026
d_big5_anon_w_dups %>% filter(!is.na(big5.chatty)) %>% count(cohort_name)
d_mbti_anon %>% filter(!is.na(mbti_personality)) %>% count(cohort_name)

d_big5_anon_w_dups %>% filter(person_id == "8001")

d_big5_anon = d_big5_anon_w_dups %>% distinct(person_id, .keep_all = TRUE) 

d_big5_anon %>% filter(!is.na(foreign)) %>% count(cohort_name)

# some background questions were on the baseline questionnaire
d_big5_anon %>% filter(is.na(foreign)) 
d_foreign_nomissing = d_big5_anon %>% filter(!is.na(foreign)) 
calc_prop_group(d_foreign_nomissing, foreign, cohort_name)
calc_prop(d_foreign_nomissing, foreign)

# find numbers for V-2026 separately, because of the double students
d_big5_anon_w_dups %>% filter(cohort_name == "V-2026") %>% count(foreign)

d_big5_anon %>% filter(is.na(education)) 
d_education_nomissing = d_big5_anon %>% filter(!is.na(education)) 
calc_prop_group(d_education_nomissing, education, cohort_name)
d_education_nomissing %>% count(education) %>% mutate(prop = n/sum(n)) 

d_big5_anon_w_dups %>% filter(cohort_name == "V-2026") %>% count(education)

d_big5_anon %>% filter(is.na(mover)) 
d_mover_nomissing = d_big5_anon %>% filter(!is.na(mover)) 
calc_prop_group(d_mover_nomissing, mover, cohort_name)
calc_prop(d_mover_nomissing, mover)

d_big5_anon_w_dups %>% filter(cohort_name == "V-2026") %>% count(mover)

# read the non-anonymous data
# there may be fewer rows in the non-anonymous data 
# 3 students actively did not consent to having their data joined
# 7 students did not respond to the baseline questionnaire, 
# and therefore did not answer the q about consent. 
d_full = read_csv2("personality_nonanon.csv")
nrow(d_full)

# number of students
d_full %>% count(cohort_name)

# number that responded and consented
# remember that 4 students in V-2026 already responded in F-2025
# these students thus have duplicate observations
# not included to count total number of students, but included in the separate cohorts
d_full %>% filter(!is.na(mbti_personality)) %>% count(cohort_name)
d_full %>% filter(!is.na(big5.chatty)) %>% count(cohort_name)

# age and gender
d_full %>% 
  group_by(cohort_name) %>% 
  summarise(mean(age, na.rm = TRUE), sd(age, na.rm = TRUE))
d_full %>% 
  summarise(mean(age, na.rm = TRUE), sd(age, na.rm = TRUE))
 
calc_prop(d_full, gender)