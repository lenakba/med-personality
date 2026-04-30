library(tidyverse)
library(flextable)
library(officer)
library(devEMF)
library(readxl)

folder_wd = "N:/durable/projects/personality/"
setwd(folder_wd)
source(paste0(folder_wd, "personality-project-functions.R"))

options(scipen = 20)

d_others_big5 = read_excel("other_articles_personality.xlsx", sheet = "big-five")
d_others_mbti_binom = read_excel("other_articles_personality.xlsx", sheet = "mbti") %>% select(-ends_with("polish"))
d_others_mbti_16 = read_excel("other_articles_personality.xlsx", sheet = "mbti_detailed") %>% extract_mbti_dichotomies()

# read anonymous data
d_big5_anon_w_dups = read_csv2("big5_anon_med_w_duplicate_students.csv") %>% filter(cohort_name != "H-2022")
cols_types_explicit = c("cicccicccciiiici") 
d_mbti_anon = read_csv2("mbti_anon_med.csv", 
                        col_types = cols_types_explicit)

# remove 4 duplicates from 2026
d_big5_anon = d_big5_anon_w_dups %>% distinct(person_id, .keep_all = TRUE)
d_mbti_anon = d_mbti_anon %>% distinct(person_id, .keep_all = TRUE)

#------------------------------------------- Compare MBTI first (the easy one) ------------------------------
d_prop_mbti_16 = calc_prop(d_mbti_anon, mbti_personality_text) %>% rename(n_current = n)
d_mbti_16 = d_others_mbti_16 %>% left_join(d_prop_mbti_16, by = "mbti_personality_text") 

# extract dichotomies and combine data
d_prop_e = calc_prop(d_mbti_anon, mbti_extrovert) %>% rename(type = "mbti_extrovert")
d_prop_n = calc_prop(d_mbti_anon, mbti_intuitive) %>% rename(type = "mbti_intuitive")
d_prop_f = calc_prop(d_mbti_anon, mbti_feeling) %>% rename(type = "mbti_feeling")
d_prop_j = calc_prop(d_mbti_anon, mbti_judging) %>% rename(type = "mbti_judging")
d_prop_dichotomies = bind_rows(d_prop_e, d_prop_n, d_prop_f, d_prop_j)

# calc dichotomies for soldiers
d_prop_e_soldier = d_others_mbti_16 %>% 
  group_by(mbti_extrovert) %>% 
  summarise(n_soldier = sum(n_soldier)) %>% 
  rename(type = "mbti_extrovert") 
d_prop_n_soldier = d_others_mbti_16 %>% 
  group_by(mbti_intuitive) %>% 
  summarise(n_soldier = sum(n_soldier)) %>% 
  rename(type = "mbti_intuitive") 
d_prop_f_soldier = d_others_mbti_16 %>% 
  group_by(mbti_feeling) %>% 
  summarise(n_soldier = sum(n_soldier)) %>% 
  rename(type = "mbti_feeling") 
d_prop_j_soldier = d_others_mbti_16 %>% 
  group_by(mbti_judging) %>% 
  summarise(n_soldier = sum(n_soldier)) %>% 
  rename(type = "mbti_judging") 
d_prop_dichotomies_soldiers = bind_rows(d_prop_e_soldier, d_prop_n_soldier, d_prop_f_soldier, d_prop_j_soldier)
n_denom_soldiers = d_others_mbti_16 %>% distinct(denom_soldier) %>% pull()
d_prop_dichotomies_soldiers = d_prop_dichotomies_soldiers %>% mutate(denom_soldier = n_denom_soldiers, prop_soldiers = n_soldier/denom_soldier)

# calc dichotomies for Norwegian norm
d_prop_e_norm = d_others_mbti_16 %>% 
  group_by(mbti_extrovert) %>% 
  summarise(n_norwegian_norm_website = sum(n_norwegian_norm_website)) %>% 
  rename(type = "mbti_extrovert") 
d_prop_n_norm = d_others_mbti_16 %>% 
  group_by(mbti_intuitive) %>% 
  summarise(n_norwegian_norm_website = sum(n_norwegian_norm_website)) %>% 
  rename(type = "mbti_intuitive") 
d_prop_f_norm = d_others_mbti_16 %>% 
  group_by(mbti_feeling) %>% 
  summarise(n_norwegian_norm_website = sum(n_norwegian_norm_website)) %>% 
  rename(type = "mbti_feeling") 
d_prop_j_norm = d_others_mbti_16 %>% 
  group_by(mbti_judging) %>% 
  summarise(n_norwegian_norm_website = sum(n_norwegian_norm_website)) %>% 
  rename(type = "mbti_judging") 
d_prop_dichotomies_norm = bind_rows(d_prop_e_norm, d_prop_n_norm, d_prop_f_norm, d_prop_j_norm)
n_denom_norm = d_others_mbti_16 %>% distinct(denom_norwegian_norm_website) %>% pull()
d_prop_dichotomies_norm = d_prop_dichotomies_norm %>% 
mutate(denom_norwegian_norm_website = n_denom_norm, 
       prop_norm = n_norwegian_norm_website/denom_norwegian_norm_website)

# make a list with datasets
l_dichotomoies = list(d_others_mbti_binom, d_prop_dichotomies_soldiers, d_prop_dichotomies_norm, d_prop_dichotomies) 

# left_join with purrr::reduce
d_others_mbti_binom = reduce(l_dichotomoies, left_join, by = 'type')
n_medstudents = d_others_mbti_binom %>% distinct(denom) %>% pull()

# comparison with other medical students
# dichotomies only
d_mbti_binom = d_others_mbti_binom %>% select(type, n_unlv, n_korean, n_norwegian_norm, n_soldier, n)

# US students
m_e_i = d_mbti_binom %>% filter(type == "E" | type == "I") %>% select(n_unlv, n) %>% as.matrix()
m_n_s = d_mbti_binom %>% filter(type == "N" | type == "S") %>% select(n_unlv, n) %>% as.matrix()
m_f_t = d_mbti_binom %>% filter(type == "F" | type == "T") %>% select(n_unlv, n) %>% as.matrix()
m_j_p = d_mbti_binom %>% filter(type == "J" | type == "P") %>% select(n_unlv, n) %>% as.matrix()

fit_unlv_e = chisq.test(as.matrix(m_e_i))
fit_unlv_n = chisq.test(as.matrix(m_n_s))
fit_unlv_f = chisq.test(as.matrix(m_f_t))
fit_unlv_j = chisq.test(as.matrix(m_j_p))

unlv_ps = c(fit_unlv_e$p.value,
fit_unlv_n$p.value,
fit_unlv_f$p.value,
fit_unlv_j$p.value)

# korean students
m_e_i = d_mbti_binom %>% filter(type == "E" | type == "I") %>% select(n_korean, n) %>% as.matrix()
m_n_s = d_mbti_binom %>% filter(type == "N" | type == "S") %>% select(n_korean, n) %>% as.matrix()
m_f_t = d_mbti_binom %>% filter(type == "F" | type == "T") %>% select(n_korean, n) %>% as.matrix()
m_j_p = d_mbti_binom %>% filter(type == "J" | type == "P") %>% select(n_korean, n) %>% as.matrix()

fit_kor_e = chisq.test(as.matrix(m_e_i))
fit_kor_n = chisq.test(as.matrix(m_n_s))
fit_kor_f = chisq.test(as.matrix(m_f_t))
fit_kor_j = chisq.test(as.matrix(m_j_p))

kor_ps = c(fit_kor_e$p.value,
           fit_kor_n$p.value,
           fit_kor_f$p.value,
           fit_kor_j$p.value)

# Norwegian norm
m_e_i = d_mbti_binom %>% filter(type == "E" | type == "I") %>% select(n_norwegian_norm, n) %>% as.matrix()
m_n_s = d_mbti_binom %>% filter(type == "N" | type == "S") %>% select(n_norwegian_norm, n) %>% as.matrix()
m_f_t = d_mbti_binom %>% filter(type == "F" | type == "T") %>% select(n_norwegian_norm, n) %>% as.matrix()
m_j_p = d_mbti_binom %>% filter(type == "J" | type == "P") %>% select(n_norwegian_norm, n) %>% as.matrix()

fit_nor_e = chisq.test(as.matrix(m_e_i))
fit_nor_n = chisq.test(as.matrix(m_n_s))
fit_nor_f = chisq.test(as.matrix(m_f_t))
fit_nor_j = chisq.test(as.matrix(m_j_p))

nor_ps = c(fit_nor_e$p.value,
           fit_nor_n$p.value,
           fit_nor_f$p.value,
           fit_nor_j$p.value)

# Norwegian soldiers
m_e_i = d_mbti_binom %>% filter(type == "E" | type == "I") %>% select(n_soldier, n) %>% as.matrix()
m_n_s = d_mbti_binom %>% filter(type == "N" | type == "S") %>% select(n_soldier, n) %>% as.matrix()
m_f_t = d_mbti_binom %>% filter(type == "F" | type == "T") %>% select(n_soldier, n) %>% as.matrix()
m_j_p = d_mbti_binom %>% filter(type == "J" | type == "P") %>% select(n_soldier, n) %>% as.matrix()

fit_sol_e = chisq.test(as.matrix(m_e_i))
fit_sol_n = chisq.test(as.matrix(m_n_s))
fit_sol_f = chisq.test(as.matrix(m_f_t))
fit_sol_j = chisq.test(as.matrix(m_j_p))

sol_ps = c(fit_sol_e$p.value,
           fit_sol_n$p.value,
           fit_sol_f$p.value,
           fit_sol_j$p.value)

paste_percent = function(n, perc){
  perc_rounded = round(perc)
  paste0(n, " (",perc_rounded,"%)")
}

d_word_table_mbti_dichotomies = d_others_mbti_binom %>% 
  transmute(`MBTI dichotomy` = type,
            `NOR medical students\n(n=632)`= paste_percent(n, perc),
            `US medical students\n(n=83)` = paste0(paste_percent(n_unlv, perc_unlv), " ", round(unlv_ps, 3)),
            `Korean medical students\n(n=171)` = paste_percent(n_korean, prop_korean*100),
            `NOR general\n(n=159468)` = paste_percent(n_norwegian_norm_website, round(prop_norm*100, 1)),
            `NOR soldiers\n(n=308)` = paste_percent(n_soldier, prop_soldiers*100)
            ) %>% 
  filter(`MBTI dichotomy` %in% c("E", "N", "F", "J"))

sect_properties = prop_section(
  page_size = page_size(orient="landscape")
)
flex_mbti_dichotomies = d_word_table_mbti_dichotomies %>% 
  as_flextable(max_row=(nrow(d_word_table_mbti_dichotomies)))
save_as_docx(flex_mbti_dichotomies, path = paste0(folder_wd, "mbti_comparison_dichotomies.docx"), 
             pr_section = sect_properties)

# comparison with US medical students
m_mbti_us = d_mbti_16 %>% select(n_unlv, n_current) %>% t
dimnames(m_mbti_us) <- list(study = c("US", "current"),
                                    mbti = d_mbti_16$mbti_personality_text)
test_16_us = chisq.test(m_mbti_us)
test_16_us$p.value

# comparison with Korean medical students
m_mbti_korean = d_mbti_16 %>% select(n_korean, n_current) %>% t
dimnames(m_mbti_korean) <- list(study = c("Korean", "current"),
                                    mbti = d_mbti_16$mbti_personality_text)
test_16_korean = chisq.test(m_mbti_korean)
test_16_korean$p.value

# comparison with soldiers
# comparison with norwegian normal
m_mbti_soldiers = d_mbti_16 %>% select(n_soldier, n_current) %>% t
dimnames(m_mbti_soldiers) <- list(study = c("soldier", "current"),
                    mbti = d_mbti_16$mbti_personality_text)

test_16_soldiers = chisq.test(m_mbti_soldiers)
test_16_soldiers$p.value

# comparison with norwegian normal
m_mbti_norwegians = d_mbti_16 %>% select(n_norwegian_norm_website, n_current) %>% t
dimnames(m_mbti_norwegians) <- list(study = c("Norwegians", "current"),
                                  mbti = d_mbti_16$mbti_personality_text)
test_16_norwegians = chisq.test(m_mbti_norwegians)
test_16_norwegians$p.value

d_word_table_mbti_types = 
  d_mbti_16 %>% arrange(desc(prop), desc(n_current)) %>% 
  transmute(`MBTI type` = mbti_personality_text,
            `Norwegian medical students\n(n=632)`= paste_percent(n_current, perc),
            `US medical students\n(n=83)`= paste_percent(n_unlv, round(prop_unlv*100, 1)),
            `Korean medical students\n(n=171)`= paste_percent(n_korean, round(prop_korean*100, 1)),
            `Norwegian general\n(n=159468)` = paste_percent(n_norwegian_norm_website, round(prop_norwegian_norm_website*100, 1)),
            `Norwegian soldiers\n(n=308)` = paste_percent(n_soldier, perc_soldier*100)
  )


flex_mbti_types = d_word_table_mbti_types %>% 
  as_flextable(max_row=(nrow(d_word_table_mbti_types)))
save_as_docx(flex_mbti_types, path = paste0(folder_wd, "mbti_comparison.docx"), pr_section = sect_properties)

#---------------------------------------------------- Gender comparisons --------------------------------------

d_mbti_gender = read_csv2("mbti_per_gender.csv")

d_korean_gender = d_mbti_16 %>% arrange(desc(prop), desc(n_current)) %>% 
  select(MBTI = mbti_personality_text, n_korean_women, prop_korean_women, n_korean_men, prop_korean_men)
d_mbti_gender_j = d_mbti_gender %>% left_join(d_korean_gender, by = "MBTI")

d_mbti_gender_j %>% summarise(sum(n_Female), sum(n_Male))

d_mbti_gender_compare_table = d_mbti_gender_j %>% 
  mutate(prop_korean_women = paste0(n_korean_women, " (", round(prop_korean_women*100), "%)"),
         prop_korean_men = paste0(n_korean_men, " (", round(prop_korean_men*100), "%)")) %>% 
  select(MBTI, n_Female, prop_text_Female, 
                          prop_korean_women, 
                           n_Male, prop_text_Male, 
                           prop_korean_men)

flex_mbti_comparison_gender_table = d_mbti_gender_compare_table %>% 
  as_flextable(max_row=(nrow(d_mbti_gender_compare_table)))
save_as_docx(flex_mbti_comparison_gender_table, 
             path = paste0(folder_wd, "mbti_comparison_gender.docx"), 
             pr_section = sect_properties)

#---------------------------------------------- Big 5 comparisons -----------------------------------

labels = d_others_big5$label
n_dimensions = length(labels)
d_big5_selected = d_big5_anon %>% 
                select(person_id, cohort_name, 
                       starts_with("big5_agreeableness"), 
                       starts_with("big5_conscientiousness"), 
                       starts_with("big5_extrovert"), 
                       starts_with("big5_neuroticism"), 
                       starts_with("big5_openness"))

d_big5_mean = d_big5_selected %>% select(all_of(ends_with("mean")))
d_big5_tot = d_big5_selected %>% select(all_of(ends_with("tot")))

# compare means with Peters 2024
m_test_results_peters = matrix(nrow = n_dimensions, ncol = 3, 
                               dimnames = list(c(1:5),
                               c("ci_low", "ci_high", "p"))
                               )
for(i in 1:n_dimensions){
  mean_to_compare = d_others_big5 %>% filter(label == labels[i]) %>% select(peters_2024_mean) %>% pull()
  one_sample_test = t.test(d_big5_mean[i], mu = mean_to_compare) 
  m_test_results_peters[i, 1] = one_sample_test$conf.int[1]
  m_test_results_peters[i, 2] = one_sample_test$conf.int[2]
  m_test_results_peters[i, 3] = one_sample_test$p.value
}

d_mean_of_means = d_big5_mean %>% 
  summarise_all(mean) %>% 
  pivot_longer(1:5, 
               names_to = "label", 
               values_to = "current_mean") %>% 
  mutate(label = labels)

d_sd_of_means = d_big5_mean %>% 
  summarise_all(sd) %>% 
  pivot_longer(1:5, 
               names_to = "label", 
               values_to = "current_sd") %>% 
  mutate(label = labels)

d_means = list(d_others_big5 %>% 
  select(label, peters_2024_mean, peters_2024_sd), 
  d_mean_of_means, d_sd_of_means) %>% 
  reduce(left_join) 

# report as 1-5 scales for the main table, but keep the original scale for supplementary
means_peter_1_5 = (d_means$peters_2024_mean-1)*(4/6) + 1
sd_scaling_ger = 4/6
sd_peter_1_5 = d_means$peters_2024_sd*sd_scaling_ger

d_one_sample_results = m_test_results_peters %>% as_tibble()
d_means = d_means %>% mutate(diff = round(current_mean - peters_2024_mean, 2), 
                   ci_low = round(d_one_sample_results$ci_low, 2), 
                   ci_high = round(d_one_sample_results$ci_high, 2), 
                   p = round(d_one_sample_results$p, 3),
                   current_mean = round(current_mean, 2),
                   current_sd = round(current_sd, 1),
                   mean_1_5_peter = means_peter_1_5,
                   sd_1_5_peter = sd_peter_1_5)
d_means = d_means %>% arrange(desc(current_mean))

# compare tots with Norwegian workers
m_test_results_workers = 
  matrix(nrow = n_dimensions, 
         ncol = 3, dimnames = list(c(1:5), c("ci_low", "ci_high", "p")))
for(i in 1:n_dimensions){
  mean_to_compare = d_others_big5 %>% filter(label == labels[i]) %>% select(norwegian_worker_mean) %>% pull()
  one_sample_test = t.test(d_big5_tot[i], mu = mean_to_compare) 
  m_test_results_workers[i, 1] = one_sample_test$conf.int[1]
  m_test_results_workers[i, 2] = one_sample_test$conf.int[2]
  m_test_results_workers[i, 3] = one_sample_test$p.value
}

d_mean_of_tots = d_big5_tot %>% 
  summarise_all(mean) %>% 
  pivot_longer(1:5, 
               names_to = "label", 
               values_to = "current_mean") %>% 
  mutate(label = labels)

d_sd_of_tots = d_big5_tot %>% 
  summarise_all(sd) %>% 
  pivot_longer(1:5, 
               names_to = "label", 
               values_to = "current_sd") %>% 
  mutate(label = labels)

d_tots = list(d_others_big5 %>% 
              select(label, norwegian_worker_mean, norwegian_worker_sd), 
              d_mean_of_tots, d_sd_of_tots) %>% 
  reduce(left_join) 

d_one_sample_results_tots = m_test_results_workers %>% as_tibble()
d_tots = d_tots %>% mutate(diff = round(current_mean - norwegian_worker_mean, 2),
                             ci_low = round(d_one_sample_results_tots$ci_low, 2), 
                             ci_high = round(d_one_sample_results_tots$ci_high, 2), 
                             p = round(d_one_sample_results_tots$p, 3),
                           current_mean = round(current_mean, 2),
                           current_sd = round(current_sd, 2))

# Plaisant requires more work for the interpolation
big5_items = names(d_big5_anon %>% select(starts_with("big5"), -ends_with("tot"), -ends_with("mean")))
d_big5_interpolated = d_big5_anon %>% mutate_at(vars(big5_items), interpolate)

extrovert_vars = c("big5.chatty", "big5.quiet", "big5.outgoing", "big5.shy")
neuroticism_vars = c("big5.depressed", "big5.relaxed_rev", "big5.worried_a_lot", "big5.nervous")
openness_vars = c("big5.original", "big5.wild_imagination", "big5.speculate", "big5.not_artistic")
agreeableness_vars = c("big5.cold", "big5.helpful", "big5.rude", "big5.considerate")
conscientiousness_vars = c("big5.thorough", "big5.unorganized", "big5.planner", "big5.uncareful")

d_big5_interpolated = d_big5_interpolated %>% 
  mutate(big5_extrovert_mean = rowMeans(across(all_of(extrovert_vars))),
         big5_neuroticism_mean = rowMeans(across(all_of(neuroticism_vars))),
         big5_openness_mean = rowMeans(across(all_of(openness_vars))),
         big5_agreeableness_mean = rowMeans(across(all_of(agreeableness_vars))),
         big5_conscientiousness_mean = rowMeans(across(all_of(conscientiousness_vars)))) 

d_big5_interpolated_means = d_big5_interpolated  %>% 
  select(all_of(names(d_big5_mean)))

d_mean_of_interpolated = 
  d_big5_interpolated_means %>%  
  summarise_all(mean) %>% 
  pivot_longer(1:5, 
               names_to = "label", 
               values_to = "current_mean") %>% 
  mutate(label = labels)

d_sd_of_interpolated = d_big5_interpolated_means %>%  
  summarise_all(sd) %>% 
  pivot_longer(1:5, 
               names_to = "label", 
               values_to = "current_sd") %>% 
  mutate(label = labels)

# add 1 - 5 to the German comparison as well
d_means_with_1_5 = d_means %>% 
  left_join(d_mean_of_interpolated %>% 
            rename(current_mean_1_5 = current_mean), by = "label") %>% 
  mutate(diff_1_5 = round(current_mean_1_5 - mean_1_5_peter, 2),
         mean_1_5_peter = round(mean_1_5_peter, 2),
         sd_1_5_peter = round(sd_1_5_peter, 2),
         current_mean_1_5 = round(current_mean_1_5, 2))

# compare interpolated means with Plaisant
m_test_results_plaisant = 
  matrix(nrow = n_dimensions, 
         ncol = 3, dimnames = list(c(1:5), c("ci_low", "ci_high", "p")))
for(i in 1:n_dimensions){
  mean_to_compare = d_others_big5 %>% filter(label == labels[i]) %>% select(plaisant_et_al_2011_first_year_mean) %>% pull()
  one_sample_test = t.test(d_big5_interpolated_means[i], mu = mean_to_compare) 
  m_test_results_plaisant[i, 1] = one_sample_test$conf.int[1]
  m_test_results_plaisant[i, 2] = one_sample_test$conf.int[2]
  m_test_results_plaisant[i, 3] = one_sample_test$p.value
}

m_test_results_plaisant_thirdyear = 
  matrix(nrow = n_dimensions, 
         ncol = 3, dimnames = list(c(1:5), c("ci_low", "ci_high", "p")))
for(i in 1:n_dimensions){
  mean_to_compare = d_others_big5 %>% filter(label == labels[i]) %>% select(plaisant_et_al_2011_third_year_mean) %>% pull()
  one_sample_test = t.test(d_big5_interpolated_means[i], mu = mean_to_compare) 
  m_test_results_plaisant_thirdyear[i, 1] = one_sample_test$conf.int[1]
  m_test_results_plaisant_thirdyear[i, 2] = one_sample_test$conf.int[2]
  m_test_results_plaisant_thirdyear[i, 3] = one_sample_test$p.value
}

d_interpolated = list(d_others_big5 %>% 
                select(label, 
                       plaisant_et_al_2011_first_year_mean, 
                       plaisant_et_al_2011_first_year_sd,
                       plaisant_et_al_2011_third_year_mean,
                       plaisant_et_al_2011_third_year_sd), 
              d_mean_of_interpolated, d_sd_of_interpolated) %>% 
  reduce(left_join) 

d_one_sample_results_interpolated = m_test_results_plaisant %>% as_tibble()
d_one_sample_results_interpolated_thirdyear = m_test_results_plaisant_thirdyear %>% as_tibble()
d_interpolated = d_interpolated %>% 
                 mutate(current_mean = round(current_mean, 2),
                        current_sd = round(current_sd, 2),
                        ci_low = round(d_one_sample_results_interpolated$ci_low, 2), 
                        ci_high = round(d_one_sample_results_interpolated$ci_high, 2),
                        diff = round(current_mean - plaisant_et_al_2011_first_year_mean, 2), 
                        p = round(d_one_sample_results_interpolated$p, 3),
                        diff_thirdyear = round(current_mean - plaisant_et_al_2011_third_year_mean, 2), 
                        p_thirdyear = round(d_one_sample_results_interpolated_thirdyear$p, 3)
                        )
d_interpolated = d_interpolated %>% arrange(desc(current_mean))

# some tests to see how these results compare with
# the observed values
(d_interpolated$plaisant_et_al_2011_first_year_mean-1)*(27/4) + 1
(d_interpolated$current_mean-1)*(27/4) + 1

# what happens if you rescale meanscores, instead of each item before calculating means?
interpolate(d_means_with_1_5$current_mean)
# these results are very similar to the original interpolated results!

sd_scaling_fr = 27/4
d_interpolated$plaisant_et_al_2011_first_year_sd*sd_scaling_fr

#-------------------- save as word docs
sect_properties = prop_section(
  page_size = page_size(orient = "landscape")
)

flex_peters = d_means_with_1_5 %>% 
  as_flextable(max_row=(nrow(d_means))) %>% 
  colformat_num(digits = 3)
save_as_docx(flex_peters, path = paste0(folder_wd, "big5_comparison_peters2024.docx"), 
             pr_section = sect_properties)

flex_norwegian_workers = d_tots %>% 
  as_flextable(max_row=(nrow(d_tots))) %>% 
  colformat_num(digits = 3)
save_as_docx(flex_norwegian_workers, 
             path = paste0(folder_wd, "big5_comparison_nor_workers.docx"), 
             pr_section = sect_properties)

flex_plaisant = d_interpolated %>% 
  as_flextable(max_row=(nrow(d_interpolated))) %>% 
  colformat_num(digits = 3)
save_as_docx(flex_plaisant, 
             path = paste0(folder_wd, "big5_comparison_plaisant2011.docx"), 
             pr_section = sect_properties)

#----------------- Comparison figure

means_tots = (d_tots$norwegian_worker_mean-1)*(4/27) + 1
d_norm = d_tots %>% mutate(tots_1_5 = means_tots) %>% select(label, tots_1_5)

d_figdata = d_interpolated %>% select(label, 
                          plaisant_et_al_2011_first_year_mean, 
                          plaisant_et_al_2011_third_year_mean, 
                          current_mean) %>% 
  left_join(d_means_with_1_5 %>% select(label, mean_1_5_peter), by = "label") %>% 
  left_join(d_norm, by = "label")

d_figdata_long = d_figdata %>% pivot_longer(2:6, values_to = "mean", names_to = "study")

d_figdata_long = d_figdata_long %>% mutate(study = case_when(
                          study == "plaisant_et_al_2011_first_year_mean" ~ "French 1st year students",
                          study == "plaisant_et_al_2011_third_year_mean" ~ "French 3rd year students",
                          study == "mean_1_5_peter" ~ "German graduating students",
                          study == "tots_1_5" ~ "Norwegian normal",
                          study == "current_mean" ~ "Norwegian students")
                          )

study_arrangement = tribble(~"study", ~"index",
                            "Norwegian students", 1,
                            "Norwegian normal", 2,
                            "German graduating students", 3,
                            "French 1st year students", 4,
                            "French 3rd year students", 5
                            )
d_figdata_long = d_figdata_long %>% left_join(study_arrangement, by = "study")
d_figdata_long = d_figdata_long %>% arrange(desc(index)) %>% mutate(study_fct = fct_inorder(study))

d_figdata_long_filter = d_figdata_long %>% filter(label != "Extraversion", label != "Openness")

emf("bfi_compare_3.emf", width = 12, height = 4, units = "in")
plot_bfi_study_compare
dev.off()

png("bfi_compare_3.png", width = 12, height = 4, units = "in", res = 900)
plot_bfi_study_compare
dev.off()

# make figure showing distribution
d_index = tribble(~index_2, ~label, ~label_long,
                        3, "Extraversion", "Introversion                                                Extraversion",
                        4, "Openness", "Closedness to experience                            Openness",
                        1, "Agreeableness", "Antagonism                                           Agreeableness",
                        2, "Conscientiousness", "Lack of direction                             Conscientiousness",
                        5, "Neuroticism", "Emotional stability                                      Neuroticism")

d_figdata_nonorm = d_figdata_long %>% filter(study != "Norwegian normal")

d_figdata_nonorm = d_figdata_nonorm %>% 
  left_join(d_index, by = "label") %>% 
  mutate(label_long_fct = fct_inorder(label_long))

black_line <- element_line(colour = "black", linewidth = 0.5)
text_size = 14
plot_bfi_study_compare = 
  ggplot(d_figdata_nonorm, aes(x = mean, y = study_fct)) +
  facet_wrap(~label_long_fct, ncol = 1, strip.position="bottom") +
  geom_point(fill = uio_soft_colors[1], size = 3) +
  xlab("Mean") +
  ylab(NULL) +
  theme_bw() +
  theme(text = element_text(size=text_size)) +
  scale_x_continuous(breaks = scales::breaks_width(0.5, 0)) +
  scale_y_discrete(position = "left") +
  theme(
    panel.spacing = unit(0.5, "lines"),
    axis.text.y = element_text(hjust = 0),
    strip.background = element_blank(),
    axis.line.x = black_line
    )
  
emf("bfi_compare_students.emf", width = 6, height = 8, units = "in")
plot_bfi_study_compare
dev.off()

png("bfi_compare_students.png", width = 6, height = 8, units = "in", res = 900)
plot_bfi_study_compare
dev.off()
