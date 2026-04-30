library(tidyverse)
library(Hmisc) # confidence intervals around the mean
library(flextable)
library(officer)
library(devEMF)
library(directlabels)
library(readxl)
library(ggmulti)
library(boot) # nonparametric confidence intervals

folder_wd = "N:/durable/projects/personality/"
setwd(folder_wd)
source(paste0(folder_wd, "personality-project-functions.R"))

# read anonymous data
d_big5_anon_w_dups = read_csv2("big5_anon_med_w_duplicate_students.csv") %>% filter(cohort_name != "H-2022")
d_big5_anon = d_big5_anon_w_dups %>% distinct(person_id, .keep_all = TRUE)
d_others_big5 = read_excel("other_articles_personality.xlsx", sheet = "big-five")

# select variables for this analysis
vars_big5_subscales = names(d_big5_anon %>% select(ends_with("tot")))
d_big5_anon_select = d_big5_anon %>% 
  select(person_id, cohort_name, all_of(vars_big5_subscales))
d_big5_anon_select_w_dups = d_big5_anon_w_dups  %>% 
  select(person_id, cohort_name, all_of(vars_big5_subscales))

#--------- total Big-5
# calculate descriptives
calc_descs = function(x){
  mean_x = mean(x)
  sd_x = sd(x)
  median_x = median(x)
  min_x = min(x)
  max_x = max(x)
  
  bind_cols(mean_x = mean_x, 
            sd_x = sd_x, 
            median_x = median_x, 
            min_x = min_x, 
            max_x = max_x)
}

calc1 = calc_descs(d_big5_anon_select$big5_extrovert_tot) %>% mutate(subscale = "Extraversion")
calc2 = calc_descs(d_big5_anon_select$big5_neuroticism_tot) %>% mutate(subscale = "Neuroticism")
calc3 = calc_descs(d_big5_anon_select$big5_openness_tot) %>% mutate(subscale = "Openness")
calc4 = calc_descs(d_big5_anon_select$big5_agreeableness_tot) %>% mutate(subscale = "Agreeableness")
calc5 = calc_descs(d_big5_anon_select$big5_conscientiousness_tot) %>% mutate(subscale = "Conscientiousness")
d_big5_descs_tot = bind_rows(calc1, calc2, calc3, calc4, calc5) %>% arrange(desc(mean_x))
desc_vars = names(d_big5_descs_tot)[-6]
d_big5_descs_tot = d_big5_descs_tot %>% select(subscale, all_of(desc_vars)) %>% mutate(mean_x = round(mean_x, 2))

flex_big5_descs_tot = d_big5_descs_tot %>% as_flextable(max_row=(nrow(d_big5_descs_tot)))
save_as_docx(flex_big5_descs_tot, path = paste0(folder_wd, "big5_tot.docx"))

# make figure showing distribution
d_label_index = tribble(~index, ~label, ~label_long,
                3, "Extraversion", "Introversion                                                                                                         Extraversion",
                4, "Openness", "Closedness to experience                                                                                      Openness",
                 1, "Agreeableness", "Antagonism                                                                                                     Agreeableness",
                 2, "Conscientiousness", "Lack of direction                                                                                        Conscientiousness",
                5, "Neuroticism", "Emotional stability                                                                                                Neuroticism")

d_labels = enframe(vars_big5_subscales,
                   name = NULL, value = "subscale") %>%
           mutate(labels = d_label_index$label_long)

d_bfi_long_med = pivot_longer(
  d_big5_anon_select,
  all_of(vars_big5_subscales),
  names_to = "subscale",
  values_to = "bfi_score"
)

d_bfi_long_med = d_bfi_long_med %>% 
  left_join(d_labels, by = "subscale") %>%
  group_by(labels) %>% 
  mutate(avg_score = mean(bfi_score)) %>% 
  ungroup() %>% 
  arrange(desc(avg_score)) %>% 
  mutate(labels = fct_inorder(labels))

#--------------- main figure
# include comparison with Norwegian workers

d_others_big5_labelled = d_others_big5 %>% 
  left_join(d_label_index, by = "label") 

d_others_big5_labelled = d_others_big5_labelled %>% 
  arrange(desc(index)) %>% 
  mutate(labels = fct_inorder(label_long))

d_tot_mean = d_others_big5_labelled %>% 
  select(labels, norwegian_worker_mean)

d_bfi_long_med = d_bfi_long_med %>% 
  left_join(d_tot_mean, by = "labels") 


text_size = 14
plot_bfi_distribution_med = 
ggplot(d_bfi_long_med, 
       aes(x = bfi_score)) +
  facet_wrap(~labels, ncol = 1, strip.position="bottom") +
  geom_histogram(bins = 20, binwidth = 1) +
  geom_vline(data = d_tot_mean, 
             aes(xintercept = norwegian_worker_mean),
             linewidth = 1)+ 
  lmisc::theme_base(text_size) +
  scale_x_continuous(breaks = scales::breaks_width(5, 0), expand = lmisc::expand_bar) +
  scale_y_discrete(expand = lmisc::expand_bar) +
  xlab("Big Five Inventory score (1–28)") +
  ylab(NULL) +
  theme(
    panel.spacing = unit(0.5, "lines"), 
    strip.text.x = element_text(margin = margin(0.05,0,0.1,0, "cm"),
    strip.background = element_blank()
    )
  ) 

emf("bfi_distributions.emf", width = fig_width, height = fig_height+3, units = "in")
plot_bfi_distribution_med
dev.off()

png("bfi_distributions.png", width = fig_width, height = fig_height+3, units = "in", res = 900)
plot_bfi_distribution_med
dev.off()

#-------- per cohort
cohort_names = d_big5_anon_w_dups %>% distinct(cohort_name) %>% pull(cohort_name)

d_nested = d_big5_anon_select_w_dups %>% group_by(cohort_name) %>% nest()
l_extrovert = d_nested$data %>% map(~smean.cl.normal(.$big5_extrovert_tot))
l_neuroticism = d_nested$data %>% map(~smean.cl.normal(.$big5_neuroticism_tot))
l_openness  = d_nested$data %>% map(~smean.cl.normal(.$big5_openness_tot))
l_agreeableness  = d_nested$data %>% map(~smean.cl.normal(.$big5_agreeableness_tot))
l_conscientousness  = d_nested$data %>% map(~smean.cl.normal(.$big5_conscientiousness_tot))

pivot_cohorts = function(l) {
bind_rows(l) %>% mutate(cohorts = cohort_names) %>%
    pivot_wider(values_from = 1:3, names_from = cohorts) %>%
    select(
      ends_with("V-2024"),
      ends_with("H-2024"),
      ends_with("V-2025"),
      ends_with("H-2025"),
      ends_with("V-2026")
    )
}

d1 = pivot_cohorts(l_extrovert) %>% mutate(subscale = "Extraversion")
d2 = pivot_cohorts(l_neuroticism) %>% mutate(subscale = "Neuroticism")
d3 = pivot_cohorts(l_openness) %>% mutate(subscale = "Openness")
d4 = pivot_cohorts(l_agreeableness) %>% mutate(subscale = "Agreeableness")
d5 = pivot_cohorts(l_conscientousness)  %>% mutate(subscale = "Conscientiousness")
d_cohorts_big5 = bind_rows(d1, d2, d3, d4, d5)

tot1 = smean.cl.normal(d_big5_anon_select$big5_extrovert_tot)
tot2 = smean.cl.normal(d_big5_anon_select$big5_neuroticism_tot) 
tot3 = smean.cl.normal(d_big5_anon_select$big5_openness_tot)
tot4 = smean.cl.normal(d_big5_anon_select$big5_agreeableness_tot) 
tot5 = smean.cl.normal(d_big5_anon_select$big5_conscientiousness_tot)
d_tot = bind_rows(tot1, tot2, tot3, tot4, tot5)
d_total_big5 = d_tot %>% mutate(vars_big5_subscales)

d_big5_table_cohorts = 
  d_cohorts_big5 %>% mutate(mean_tot = d_tot$Mean, lower_tot = d_tot$Lower, upper_tot = d_tot$Upper)

d_big5_table_cohorts = d_big5_table_cohorts %>% 
  select(subscale, mean_tot, lower_tot, upper_tot, all_of(names(d_cohorts_big5)[-13]))

d_big5_table_cohorts = d_big5_table_cohorts %>% arrange(desc(mean_tot))

sect_properties = prop_section(
  page_size = page_size(orient = "landscape")
  )
flex_big5_cohorts = d_big5_table_cohorts %>% as_flextable(max_row=(nrow(d_big5_table_cohorts)))
flex_big5_cohorts = set_table_properties(flex_big5_cohorts, layout = "autofit")
save_as_docx(flex_big5_cohorts, path =  paste0(folder_wd, "big5_cohorts.docx"), pr_section = sect_properties)

#----------------------- adjust for age and gender (cannot be done on anonymous data)

# For this analyses, unavailable,
# pseudoanonymous data is used. 
# code is shown for transparency.

# read pseudoanonymous data
d_personality = read_csv2("personality_nonanon.csv")
d_personality_med = d_personality %>% distinct(person_id, .keep_all=TRUE)
d_personality_med = d_personality_med %>% 
  mutate(season = ifelse(str_detect(cohort_name, "V"), "Spring", "Fall"))

d_nested_gender = d_personality_med %>% filter(!is.na(gender)) %>% group_by(gender) %>% nest()
l_agreeableness_ge  = d_nested_gender$data %>% map(~smean.cl.normal(.$big5_agreeableness_tot))
l_conscientousness_ge  = d_nested_gender$data %>% map(~smean.cl.normal(.$big5_conscientiousness_tot))
l_extrovert_ge = d_nested_gender$data %>% map(~smean.cl.normal(.$big5_extrovert_tot))
l_neuroticism_ge = d_nested_gender$data %>% map(~smean.cl.normal(.$big5_neuroticism_tot))
l_openness_ge  = d_nested_gender$data %>% map(~smean.cl.normal(.$big5_openness_tot))

genders = c("m", "fe")
pivot_genders = function(l) {
  bind_rows(l) %>% mutate(gender = genders) %>%
    pivot_wider(values_from = 1:3, names_from = gender)
}
d1 = pivot_genders(l_agreeableness_ge) %>% mutate(subscale = "Agreeableness")
d2 = pivot_genders(l_conscientousness_ge)  %>% mutate(subscale = "Conscientiousness")
d3 = pivot_genders(l_extrovert_ge) %>% mutate(subscale = "Extrovertedness")
d4 = pivot_genders(l_neuroticism_ge) %>% mutate(subscale = "Neuroticism")
d5 = pivot_genders(l_openness_ge) %>% mutate(subscale = "Openness")
d_genders_big5 = bind_rows(d1, d2, d3, d4, d5) %>% select(subscale, ends_with("m"), ends_with("fe"))

flex_big5_gender = d_genders_big5 %>% as_flextable(max_row=(nrow(d_genders_big5)))
flex_big5_gender = set_table_properties(flex_big5_gender, layout = "autofit")
save_as_docx(flex_big5_gender, 
             path =  paste0(folder_wd, "big5_gender.docx"))


#-------------- same for admissions quota

d_personality_med = d_personality_med %>% mutate(first_diploma = ifelse(age <=21, "First diploma", "Ordinary admission"))

d_personality_med %>% count(first_diploma)

d_nested_quota = d_personality_med %>% filter(!is.na(first_diploma)) %>% group_by(first_diploma) %>% nest()
l_agreeableness_ge  = d_nested_quota$data %>% map(~smean.cl.normal(.$big5_agreeableness_tot))
l_conscientousness_ge  = d_nested_quota$data %>% map(~smean.cl.normal(.$big5_conscientiousness_tot))
l_extrovert_ge = d_nested_quota$data %>% map(~smean.cl.normal(.$big5_extrovert_tot))
l_neuroticism_ge = d_nested_quota$data %>% map(~smean.cl.normal(.$big5_neuroticism_tot))
l_openness_ge  = d_nested_quota$data %>% map(~smean.cl.normal(.$big5_openness_tot))

quotas = c("first", "ordn")
pivot_quotas = function(l) {
  bind_rows(l) %>% mutate(quota = quotas) %>%
    pivot_wider(values_from = 1:3, names_from = quota)
}
d1 = pivot_quotas(l_agreeableness_ge) %>% mutate(subscale = "Agreeableness")
d2 = pivot_quotas(l_conscientousness_ge)  %>% mutate(subscale = "Conscientiousness")
d3 = pivot_quotas(l_extrovert_ge) %>% mutate(subscale = "Extrovertedness")
d4 = pivot_quotas(l_neuroticism_ge) %>% mutate(subscale = "Neuroticism")
d5 = pivot_quotas(l_openness_ge) %>% mutate(subscale = "Openness")
d_quotas_big5 = bind_rows(d1, d2, d3, d4, d5) %>% select(subscale, ends_with("first"), ends_with("ordn"))


flex_big5_quota = d_quotas_big5 %>% as_flextable(max_row=(nrow(d_quotas_big5)))
flex_big5_quota = set_table_properties(flex_big5_quota, layout = "autofit")
save_as_docx(flex_big5_quota, 
             path =  paste0(folder_wd, "big5_quota.docx"))
