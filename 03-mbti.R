library(tidyverse)
library(readxl) # reading the codebook
library(binom) # Wilson confidence intervals
library(flextable) # save table in word doc
library(officer) # landscape page orientation
library(devEMF) # save figure as EMF
library(directlabels) # add labels directly to linegraph

# read the codebook (metadata).
d_codebook = read_excel("variable_codebook_personality.xlsx")

folder_wd = "N:/durable/projects/personality/"
setwd(folder_wd)
source(paste0(folder_wd, "personality-project-functions.R"))

#------------------- some figure theme objects
# read anonymous data
# variable types had to be specified, otherwise F/T is interpreted as logical, 
# instead of Feeling and Thinking...
cols_types_explicit = c("cicccicccciiiici") 
d_mbti_anon = read_csv2("mbti_anon_med.csv", 
                        col_types = cols_types_explicit)

calc_prop_cis = function(d, var){
  var = enquo(var)  
d_confints = binom.confint(d$n, d$denom, conf.level = 0.95, methods = "wilson") %>% 
  tibble() %>% 
  mutate_at(c("mean", "lower", "upper"), ~round(.*100)) %>% 
  mutate(mbti_type = d %>% pull(!!var)) %>% 
  rename(prop = mean) %>% 
  arrange(desc(prop)) %>% 
  select(mbti_type, x, prop, lower, upper, n)
d_confints
}

# calculation proportions with Wilson CIs for a given variable
calc_prop_no_group = function(d, var){
  var = enquo(var)
  
  d_props = calc_prop(d, !!var)
  calc_prop_cis(d_props, !!var)
}

# look only at proportion A vs. T, a feature of 2026, only
d_mbti_2026 = d_mbti_anon %>% filter(cohort_name == "V-2026")
d_perc_assertive = d_mbti_2026 %>% count(mbti_assertive) %>% mutate(denom = sum(n))
calc_prop_cis(d_perc_assertive, mbti_assertive)

# remove 4 students that were part of H-2025 and V-2026
# for total analyses
# here, we only consider them the first time they attended
d_mbti_anon_nodups = d_mbti_anon %>% distinct(person_id, .keep_all = TRUE)

# distribution of the 16
d_props_mbti_tot = calc_prop_no_group(d_mbti_anon_nodups, mbti_personality_text) %>% arrange(desc(prop), desc(x))
d_props_extrovert_tot = calc_prop_no_group(d_mbti_anon_nodups, mbti_extrovert)
d_props_inutitive_tot = calc_prop_no_group(d_mbti_anon_nodups, mbti_intuitive)
d_props_feeling_tot = calc_prop_no_group(d_mbti_anon_nodups, mbti_feeling)
d_props_judging_tot = calc_prop_no_group(d_mbti_anon_nodups, mbti_judging)

d_mbti_subdimenions_tot = bind_rows(d_props_extrovert_tot, d_props_inutitive_tot, 
                                    d_props_feeling_tot, d_props_judging_tot)
d_data_for_mbti_table = bind_rows(d_props_mbti_tot, d_mbti_subdimenions_tot)

# save table in word
flex_mbti_tot = d_data_for_mbti_table %>% as_flextable(max_row=(nrow(d_data_for_mbti_table)))
flex_mbti_tot = set_table_properties(flex_mbti_tot, layout = "autofit")
save_as_docx(flex_mbti_tot, path =  paste0(folder_wd, "mbti_tot.docx"))

# distribtuion of 4-letter MBTI codes
# as bargraph
d_props_figdata = d_props_mbti_tot %>% arrange(prop, x) %>% mutate(prop = prop/100, mbti_type = fct_inorder(mbti_type))
mbti_plot_med = ggplot(d_props_figdata, aes(y = mbti_type, x = prop)) +
  ggstance::geom_barh(stat = "identity", fill = uio_soft_colors[1])  +
  lmisc::theme_uio() + 
  lmisc::theme_barh(text_size = text_size) +
  scale_x_continuous(labels = lmisc::axis_percent, expand = lmisc::expand_bar) +
  theme(axis.title = element_blank(),
        panel.grid.major = element_blank())

emf("mbti_tot_bars.emf", width = fig_width, height = fig_height_mbti, units = "in")
mbti_plot_med
dev.off()

png("mbti_tot_bars.png", width = fig_width, height = fig_height_mbti, units = "in", res = 900)
mbti_plot_med
dev.off()

#----------------------------------------------- figure with mbti dimension continuous

# make figure showing distribution
d_mbti_2026_percfix = d_mbti_2026 %>% mutate(mbti_perc_e_i = ifelse(mbti_extrovert == "I", 0-mbti_perc_e_i+50, mbti_perc_e_i-50),
                                     mbti_perc_n_s = ifelse(mbti_intuitive == "S", 0-mbti_perc_n_s+50, mbti_perc_n_s-50),
                                     mbti_perc_t_f = ifelse(mbti_feeling == "T", 0-mbti_perc_t_f+50, mbti_perc_t_f-50),
                                     mbti_perc_j_p = ifelse(mbti_judging == "P", 0-mbti_perc_j_p+50, mbti_perc_j_p-50),
                                     mbti_perc_a_t = ifelse(mbti_assertive == "T", 0-mbti_perc_a_t+50, mbti_perc_a_t-50))

make_histfig = function(var, label){
  var = enquo(var)
  text_size = 14
  ggplot(d_mbti_2026_percfix, aes(x = !!var)) +
    ggmulti::geom_histogram_(bins = 20, binwidth = 2) +
    lmisc::theme_base(text_size) +
    geom_vline(xintercept = 0) + 
    scale_x_continuous(breaks = scales::breaks_width(10, 0), expand = c(0.05, 0, 0.05, 0), limits = c(-50, 50)) +
    scale_y_discrete(expand = lmisc::expand_bar) +
    xlab(label) +
    ylab(NULL)
  
}
plot_e_i = make_histfig(mbti_perc_e_i, label = "Degree of Introversion (-50–0) to Extraversion (0–50)")
plot_n_s = make_histfig(mbti_perc_n_s, label = "Degree of Observant (-50–0) to Intuitive (0–50)")
plot_t_f = make_histfig(mbti_perc_t_f, label = "Degree of Thinking (-50–0) to Feeling (0–50)")
plot_j_p = make_histfig(mbti_perc_j_p, label = "Degree of Prospecting (-50–0) to Judging (0–50)")
plot_a_t = make_histfig(mbti_perc_a_t, label = "Degree of Turbulent (-50–0) to Assertive (0–50)")

fig_width = 6
fig_height = 2
emf("mbti_e_i.emf", width = fig_width, height = fig_height, units = "in")
plot_e_i
dev.off()

emf("mbti_n_s.emf", width = fig_width, height = fig_height, units = "in")
plot_n_s
dev.off()

emf("mbti_t_f.emf", width = fig_width, height = fig_height, units = "in")
plot_t_f
dev.off()

emf("mbti_j_p.emf", width = fig_width, height = fig_height, units = "in")
plot_j_p
dev.off()

emf("mbti_a_t.emf", width = fig_width, height = fig_height, units = "in")
plot_a_t
dev.off()

# calculate means
d_mbti_2026_percfix %>% 
  group_by(mbti_extrovert) %>% 
  summarise(mean(mbti_perc_e_i, na.rm = TRUE),
sd(mbti_perc_e_i, na.rm = TRUE))
d_mbti_2026_percfix %>% 
  group_by(mbti_intuitive) %>% 
  summarise(mean(mbti_perc_n_s, na.rm = TRUE),
            sd(mbti_perc_n_s, na.rm = TRUE))
d_mbti_2026_percfix %>% 
  group_by(mbti_feeling) %>% 
  summarise(mean(mbti_perc_t_f, na.rm = TRUE),
            sd(mbti_perc_t_f, na.rm = TRUE))
d_mbti_2026_percfix %>% 
  group_by(mbti_judging) %>% 
  summarise(mean(mbti_perc_j_p, na.rm = TRUE),
            sd(mbti_perc_j_p, na.rm = TRUE))
d_mbti_2026_percfix %>% 
  group_by(mbti_assertive) %>% 
  summarise(mean(mbti_perc_a_t, na.rm = TRUE),
            sd(mbti_perc_a_t, na.rm = TRUE))

#----------------------------------- gender

# For this analyses, unavailable,
# pseudoanonymous data is used. 
# code is shown for transparency.

d_personality = read_csv2("personality_nonanon.csv")
d_personality_med = d_personality %>% filter(study_program == "MEDISIN")
d_personality_med = d_personality_med %>% mutate(first_diploma = ifelse(age <=21, "First diploma", "Ordinary admission"))
nrow(d_personality_med %>% filter(is.na(mbti_personality))) 

# remove dups, remove missing
# n=416. Out of 434 that reported this data, 18 were missing MBTI.
d_pers_nomissing = d_personality_med %>% distinct(person_id, .keep_all = TRUE)

# find number of students with admissions quotas for medical student characteristics table
d_pers_nomissing %>% count(cohort_name, first_diploma)
pos_dups = d_personality_med %>% select(person_id) %>% duplicated(.) %>% which(.)
d_personality_med %>% slice(pos_dups) # 

# remove missing MBTI data
d_pers_nomissing = d_pers_nomissing %>% filter(!is.na(mbti_personality))
d_pers_nomissing %>% count(cohort_name)

# function for calculating percentages per group
# and add CIs
calc_prop_group = function(d, var, group){
  var = enquo(var)
  group = enquo(group)
  var_string = rlang::as_string(rlang::quo_name(var))
  
  # finds all potential values from codebook
  all_values = clean_codebook(d_codebook) %>% 
    filter(variable_id == var_string) %>% 
    pull(cat_value) 
  
  d = d %>% 
    mutate(var_fac = factor(x = !!var, levels = all_values)) %>%  
    count(!!group, var_fac) %>% 
    complete(!!group, var_fac) %>% 
    group_by(!!group) %>% 
    mutate(n = ifelse(is.na(n), 0, n),
           denom = sum(n), 
           prop = n/denom,
           prop = ifelse(n == 0, 0, prop)
    )
  
  # return to correct varnames
  varnames = names(d)
  pos_var = which(varnames=="var_fac")
  names(d)[pos_var] = var_string
  
  calc_prop_cis(d, !!var)
}

d_gender_counts = d_pers_nomissing %>% count(gender)

d_props_mbti_gender = 
  calc_prop_group(d_pers_nomissing, mbti_personality, gender) %>% 
  left_join(d_gender_counts, by = c("n")) %>% 
  arrange(gender, desc(prop))

d_props_mbti_gender = d_props_mbti_gender %>% 
  left_join(d_mbti_codes, by = c("mbti_type" = "cat_value"))

d_props_mbti_gender = d_props_mbti_gender %>% 
                      mutate(gender = ifelse(gender == 0, "Female", "Male"),
                             prop_text = paste0(prop, "% (", lower, "–", upper, ")"))

d_props_mbti_gender = d_props_mbti_gender %>% 
  select(MBTI=cat_value_label, n=x, denom=n, prop_text, gender)

d_props_mbti_gender_wide = d_props_mbti_gender %>% pivot_wider(id_cols = MBTI, values_from = 2:4, names_from = gender)

flex_mbti_gender = d_props_mbti_gender_wide %>% as_flextable(max_row=(nrow(d_props_mbti_gender_wide)))
flex_mbti_gender = set_table_properties(flex_mbti_gender, layout = "autofit")
save_as_docx(flex_mbti_gender, path =  paste0(folder_wd, "mbti_gender.docx"))

contingency_table_mbti_gender <- table(d_pers_nomissing$mbti_personality, d_pers_nomissing$gender)
res = chisq.test(contingency_table_mbti_gender)

# save as dataset for study comparisons
write_excel_csv2(d_props_mbti_gender_wide, "mbti_per_gender.csv", na = "")

# born abroad, and new to Oslo

contingency_table_mbti_move <- table(d_pers_nomissing$mbti_personality, d_pers_nomissing$mover)
res_move = chisq.test(contingency_table_mbti_move)

contingency_table_mbti_foreign <- table(d_pers_nomissing$mbti_personality, d_pers_nomissing$foreign)
res_foreign = chisq.test(contingency_table_mbti_foreign)

contingency_table_mbti_edu <- table(d_pers_nomissing$mbti_personality, d_pers_nomissing$education)
res_edu = chisq.test(contingency_table_mbti_edu)

res_move
res_foreign
res_edu

#-------------------------------- admissions quota

d_prop_first_diploma = d_pers_nomissing %>% 
  group_by(first_diploma) %>% 
  count(mbti_personality_text) %>% 
  mutate(denom = sum(n), prop = n/denom) %>% 
  ungroup() 

d_quota_counts = d_pers_nomissing %>% count(first_diploma)

d_props_mbti_quota = 
  calc_prop_group(d_pers_nomissing, mbti_personality, first_diploma) %>% 
  left_join(d_quota_counts, by = c("n")) %>% 
  arrange(first_diploma, desc(prop))

d_props_mbti_quota = d_props_mbti_quota %>% 
  left_join(d_mbti_codes, by = c("mbti_type" = "cat_value"))

d_props_mbti_quota = d_props_mbti_quota %>% 
  mutate(prop_text = paste0(prop, "% (", lower, "–", upper, ")"))

d_props_mbti_quota = d_props_mbti_quota %>% 
  select(MBTI=cat_value_label, n=x, denom=n, prop_text, first_diploma)

d_props_mbti_quota_wide = d_props_mbti_quota %>% pivot_wider(id_cols = MBTI, values_from = 2:4, names_from = first_diploma)

flex_mbti_quota = d_props_mbti_quota_wide %>% as_flextable(max_row=(nrow(d_props_mbti_quota_wide)))
flex_mbti_quota = set_table_properties(flex_mbti_quota, layout = "autofit")
save_as_docx(flex_mbti_quota, path =  paste0(folder_wd, "mbti_quota.docx"))

contingency_table_mbti_quota <- table(d_pers_nomissing$mbti_personality, d_pers_nomissing$first_diploma)
res = chisq.test(contingency_table_mbti_quota)

#----------------- making figure

d_cohort_counts_nodups = d_mbti_anon_nodups %>% count(cohort_name)
d_props_mbti_cohorts = 
  calc_prop_group(d_mbti_anon_nodups, mbti_personality, cohort_name) %>% 
  left_join(d_cohort_counts_nodups, by = c("n")) %>% 
  arrange(cohort_name, desc(prop))


# ggplot code generated with ChatGPT
# uncessarily long, but it works
d_first_dip_w_cis = binom.confint(d_prop_first_diploma$n, d_prop_first_diploma$denom, conf.level = 0.95, methods = "wilson") %>% 
  tibble() %>% 
  mutate_at(c("mean", "lower", "upper"), ~round(.*100)) %>% 
  mutate(mbti_personality_text = d_prop_first_diploma %>% pull(mbti_personality_text)) %>% 
  mutate(first_diploma = d_prop_first_diploma %>% pull(first_diploma)) %>% 
  rename(prop = mean) %>% 
  mutate(
    prop = prop/100,
    lower = lower/100,
    upper = upper/100) %>% 
  select(first_diploma, mbti_personality_text, n=x, denom=n, prop, lower, upper)


vec_isfp = tribble(~first_diploma, ~mbti_personality_text, ~n, ~denom, ~prop, ~lower, ~upper,
                   "Ordinary admission",
                   "ISFP",
                   0,
                   192,
                   0,
                   0,
                   0)

## Types ordered by frequency in Norwegian medical students
types <- c("ENFJ","ESFJ","ENTJ","INFJ","ENFP","INTJ",
           "ESTJ","ISFJ","ISTJ","ESFP","ENTP","ESTP",
           "INFP","INTP","ISFP","ISTP")

d_prop_first_diploma_compl = 
  bind_rows(d_first_dip_w_cis, vec_isfp) %>% 
  arrange(desc(prop)) %>% 
  mutate(mbti_personality_text_fct = factor(mbti_personality_text,  levels = types),)

pd = position_dodge(width = 0.8)
plot = ggplot(d_prop_first_diploma_compl, aes(x = mbti_personality_text_fct, y = prop, fill = first_diploma)) +
  geom_col(position = pd, width = 0.7) +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    position = pd,
    width = 0.2
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05)),
    labels = lmisc::axis_percent
  ) +
  scale_fill_manual(
    values = c(
      "First diploma" = lmisc::uio_distinct[2],
      "Ordinary admission"   = "burlywood4"
    )
  ) +
  labs(
    y   = "",
    x   = "",
    fill = ""
  ) +
  lmisc::theme_uio(18) +
  theme(
    axis.text.x         = element_text(angle = 45, hjust = 1),
    legend.position     = c(.95, .95),
    legend.justification = c("right", "top"),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor    = element_blank()
  )


##
png("Figure_MBTI_16types_comparison_quota.png", 
    width  = 12,
    height = 6,
    units = "in",
    res = 900)
plot
dev.off()

emf("Figure_MBTI_16types_comparison_quota.emf", 
    width  = 12,
    height = 6,
    units = "in")
plot
dev.off()

# test differences
d_diploma_e = d_pers_nomissing %>% 
  count(mbti_extrovert, first_diploma) %>% 
  group_by(first_diploma) %>% mutate(denom = sum(n), perc = (n/denom)*100)
d_diploma_n = d_pers_nomissing %>% 
  count(mbti_intuitive, first_diploma) %>% 
  group_by(first_diploma) %>% mutate(denom = sum(n), perc = (n/denom)*100)
d_diploma_f = d_pers_nomissing %>% 
  count(mbti_feeling, first_diploma) %>% 
  group_by(first_diploma) %>% mutate(denom = sum(n), perc = (n/denom)*100)
d_diploma_j = d_pers_nomissing %>% 
  count(mbti_judging, first_diploma) %>% 
  group_by(first_diploma) %>% mutate(denom = sum(n), perc = (n/denom)*100)

calc_cis_dicho = function(d, var){
  var = enquo(var)
binom.confint(d$n, d$denom, conf.level = 0.95, methods = "wilson") %>% 
    tibble() %>% 
    mutate_at(c("mean", "lower", "upper"), ~round(.*100)) %>%  
    mutate(mbti = d %>% pull(!!var)) %>% 
    mutate(first_diploma = d %>% pull(first_diploma)) %>% 
    rename(prop = mean) %>% 
    select(first_diploma, mbti, n=x, denom=n, prop, lower, upper)
}

contingency_table_e <- table(d_pers_nomissing$mbti_extrovert, d_pers_nomissing$first_diploma)
res = chisq.test(contingency_table_e)
final_e = calc_cis_dicho(d_diploma_e, mbti_extrovert) %>% mutate(p = res$p.value)

contingency_table_n <- table(d_pers_nomissing$mbti_intuitive, d_pers_nomissing$first_diploma)
res_n = chisq.test(contingency_table_n)
final_n = calc_cis_dicho(d_diploma_n, mbti_intuitive)%>% mutate(p = res_n$p.value)

contingency_table_f <- table(d_pers_nomissing$mbti_feeling, d_pers_nomissing$first_diploma)
res_f = chisq.test(contingency_table_f)
final_f = calc_cis_dicho(d_diploma_f, mbti_feeling)%>% mutate(p = res_f$p.value, mbti = as.character(mbti))


contingency_table_j <- table(d_pers_nomissing$mbti_judging, d_pers_nomissing$first_diploma)
res_j = chisq.test(contingency_table_j)
final_j = calc_cis_dicho(d_diploma_j, mbti_judging)%>% mutate(p = res_j$p.value)

d_admission_results = bind_rows(final_e, final_n, final_f, final_j)

# save table in word
sect_properties = prop_section(
  page_size = page_size(orient = "landscape")
)
flex_mbti_chisquare_quota = d_admission_results %>% as_flextable(max_row=(nrow(d_admission_results)))
flex_mbti_chisquare_quota = set_table_properties(flex_mbti_chisquare_quota, layout = "autofit")
save_as_docx(flex_mbti_chisquare_quota, path =  paste0(folder_wd, "admissions_quota_chisquare.docx"), pr_section = sect_properties)

