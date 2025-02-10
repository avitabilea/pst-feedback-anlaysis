#ReadMe----
#Purpose: Generating tables for analysis
#Author: Andrew Avitabile

#General----
#Load packages
pacman::p_load(conflicted, here, tidyverse, fixest)

# Remove everything
rm(list=ls())

# Conflict prefer
conflict_prefer(name = "filter", winner = "dplyr")

# Outcome mean function
glance_custom.fixest <- function(x, ...) {
  out <- data.frame("Mean(DV)" = as.numeric(fitstat(x, type = "my")))
  return(out)
}

# Replace X with checkmark
replace_x_with_checkmark <- function(text) {
  gsub("X", "$\\checkmark$", text, fixed = TRUE)
}

# Renaming goodness-of-fit statistics
gm <- tibble::tribble(
  ~raw,                ~clean,          ~fmt,
  "nobs",               "\\midrule N",             0,
  "r.squared",          "$R^2$",         3,
  "Mean.DV.",           "Outcome Mean",  3,
  "FE: programcohort",  "Program $\\times$ cohort FE", 0,
  "FE: supervisor_id",  "Supervisor FE", 0,
  "FE: pst_id",  "PST FE", 0,
  "FE: observation_order", "Order FE", 0,
  "FE: cohort_cl", "Cohort FE", 0)

# Renaming goodness-of-fit statistics (bivariate models)
gm_bi <- tibble::tribble(
  ~raw,                ~clean,          ~fmt,
  "nobs",               "N",             0,
  "r.squared",          "$R^2$",         3,
  "FE: programcohort",  "Program $\\times$ cohort FE", 0,
  "FE: supervisor_id",  "Supervisor FE", 0,
  "FE: pst_id",  "PST FE", 0,
  "FE: observation_order", "Order FE", 0,
  "FE: cohort", "Cohort FE", 0,
  "FE: st_school_id", "Placement School FE", 0)

#Predicting feedback----

# Observation order
model1 <- feglm(strengths_mentioned ~ i(observation_order, ref = 1), data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model2 <- feglm(specific_examples ~ i(observation_order, ref = 1), data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model3 <- feglm(areas_for_growth ~ i(observation_order, ref = 1), data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model4 <- feglm(next_steps ~ i(observation_order, ref = 1), data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
models <- list(model1,model2,model3,model4)

cm <- c("observation_order::2" = "\\hspace{3mm} 2",
        "observation_order::3" = "\\hspace{3mm} 3",
        "observation_order::4" = "\\hspace{3mm} 4")
table <- modelsummary(models = models,
                      exponentiate = T,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.') %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Pred_feedback_1_obs_order.tex")

# Cert area
model1 <- feglm(strengths_mentioned ~ i(certification, ref = "EC-6"), data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model2 <- feglm(specific_examples ~ i(certification, ref = "EC-6"), data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model3 <- feglm(areas_for_growth ~ i(certification, ref = "EC-6"), data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model4 <- feglm(next_steps ~ i(certification, ref = "EC-6"), data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
models <- list(model1,model2,model3,model4)

cm <- c("certification::4-8 English/SS" = "\\hspace{3mm} 4-8 English/SS",
        "certification::4-8 Math/Sci" = "\\hspace{3mm} 4-8 Math/Sci",
        "certification::HS Subjects" = "\\hspace{3mm} HS Subjects")
table <- modelsummary(models = models,
                      exponentiate = T,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.') %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Pred_feedback_2_cert_area.tex")

# BLUPs
model1 <- feglm(strengths_mentioned ~ sup_blup_std + pst_blup_std + sch_blup_std, data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model2 <- feglm(specific_examples ~ sup_blup_std + pst_blup_std + sch_blup_std, data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model3 <- feglm(areas_for_growth ~ sup_blup_std + pst_blup_std + sch_blup_std, data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model4 <- feglm(next_steps ~ sup_blup_std + pst_blup_std + sch_blup_std, data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
models <- list(model1,model2,model3,model4)

cm <- c("pst_blup_std" = "\\hspace{3mm} PST",
        "sup_blup_std" = "\\hspace{3mm} Supervisor",
        "sch_blup_std" = "\\hspace{3mm} Placement School")
table <- modelsummary(models = models,
                      exponentiate = T,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.') %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Pred_feedback_3_blups.tex")

# Student teaching school
model1 <- feglm(strengths_mentioned ~ st_advantage_index + suspensions_instances + st_local, data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model2 <- feglm(specific_examples ~ st_advantage_index + suspensions_instances + st_local, data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model3 <- feglm(areas_for_growth ~ st_advantage_index + suspensions_instances + st_local, data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model4 <- feglm(next_steps ~ st_advantage_index + suspensions_instances + st_local, data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
models <- list(model1,model2,model3,model4)

cm <- c("st_advantage_index" = "\\hspace{3mm} Standardized Advantage Index",
        "suspensions_instances" = "\\hspace{3mm} Standardized Suspensions",
        "st_local" = "\\hspace{3mm} Local District")
table <- modelsummary(models = models,
                      exponentiate = T,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.') %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Pred_feedback_4_placement_school.tex")

# PST demographics
model1 <- feglm(strengths_mentioned ~ i(sex, ref = "Female") + i(race, ref = "White") + i(sat_score_cat, ref = "1000-1290"), data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model2 <- feglm(specific_examples ~ i(sex, ref = "Female") + i(race, ref = "White") + i(sat_score_cat, ref = "1000-1290"), data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model3 <- feglm(areas_for_growth ~ i(sex, ref = "Female") + i(race, ref = "White") + i(sat_score_cat, ref = "1000-1290"), data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
model4 <- feglm(next_steps ~ i(sex, ref = "Female") + i(race, ref = "White") + i(sat_score_cat, ref = "1000-1290"), data = analysis_data, family = poisson(link = "log"), cluster = "pst_id")
models <- list(model1,model2,model3,model4)

cm <- c("sex::Male" = "\\hspace{3mm} Male",
        "race::Hispanic" = "\\hspace{3mm} Hispanic",
        "race::Black" = "\\hspace{3mm} Black",
        "race::Asian or Pacific Islander" = "\\hspace{3mm} Asian or Pacific Islander",
        "race::Amer Indian/Alaskan Native" = "\\hspace{3mm} American Indian/Alaskan Native",
        "sat_score_cat::<1000" = "\\hspace{3mm} Less than 1000",
        "sat_score_cat::1300-1600" = "\\hspace{3mm} 1300 to 1600")
table <- modelsummary(models = models,
                      exponentiate = T,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm__) %>%
  group_tt(i = list("\\hspace{1mm} \\textit{Sex (Base = Female)}" = 1,
                    "\\hspace{1mm} \\textit{Race/Ethnicity (Base = White)}" = 3,
                    "\\hspace{1mm} \\textit{SAT Score (Base = 1000 to 1290)}" = 11)) %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Pred_feedback_5_demographics.tex")

#Predicting evaluation score----
model1 <- feols(avg_eval_score_std ~ area_for_improvement | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(avg_eval_score_std ~ area_for_improvement | pst_id + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(avg_eval_score_std ~ area_for_improvement | supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement),area_for_improvement=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(avg_eval_score_std ~ area_for_improvement | pst_id + observation_order, data = rename(select(analysis_data,-area_for_improvement),area_for_improvement=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)

# Create combined regression table
models <- list(model1,model2,model3,model4)
etable(models)

cm <- c(
  "area_for_improvementClassroom Management" = "\\hspace{3mm} Classroom Management",
  "area_for_improvementNone" = "\\hspace{3mm} No Area",
  "area_for_improvementTie" = "\\hspace{3mm} Tie",
  "area_for_improvementStudent Engagement" = "\\hspace{3mm} Student Engagement",
  "area_for_improvementLesson Planning" = "\\hspace{3mm} Lesson Planning",
  "area_for_improvementCommunication" = "\\hspace{3mm} Communication",
  "area_for_improvementAssessment and Feedback" = "\\hspace{3mm} Assessment and Feedback",
  "area_for_improvementStudent Comprehension" = "\\hspace{3mm} Student Comprehension",
  "area_for_improvementDifferentiation" = "\\hspace{3mm} Differentiation",
  "area_for_improvementMultiple Areas" = "\\hspace{3mm} Multiple Areas",
  "area_for_improvementOther" = "\\hspace{3mm} Other Area"
)

# Function to format F-test results
format_ftest <- function(ftest_result) {
  f_stat <- round(ftest_result$F[2], 1) # Extract and round F-statistic
  p_value <- ftest_result$`Pr(>F)`[2]  # Extract p-value
  
  # Add stars for significance
  stars <- ifelse(p_value <= 0.001, "***",
                  ifelse(p_value <= 0.01, "**",
                         ifelse(p_value <= 0.05, "*", "")))
  
  # Create dataframe with F-statistic and significance
  data.frame(F = paste0(f_stat, stars))
}

# Hypotheses
hypothesis_afi <- "area_for_improvementClassroom Management = area_for_improvementLesson Planning + area_for_improvementStudent Engagement + area_for_improvementCommunication + area_for_improvementAssessment and Feedback + area_for_improvementStudent Comprehension + area_for_improvementDifferentiation + area_for_improvementMultiple Areas + area_for_improvementOther"
hypothesis_afi_ref <- "0 = area_for_improvementLesson Planning + area_for_improvementStudent Engagement + area_for_improvementCommunication + area_for_improvementAssessment and Feedback + area_for_improvementStudent Comprehension + area_for_improvementDifferentiation + area_for_improvementMultiple Areas + area_for_improvementOther"

# Run F-tests and format results
ftest1 <- linearHypothesis(model1, hypothesis_afi, test = "F") %>% format_ftest()
ftest2 <- linearHypothesis(model2, hypothesis_afi, test = "F") %>% format_ftest()
ftest3 <- linearHypothesis(model3, hypothesis_afi_ref, test = "F") %>% format_ftest()
ftest4 <- linearHypothesis(model4, hypothesis_afi_ref, test = "F") %>% format_ftest()

ftest <- bind_cols(tibble("F-stat$\\dag$"),ftest1,ftest2,ftest3,ftest4)
attr(ftest, 'position') <- c(24)
 
table <- modelsummary(models = models,
                      add_rows = ftest,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      # vcov = "bootstrap", R = 1000, 
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm) %>%
  group_tt(i = list("\\textbf{Area for improvement}" = 1)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()

# Save to LaTeX
writeLines(table, "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Pred_Eval_Score.tex")

#Predicting outcomes----
# Run all regressions
model1 <- feols(examscore_std_ppr ~ area_for_improvement | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(examscore_std_req ~ area_for_improvement | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(ever_enter_teaching ~ area_for_improvement | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(enter_teaching_imm ~ area_for_improvement | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model5 <- feols(same_school ~ area_for_improvement | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model6 <- feols(advantage_index ~ area_for_improvement | sat_score_cat + gpa_z_cat + race + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model7 <- feols(examscore_std_ppr ~ area_for_improvement | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement),area_for_improvement=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)
model8 <- feols(examscore_std_req ~ area_for_improvement | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement),area_for_improvement=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)
model9 <- feols(ever_enter_teaching ~ area_for_improvement | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement),area_for_improvement=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)
model10 <- feols(enter_teaching_imm ~ area_for_improvement | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement),area_for_improvement=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)
model11 <- feols(same_school ~ area_for_improvement | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement),area_for_improvement=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)
model12 <- feols(advantage_index ~ area_for_improvement | sat_score_cat + gpa_z_cat + race + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement),area_for_improvement=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)

# Create combined regression table
models <- list(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12)
etable(models)

cm <- c(
  "area_for_improvementClassroom Management" = "\\hspace{3mm} Classroom Management",
  "area_for_improvementNone" = "\\hspace{3mm} No Area",
  "area_for_improvementTie" = "\\hspace{3mm} Tie",
  "area_for_improvementStudent Engagement" = "\\hspace{3mm} Student Engagement",
  "area_for_improvementLesson Planning" = "\\hspace{3mm} Lesson Planning",
  "area_for_improvementCommunication" = "\\hspace{3mm} Communication",
  "area_for_improvementAssessment and Feedback" = "\\hspace{3mm} Assessment and Feedback",
  "area_for_improvementStudent Comprehension" = "\\hspace{3mm} Student Comprehension",
  "area_for_improvementDifferentiation" = "\\hspace{3mm} Differentiation",
  "area_for_improvementMultiple Areas" = "\\hspace{3mm} Multiple Areas",
  "area_for_improvementOther" = "\\hspace{3mm} Other Area"
)

# Classroom management binary
model1 <- feols(examscore_std_ppr ~ afi_1 | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(examscore_std_req ~ afi_1 | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(ever_enter_teaching ~ afi_1 | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(enter_teaching_imm ~ afi_1 | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model5 <- feols(same_school ~ afi_1 | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model6 <- feols(advantage_index ~ afi_1 | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model7 <- feols(examscore_std_ppr ~ afi_1 | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data, -afi_1), afi_1=afi_ref_1), cluster = "pst_id", weights = ~inv_n_obs)
model8 <- feols(examscore_std_req ~ afi_1 | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data, -afi_1), afi_1=afi_ref_1), cluster = "pst_id", weights = ~inv_n_obs)
model9 <- feols(ever_enter_teaching ~ afi_1 | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data, -afi_1), afi_1=afi_ref_1), cluster = "pst_id", weights = ~inv_n_obs)
model10 <- feols(enter_teaching_imm ~ afi_1 | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data, -afi_1), afi_1=afi_ref_1), cluster = "pst_id", weights = ~inv_n_obs)
model11 <- feols(same_school ~ afi_1 | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data, -afi_1), afi_1=afi_ref_1), cluster = "pst_id", weights = ~inv_n_obs)
model12 <- feols(advantage_index ~ afi_1 | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data, -afi_1), afi_1=afi_ref_1), cluster = "pst_id", weights = ~inv_n_obs)

cm_binary <- modelsummary(models = list(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12), 
                          stars = c('*' = .05, '**' = .01, '***' = .001),
                          gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors|R2|FE|N|Mean.DV',
                          output = "data.frame") %>%
  mutate(term=ifelse(row_number()!=1, "", "\\midrule Classroom Management (0/1)$\\dag$")) %>%
  select(-part,-statistic)
attr(cm_binary, 'position') <- c(21, 22)

table <- modelsummary(models = models,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm,
                      add_rows = cm_binary) %>%
  group_tt(i = list("\\textbf{Area for improvement}" = 1)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()

# Save to LaTeX
writeLines(table, "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Pred_Outcomes.tex")

#Dynamics between PST reflections and supervisor feedback----
model1 <- feols(scale(n_sentences_ref) ~ area_for_improvement | programcohort + supervisor_id + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(scale(n_sentences_ref) ~ area_for_improvement | pst_id + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(scale(n_sentences_ref) ~ scale(n_sentences_feed) | programcohort + supervisor_id + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(scale(n_sentences_ref) ~ scale(n_sentences_feed) | pst_id + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)

models <- list(model1,model2,model3,model4)
etable(models)

cm <- c(
  "area_for_improvementClassroom Management" = "\\hspace{3mm} Classroom Management",
  "area_for_improvementNone" = "\\hspace{3mm} No Area",
  "area_for_improvementTie" = "\\hspace{3mm} Tie",
  "area_for_improvementStudent Engagement" = "\\hspace{3mm} Student Engagement",
  "area_for_improvementLesson Planning" = "\\hspace{3mm} Lesson Planning",
  "area_for_improvementCommunication" = "\\hspace{3mm} Communication",
  "area_for_improvementAssessment and Feedback" = "\\hspace{3mm} Assessment and Feedback",
  "area_for_improvementStudent Comprehension" = "\\hspace{3mm} Student Comprehension",
  "area_for_improvementDifferentiation" = "\\hspace{3mm} Differentiation",
  "area_for_improvementMultiple Areas" = "\\hspace{3mm} Multiple Areas",
  "area_for_improvementOther" = "\\hspace{3mm} Other Area",
  "scale(n_sentences_feed)" = "Standardized Sentences in Feedback"
)

table <- modelsummary(models = models,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm,
                      escape = T) %>%
  group_tt(i = list("\\textbf{Area for improvement (Base=No Area)}" = 1)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark() 

# Save to LaTeX
writeLines(table, "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Pred_Reflection_Length.tex")