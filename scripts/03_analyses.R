#ReadMe----
#Purpose: Generating tables for analysis
#Author: Andrew Avitabile

#General----
#Load packages
pacman::p_load(conflicted, here, tidyverse, fixest, modelsummary, tinytable, car, gtheory)

# Remove everything
rm(list=ls())

# Set output filepath - YOU'LL NEED TO UPDATE THIS 
output_path <- "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables"

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

#Load data
analysis_data <- readRDS(here("processed data", "analysis_data.RDS"))

#Predicting feedback----

# Observation order
model1 <- feglm(strengths_mentioned_feed ~ i(observation_order, ref = 1), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model2 <- feglm(specific_examples_feed ~ i(observation_order, ref = 1), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model3 <- feglm(areas_for_growth_feed ~ i(observation_order, ref = 1), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model4 <- feglm(next_steps_feed ~ i(observation_order, ref = 1), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model5 <- feglm(strengths_mentioned_ref ~ i(observation_order, ref = 1), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model6 <- feglm(specific_examples_ref ~ i(observation_order, ref = 1), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model7 <- feglm(areas_for_growth_ref ~ i(observation_order, ref = 1), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model8 <- feglm(next_steps_ref ~ i(observation_order, ref = 1), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
models <- list(model1,model2,model3,model4,model5,model6,model7,model8)

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
writeLines(table, file.path(output_path, "Pred_feedback_1_obs_order.tex"))

# Cert area
model1 <- feglm(strengths_mentioned_feed ~ i(certification, ref = "EC-6"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model2 <- feglm(specific_examples_feed ~ i(certification, ref = "EC-6"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model3 <- feglm(areas_for_growth_feed ~ i(certification, ref = "EC-6"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model4 <- feglm(next_steps_feed ~ i(certification, ref = "EC-6"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model5 <- feglm(strengths_mentioned_ref ~ i(certification, ref = "EC-6"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model6 <- feglm(specific_examples_ref ~ i(certification, ref = "EC-6"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model7 <- feglm(areas_for_growth_ref ~ i(certification, ref = "EC-6"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model8 <- feglm(next_steps_ref ~ i(certification, ref = "EC-6"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
models <- list(model1,model2,model3,model4,model5,model6,model7,model8)

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
writeLines(table, file.path(output_path, "Pred_feedback_2_cert_area.tex"))

# BLUPs
model1 <- feglm(strengths_mentioned_feed ~ sup_blup_std + pst_blup_std + sch_blup_std, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model2 <- feglm(specific_examples_feed ~ sup_blup_std + pst_blup_std + sch_blup_std, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model3 <- feglm(areas_for_growth_feed ~ sup_blup_std + pst_blup_std + sch_blup_std, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model4 <- feglm(next_steps_feed ~ sup_blup_std + pst_blup_std + sch_blup_std, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model5 <- feglm(strengths_mentioned_ref ~ sup_blup_std + pst_blup_std + sch_blup_std, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model6 <- feglm(specific_examples_ref ~ sup_blup_std + pst_blup_std + sch_blup_std, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model7 <- feglm(areas_for_growth_ref ~ sup_blup_std + pst_blup_std + sch_blup_std, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model8 <- feglm(next_steps_ref ~ sup_blup_std + pst_blup_std + sch_blup_std, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
models <- list(model1,model2,model3,model4,model5,model6,model7,model8)

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
writeLines(table, file.path(output_path, "Pred_feedback_3_blups.tex"))

# Student teaching school
model1 <- feglm(strengths_mentioned_feed ~ st_advantage_index + suspensions_instances + st_local, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model2 <- feglm(specific_examples_feed ~ st_advantage_index + suspensions_instances + st_local, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model3 <- feglm(areas_for_growth_feed ~ st_advantage_index + suspensions_instances + st_local, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model4 <- feglm(next_steps_feed ~ st_advantage_index + suspensions_instances + st_local, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model5 <- feglm(strengths_mentioned_ref ~ st_advantage_index + suspensions_instances + st_local, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model6 <- feglm(specific_examples_ref ~ st_advantage_index + suspensions_instances + st_local, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model7 <- feglm(areas_for_growth_ref ~ st_advantage_index + suspensions_instances + st_local, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model8 <- feglm(next_steps_ref ~ st_advantage_index + suspensions_instances + st_local, data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
models <- list(model1,model2,model3,model4,model5,model6,model7,model8)

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
writeLines(table, file.path(output_path, "Pred_feedback_4_placement_school.tex"))

# PST demographics
model1 <- feglm(strengths_mentioned_feed ~ i(sex, ref = "Female") + i(race, ref = "White") + i(sat_score_cat, ref = "1000-1290"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model2 <- feglm(specific_examples_feed ~ i(sex, ref = "Female") + i(race, ref = "White") + i(sat_score_cat, ref = "1000-1290"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model3 <- feglm(areas_for_growth_feed ~ i(sex, ref = "Female") + i(race, ref = "White") + i(sat_score_cat, ref = "1000-1290"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model4 <- feglm(next_steps_feed ~ i(sex, ref = "Female") + i(race, ref = "White") + i(sat_score_cat, ref = "1000-1290"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model5 <- feglm(strengths_mentioned_ref ~ i(sex, ref = "Female") + i(race, ref = "White") + i(sat_score_cat, ref = "1000-1290"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model6 <- feglm(specific_examples_ref ~ i(sex, ref = "Female") + i(race, ref = "White") + i(sat_score_cat, ref = "1000-1290"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model7 <- feglm(areas_for_growth_ref ~ i(sex, ref = "Female") + i(race, ref = "White") + i(sat_score_cat, ref = "1000-1290"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
model8 <- feglm(next_steps_ref ~ i(sex, ref = "Female") + i(race, ref = "White") + i(sat_score_cat, ref = "1000-1290"), data = analysis_data, family = binomial(link = "logit"), cluster = "pst_id")
models <- list(model1,model2,model3,model4,model5,model6,model7,model8)

cm <- c("sex::Male" = "\\hspace{3mm} Male",
        "race::Hispanic" = "\\hspace{3mm} Hispanic",
        "race::Black" = "\\hspace{3mm} Black",
        "race::Asian or Pacific Islander" = "\\hspace{3mm} Asian or Pacific Islander",
        "race::Other" = "\\hspace{3mm} Other",
        "sat_score_cat::<1000" = "\\hspace{3mm} Less than 1000",
        "sat_score_cat::1300-1600" = "\\hspace{3mm} 1300 to 1600")
table <- modelsummary(models = models,
                      exponentiate = T,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm) %>%
  group_tt(i = list("\\hspace{1mm} \\textit{Sex (Base = Female)}" = 1,
                    "\\hspace{1mm} \\textit{Race/Ethnicity (Base = White)}" = 3,
                    "\\hspace{1mm} \\textit{SAT Score (Base = 1000 to 1290)}" = 11)) %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, file.path(output_path, "Pred_feedback_5_demographics.tex"))

#Predicting evaluation score----
model1 <- feols(avg_eval_score_std ~ area_for_improvement_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(avg_eval_score_std ~ area_for_improvement_feed | pst_id + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(avg_eval_score_std ~ area_for_improvement_feed | supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement_feed),area_for_improvement_feed=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(avg_eval_score_std ~ area_for_improvement_feed | pst_id + observation_order, data = rename(select(analysis_data,-area_for_improvement_feed),area_for_improvement_feed=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)

# Create combined regression table
models <- list(model1,model2,model3,model4)
etable(models)

cm <- c(
  "area_for_improvement_feedClassroom Management" = "\\hspace{3mm} Classroom Management",
  "area_for_improvement_feedNone" = "\\hspace{3mm} No Area",
  "area_for_improvement_feedTie" = "\\hspace{3mm} Tie",
  "area_for_improvement_feedStudent Engagement" = "\\hspace{3mm} Student Engagement",
  "area_for_improvement_feedLesson Planning" = "\\hspace{3mm} Lesson Planning",
  "area_for_improvement_feedCommunication" = "\\hspace{3mm} Communication",
  "area_for_improvement_feedAssessment and Feedback" = "\\hspace{3mm} Assessment and Feedback",
  "area_for_improvement_feedStudent Comprehension" = "\\hspace{3mm} Student Comprehension",
  "area_for_improvement_feedDifferentiation" = "\\hspace{3mm} Differentiation",
  "area_for_improvement_feedMultiple Areas" = "\\hspace{3mm} Multiple Areas",
  "area_for_improvement_feedOther" = "\\hspace{3mm} Other Area"
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
hypothesis_afi <- "area_for_improvement_feedClassroom Management = area_for_improvement_feedLesson Planning + area_for_improvement_feedStudent Engagement + area_for_improvement_feedCommunication + area_for_improvement_feedAssessment and Feedback + area_for_improvement_feedStudent Comprehension + area_for_improvement_feedDifferentiation + area_for_improvement_feedMultiple Areas + area_for_improvement_feedOther"
hypothesis_afi_ref <- "0 = area_for_improvement_feedLesson Planning + area_for_improvement_feedStudent Engagement + area_for_improvement_feedCommunication + area_for_improvement_feedAssessment and Feedback + area_for_improvement_feedStudent Comprehension + area_for_improvement_feedDifferentiation + area_for_improvement_feedMultiple Areas + area_for_improvement_feedOther"

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
writeLines(table, file.path(output_path, "Pred_Eval_Score.tex"))

#Predicting outcomes----
# Run all regressions
model1 <- feols(examscore_std_ppr ~ area_for_improvement_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(examscore_std_req ~ area_for_improvement_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(ever_enter_teaching ~ area_for_improvement_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(enter_teaching_imm ~ area_for_improvement_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model5 <- feols(same_school ~ area_for_improvement_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model6 <- feols(advantage_index ~ area_for_improvement_feed | sat_score_cat + gpa_z_cat + race + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model7 <- feols(examscore_std_ppr ~ area_for_improvement_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement_feed),area_for_improvement_feed=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)
model8 <- feols(examscore_std_req ~ area_for_improvement_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement_feed),area_for_improvement_feed=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)
model9 <- feols(ever_enter_teaching ~ area_for_improvement_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement_feed),area_for_improvement_feed=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)
model10 <- feols(enter_teaching_imm ~ area_for_improvement_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement_feed),area_for_improvement_feed=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)
model11 <- feols(same_school ~ area_for_improvement_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement_feed),area_for_improvement_feed=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)
model12 <- feols(advantage_index ~ area_for_improvement_feed | sat_score_cat + gpa_z_cat + race + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data,-area_for_improvement_feed),area_for_improvement_feed=area_for_improvement_ref), cluster = "pst_id", weights = ~inv_n_obs)

# Create combined regression table
models <- list(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12)
etable(models)

cm <- c(
  "area_for_improvement_feedClassroom Management" = "\\hspace{3mm} Classroom Management",
  "area_for_improvement_feedNone" = "\\hspace{3mm} No Area",
  "area_for_improvement_feedTie" = "\\hspace{3mm} Tie",
  "area_for_improvement_feedStudent Engagement" = "\\hspace{3mm} Student Engagement",
  "area_for_improvement_feedLesson Planning" = "\\hspace{3mm} Lesson Planning",
  "area_for_improvement_feedCommunication" = "\\hspace{3mm} Communication",
  "area_for_improvement_feedAssessment and Feedback" = "\\hspace{3mm} Assessment and Feedback",
  "area_for_improvement_feedStudent Comprehension" = "\\hspace{3mm} Student Comprehension",
  "area_for_improvement_feedDifferentiation" = "\\hspace{3mm} Differentiation",
  "area_for_improvement_feedMultiple Areas" = "\\hspace{3mm} Multiple Areas",
  "area_for_improvement_feedOther" = "\\hspace{3mm} Other Area"
)

# Classroom management binary
model1 <- feols(examscore_std_ppr ~ afi_1_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(examscore_std_req ~ afi_1_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(ever_enter_teaching ~ afi_1_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(enter_teaching_imm ~ afi_1_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model5 <- feols(same_school ~ afi_1_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model6 <- feols(advantage_index ~ afi_1_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model7 <- feols(examscore_std_ppr ~ afi_1_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data, -afi_1_feed), afi_1_feed=afi_1_ref), cluster = "pst_id", weights = ~inv_n_obs)
model8 <- feols(examscore_std_req ~ afi_1_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data, -afi_1_feed), afi_1_feed=afi_1_ref), cluster = "pst_id", weights = ~inv_n_obs)
model9 <- feols(ever_enter_teaching ~ afi_1_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data, -afi_1_feed), afi_1_feed=afi_1_ref), cluster = "pst_id", weights = ~inv_n_obs)
model10 <- feols(enter_teaching_imm ~ afi_1_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data, -afi_1_feed), afi_1_feed=afi_1_ref), cluster = "pst_id", weights = ~inv_n_obs)
model11 <- feols(same_school ~ afi_1_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data, -afi_1_feed), afi_1_feed=afi_1_ref), cluster = "pst_id", weights = ~inv_n_obs)
model12 <- feols(advantage_index ~ afi_1_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename(select(analysis_data, -afi_1_feed), afi_1_feed=afi_1_ref), cluster = "pst_id", weights = ~inv_n_obs)

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
writeLines(table, file.path(output_path, "Pred_Outcomes.tex"))

#Variance Decomposition----
formula_rate <- avg_eval_score_std ~  (1|observation_order) + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order)
formula_len_feed <- n_sentences_feed ~  (1|observation_order) + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order) 
formula_afi_feed <- areas_for_growth_feed ~  (1|observation_order) + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order) 
formula_len_ref <- n_sentences_ref ~  (1|observation_order) + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order)

# Estimate models using gstudy function
gstudy_rate <- gstudy(data = analysis_data, formula = formula_rate, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `Observation Score` = percent)
gstudy_len_feed <- gstudy(data = analysis_data, formula = formula_len_feed, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `# Sentences Feedback` = percent)
gstudy_afi_feed <- gstudy(data = analysis_data, formula = formula_afi_feed, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `Feedback Has Area for Improvement` = percent)
gstudy_len_ref <- gstudy(data = analysis_data, formula = formula_len_ref, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `# Sentences Reflection` = percent)

# Make results a table
table <- left_join(gstudy_rate, gstudy_len_feed) %>%
  left_join(gstudy_afi_feed) %>%
  left_join(gstudy_len_ref) %>%
  # left_join(gstudy_afi_ref) %>%
  mutate(Variable = case_when(source == "observation_order" ~ "Order",
                              source == "pst_id" ~ "Preservice Teacher",
                              source == "st_school_id" ~ "Placement School",
                              source == "st_school_id:observation_order" ~ "Placement School x Order",
                              source == "supervisor_id" ~ "Supervisor",
                              source == "st_school_id:supervisor_id" ~ "Placement School x Supervisor",
                              source == "supervisor_id:observation_order" ~ "Supervisor x Order",
                              source == "st_school_id:supervisor_id:observation_order" ~ "School x Supervisor x Order",
                              source == "certification" ~ "Certification Type",
                              source == "n_sentences_feed" ~ "Feedback Sentences",
                              source == "areas_for_growth_feed" ~ "Feedback Area for Improvement",
                              source == "Residual" ~ "Residual")) %>%
  dplyr::select(Variable, `Observation Score`, `# Sentences Feedback`, `Feedback Has Area for Improvement`, `# Sentences Reflection`) %>%
  tinytable::tt() %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) 

# Save to LaTeX
writeLines(table, file.path(output_path, "Variance Decomposition.tex"))

# Relationship between feedback and reflections----
model1 <- feglm(strengths_mentioned_ref ~ strengths_mentioned_feed, data=analysis_data, cluster = "pst_id", family = binomial(link = "logit"))
model2 <- feglm(strengths_mentioned_ref ~ strengths_mentioned_feed + specific_examples_feed + areas_for_growth_feed + next_steps_feed, data=analysis_data, cluster = "pst_id", family = binomial(link = "logit"))
model3 <- feglm(specific_examples_ref ~ specific_examples_feed, data=analysis_data, cluster = "pst_id", family = binomial(link = "logit"))
model4 <- feglm(specific_examples_ref ~ strengths_mentioned_feed + specific_examples_feed + areas_for_growth_feed + next_steps_feed, data=analysis_data, cluster = "pst_id", family = binomial(link = "logit"))
model5 <- feglm(areas_for_growth_ref ~ areas_for_growth_feed, data=analysis_data, cluster = "pst_id", family = binomial(link = "logit"))
model6 <- feglm(areas_for_growth_ref ~ strengths_mentioned_feed + specific_examples_feed + areas_for_growth_feed + next_steps_feed, data=analysis_data, cluster = "pst_id", family = binomial(link = "logit"))
model7 <- feglm(next_steps_ref ~ next_steps_feed, data=analysis_data, cluster = "pst_id", family = binomial(link = "logit"))
model8 <- feglm(next_steps_ref ~ strengths_mentioned_feed + specific_examples_feed + areas_for_growth_feed + next_steps_feed, data=analysis_data, cluster = "pst_id", family = binomial(link = "logit"))
models <- list(model1,model2,model3,model4,model5,model6,model7,model8)

cm <- c("strengths_mentioned_feed" = "\\hspace{3mm} PST Strengths",
        "specific_examples_feed" = "\\hspace{3mm} Specific Examples",
        "areas_for_growth_feed" = "\\hspace{3mm} Area for Improvement",
        "next_steps_feed" = "\\hspace{3mm} Actionable Steps")
table <- modelsummary(models = models,
                      exponentiate = T,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm) %>%
  group_tt(i = list("\\textbf{Supervisor Feedback}" = 1)) %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, file.path(output_path, "Predicting_Ref_w_Feedback.tex"))
