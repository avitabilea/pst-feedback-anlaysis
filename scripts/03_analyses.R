#ReadMe----
#Purpose: Generating tables for analysis
#Author: Andrew Avitabile

#General----
#Load packages
pacman::p_load(conflicted, here, tidyverse, sandwich, lmtest, modelsummary, tinytable, car, gtheory, fixest)

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

# Load a function that create both univariate and bivariate regression results
source(here("scripts", "XX_univar_and_multivar_feols.R"))

#Load data
analysis_data <- readRDS(here("processed data", "analysis_data.RDS"))

# Define a function to fit clustered models
fit_clustered_model <- function(formula, data, cluster_var) {
  # Convert the formula to a character string to analyze it
  formula_str <- as.character(formula)
  
  # Determine the dependent variable name
  dep_var <- formula_str[2]
  
  # Count the number of coefficients by creating a model matrix
  model_matrix <- model.matrix(formula, data)
  n_coefs <- ncol(model_matrix)
  
  # Fit the model using glm with log link and appropriate starting values
  model <- try(glm(
    formula, 
    data = data, 
    family = binomial(link = "log"),
    start = c(log(mean(data[[dep_var]])), rep(0, n_coefs - 1))
  ), silent = TRUE)
  
  # Apply clustered standard errors
  clustered_vcov <- vcovCL(model, cluster = data[[cluster_var]])
  clustered_model <- list(
    model = model,
    vcov = clustered_vcov
  )
  
  return(clustered_model)
}

#Predicting feedback----
# Observation order
model1 <- fit_clustered_model(strengths_mentioned_feed ~ factor(observation_order), analysis_data, "pst_id")
model2 <- fit_clustered_model(specific_examples_feed ~ factor(observation_order), analysis_data, "pst_id")
model3 <- fit_clustered_model(areas_for_growth_feed ~ factor(observation_order), analysis_data, "pst_id")
model4 <- fit_clustered_model(next_steps_feed ~ factor(observation_order), analysis_data, "pst_id")
model5 <- fit_clustered_model(strengths_mentioned_ref ~ factor(observation_order), analysis_data, "pst_id")
model6 <- fit_clustered_model(specific_examples_ref ~ factor(observation_order), analysis_data, "pst_id")
model7 <- fit_clustered_model(areas_for_growth_ref ~ factor(observation_order), analysis_data, "pst_id")
model8 <- fit_clustered_model(next_steps_ref ~ factor(observation_order), analysis_data, "pst_id")

# Extract the model objects for modelsummary
models <- list(model1$model, model2$model, model3$model, model4$model, model5$model, model6$model, model7$model, model8$model)

# Create a list of vcov matrices for modelsummary
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov, model5$vcov, model6$vcov, model7$vcov, model8$vcov)

cm <- c("factor(observation_order)2" = "\\hspace{3mm} 2",
        "factor(observation_order)3" = "\\hspace{3mm} 3",
        "factor(observation_order)4" = "\\hspace{3mm} 4")
table <- modelsummary(models = models, 
                      vcov = vcov_list,
                      exponentiate = TRUE,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.') %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, file.path(output_path, "Pred_feedback_1_obs_order.tex"))

# Cert area
model1 <- fit_clustered_model(strengths_mentioned_feed ~ factor(certification), analysis_data, "pst_id")
model2 <- fit_clustered_model(specific_examples_feed ~ factor(certification), analysis_data, "pst_id")
model3 <- fit_clustered_model(areas_for_growth_feed ~ factor(certification), analysis_data, "pst_id")
model4 <- fit_clustered_model(next_steps_feed ~ factor(certification), analysis_data, "pst_id")
model5 <- fit_clustered_model(strengths_mentioned_ref ~ factor(certification), analysis_data, "pst_id")
model6 <- fit_clustered_model(specific_examples_ref ~ factor(certification), analysis_data, "pst_id")
model7 <- fit_clustered_model(areas_for_growth_ref ~ factor(certification), analysis_data, "pst_id")
model8 <- fit_clustered_model(next_steps_ref ~ factor(certification), analysis_data, "pst_id")

# Extract models and vcov
models <- list(model1$model, model2$model, model3$model, model4$model, model5$model, model6$model, model7$model, model8$model)
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov, model5$vcov, model6$vcov, model7$vcov, model8$vcov)

cm <- c("factor(certification)4-8 English/SS" = "\\hspace{3mm} 4-8 English/SS",
        "factor(certification)4-8 Math/Sci" = "\\hspace{3mm} 4-8 Math/Sci",
        "factor(certification)HS Subjects" = "\\hspace{3mm} HS Subjects")
table <- modelsummary(models = models,
                      vcov = vcov_list,
                      exponentiate = TRUE,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.') %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, file.path(output_path, "Pred_feedback_2_cert_area.tex"))

# BLUPs
model1 <- fit_clustered_model(strengths_mentioned_feed ~ sup_blup_std + pst_blup_std + sch_blup_std, analysis_data, "pst_id")
model2 <- fit_clustered_model(specific_examples_feed ~ sup_blup_std + pst_blup_std + sch_blup_std, analysis_data, "pst_id")
model3 <- fit_clustered_model(areas_for_growth_feed ~ sup_blup_std + pst_blup_std + sch_blup_std, analysis_data, "pst_id")
model4 <- fit_clustered_model(next_steps_feed ~ sup_blup_std + pst_blup_std + sch_blup_std, analysis_data, "pst_id")
model5 <- fit_clustered_model(strengths_mentioned_ref ~ sup_blup_std + pst_blup_std + sch_blup_std, analysis_data, "pst_id")
model6 <- fit_clustered_model(specific_examples_ref ~ sup_blup_std + pst_blup_std + sch_blup_std, analysis_data, "pst_id")
model7 <- fit_clustered_model(areas_for_growth_ref ~ sup_blup_std + pst_blup_std + sch_blup_std, analysis_data, "pst_id")
model8 <- fit_clustered_model(next_steps_ref ~ sup_blup_std + pst_blup_std + sch_blup_std, analysis_data, "pst_id")

# Extract models and vcov
models <- list(model1$model, model2$model, model3$model, model4$model, model5$model, model6$model, model7$model, model8$model)
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov, model5$vcov, model6$vcov, model7$vcov, model8$vcov)

cm <- c("pst_blup_std" = "\\hspace{3mm} PST",
        "sup_blup_std" = "\\hspace{3mm} Supervisor",
        "sch_blup_std" = "\\hspace{3mm} Placement School")
table <- modelsummary(models = models,
                      vcov = vcov_list,
                      exponentiate = TRUE,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.') %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, file.path(output_path, "Pred_feedback_3_blups.tex"))

# Student teaching school
model1 <- fit_clustered_model(strengths_mentioned_feed ~ st_advantage_index + suspensions_instances + st_local, analysis_data, "pst_id")
model2 <- fit_clustered_model(specific_examples_feed ~ st_advantage_index + suspensions_instances + st_local, analysis_data, "pst_id")
model3 <- fit_clustered_model(areas_for_growth_feed ~ st_advantage_index + suspensions_instances + st_local, analysis_data, "pst_id")
model4 <- fit_clustered_model(next_steps_feed ~ st_advantage_index + suspensions_instances + st_local, analysis_data, "pst_id")
model5 <- fit_clustered_model(strengths_mentioned_ref ~ st_advantage_index + suspensions_instances + st_local, analysis_data, "pst_id")
model6 <- fit_clustered_model(specific_examples_ref ~ st_advantage_index + suspensions_instances + st_local, analysis_data, "pst_id")
model7 <- fit_clustered_model(areas_for_growth_ref ~ st_advantage_index + suspensions_instances + st_local, analysis_data, "pst_id")
model8 <- fit_clustered_model(next_steps_ref ~ st_advantage_index + suspensions_instances + st_local, analysis_data, "pst_id")

# Extract models and vcov
models <- list(model1$model, model2$model, model3$model, model4$model, model5$model, model6$model, model7$model, model8$model)
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov, model5$vcov, model6$vcov, model7$vcov, model8$vcov)

cm <- c("st_advantage_index" = "\\hspace{3mm} Standardized Advantage Index",
        "suspensions_instances" = "\\hspace{3mm} Standardized Suspensions",
        "st_local" = "\\hspace{3mm} Local District")
table <- modelsummary(models = models,
                      vcov = vcov_list,
                      exponentiate = TRUE,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.') %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, file.path(output_path, "Pred_feedback_4_placement_school.tex"))

# PST demographics
model1 <- fit_clustered_model(strengths_mentioned_feed ~ factor(sex) + factor(race) + factor(sat_score_cat), analysis_data, "pst_id")
model2 <- fit_clustered_model(specific_examples_feed ~ factor(sex) + factor(race) + factor(sat_score_cat), analysis_data, "pst_id")
model3 <- fit_clustered_model(areas_for_growth_feed ~ factor(sex) + factor(race) + factor(sat_score_cat), analysis_data, "pst_id")
model4 <- fit_clustered_model(next_steps_feed ~ factor(sex) + factor(race) + factor(sat_score_cat), analysis_data, "pst_id")
model5 <- fit_clustered_model(strengths_mentioned_ref ~ factor(sex) + factor(race) + factor(sat_score_cat), analysis_data, "pst_id")
model6 <- fit_clustered_model(specific_examples_ref ~ factor(sex) + factor(race) + factor(sat_score_cat), analysis_data, "pst_id")
model7 <- fit_clustered_model(areas_for_growth_ref ~ factor(sex) + factor(race) + factor(sat_score_cat), analysis_data, "pst_id")
model8 <- fit_clustered_model(next_steps_ref ~ factor(sex) + factor(race) + factor(sat_score_cat), analysis_data, "pst_id")

# Extract models and vcov
models <- list(model1$model, model2$model, model3$model, model4$model, model5$model, model6$model, model7$model, model8$model)
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov, model5$vcov, model6$vcov, model7$vcov, model8$vcov)

outcome_means <- tibble::tribble(
  ~term,           ~model1,                                             ~model2,                                           ~model3,                                          ~model4,                                        ~model5,                                             ~model6,                                           ~model7,                                          ~model8,
  "Outcome Mean",  mean(analysis_data$strengths_mentioned_feed, na.rm = TRUE), mean(analysis_data$specific_examples_feed, na.rm = TRUE), mean(analysis_data$areas_for_growth_feed, na.rm = TRUE), mean(analysis_data$next_steps_feed, na.rm = TRUE), mean(analysis_data$strengths_mentioned_ref, na.rm = TRUE), mean(analysis_data$specific_examples_ref, na.rm = TRUE), mean(analysis_data$areas_for_growth_ref, na.rm = TRUE), mean(analysis_data$next_steps_ref, na.rm = TRUE)
)

cm <- c("factor(sex)Male" = "\\hspace{3mm} Male",
        "factor(race)Hispanic" = "\\hspace{3mm} Hispanic",
        "factor(race)Black" = "\\hspace{3mm} Black",
        "factor(race)Asian or Pacific Islander" = "\\hspace{3mm} Asian or Pacific Islander",
        "factor(race)Other" = "\\hspace{3mm} Other",
        "factor(sat_score_cat)<1000" = "\\hspace{3mm} Less than 1000",
        "factor(sat_score_cat)1300-1600" = "\\hspace{3mm} 1300 to 1600")
table <- modelsummary(models = models,
                      vcov = vcov_list,
                      exponentiate = TRUE,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm,
                      add_rows = outcome_means) %>%
  group_tt(i = list("\\hspace{1mm} \\textit{Sex (Base = Female)}" = 1,
                    "\\hspace{1mm} \\textit{Race/Ethnicity (Base = White)}" = 3,
                    "\\hspace{1mm} \\textit{SAT Score (Base = 1000 to 1290)}" = 11)) %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, file.path(output_path, "Pred_feedback_5_demographics.tex"))

#Predicting evaluation score----
model1 <- feols(avg_eval_score_std ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(avg_eval_score_std ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | pst_id + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(avg_eval_score_std ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = rename_with(select(analysis_data, -ends_with("_feed")), ~ sub("_ref$", "_feed", .), ends_with("_ref")), cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(avg_eval_score_std ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | pst_id + observation_order, data = rename_with(select(analysis_data, -ends_with("_feed")), ~ sub("_ref$", "_feed", .), ends_with("_ref")), cluster = "pst_id", weights = ~inv_n_obs)

# Create combined regression table
models <- list(model1,model2,model3,model4)
etable(models)

cm <- c(
  "no_afi_mentioned_feed" = "\\hspace{3mm} No Area",
  "classroom_management_mentioned_feed" = "\\hspace{3mm} Classroom Management",
  "lesson_planning_mentioned_feed" = "\\hspace{3mm} Lesson Planning",
  "student_engagement_mentioned_feed" = "\\hspace{3mm} Student Engagement",
  "communication_mentioned_feed" = "\\hspace{3mm} Communication",
  "assessment_feedback_mentioned_feed" = "\\hspace{3mm} Assessment and Feedback",
  "student_comprehension_mentioned_feed" = "\\hspace{3mm} Student Comprehension",
  "differentiation_mentioned_feed" = "\\hspace{3mm} Differentiation",
  "other_mentioned_feed" = "\\hspace{3mm} Other Area"
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

# Create hypothesis matrix for testing equality of classroom_management coefficient with all others
run_equality_test <- function(model) {
  # Get coefficient names for mentioned_feed variables
  coef_names <- names(coef(model))
  feed_vars <- coef_names[grepl("_mentioned_feed$", coef_names)]
  feed_vars <- feed_vars[feed_vars != "no_afi_mentioned_feed"] # Exclude no_afi if desired
  
  cm_var <- "classroom_management_mentioned_feed"
  other_vars <- feed_vars[feed_vars != cm_var]
  
  # Create hypothesis matrix for testing: classroom_management = other_var_1 = other_var_2 = ... = other_var_n
  # This requires (n-1) constraints where n is the number of variables
  hyp_matrix <- matrix(0, nrow = length(other_vars), ncol = length(coef_names))
  colnames(hyp_matrix) <- coef_names
  
  # For each row, compare classroom_management to one other variable
  for (i in seq_along(other_vars)) {
    hyp_matrix[i, cm_var] <- 1
    hyp_matrix[i, other_vars[i]] <- -1
  }
  
  # Run the F-test
  ftest_result <- linearHypothesis(model, hyp_matrix, test = "F")
  return(ftest_result)
}

# Run F-tests for each model
ftest1 <- run_equality_test(model1) %>% format_ftest()
ftest2 <- run_equality_test(model2) %>% format_ftest()
ftest3 <- run_equality_test(model3) %>% format_ftest()
ftest4 <- run_equality_test(model4) %>% format_ftest()

# Create the combined table row
ftest <- bind_cols(tibble("F-stat$\\dag$"), ftest1, ftest2, ftest3, ftest4)
attr(ftest, 'position') <- c(22)

# Continue with table creation
table <- modelsummary(models = models,
                      add_rows = ftest,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm) %>%
  group_tt(i = list("\\textbf{Area for improvement}" = 1)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()

# Save to LaTeX
writeLines(table, file.path(output_path, "Pred_Eval_Score.tex"))

#Predicting outcomes----
# Run all regressions
model1 <- feols(examscore_std_ppr ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(examscore_std_req ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(ever_enter_teaching ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(enter_teaching_imm ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model5 <- feols(same_school ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model6 <- feols(advantage_index ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model7 <- feols(examscore_std_ppr ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename_with(select(analysis_data, -ends_with("_feed")), ~ sub("_ref$", "_feed", .), ends_with("_ref")), cluster = "pst_id", weights = ~inv_n_obs)
model8 <- feols(examscore_std_req ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename_with(select(analysis_data, -ends_with("_feed")), ~ sub("_ref$", "_feed", .), ends_with("_ref")), cluster = "pst_id", weights = ~inv_n_obs)
model9 <- feols(ever_enter_teaching ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename_with(select(analysis_data, -ends_with("_feed")), ~ sub("_ref$", "_feed", .), ends_with("_ref")), cluster = "pst_id", weights = ~inv_n_obs)
model10 <- feols(enter_teaching_imm ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename_with(select(analysis_data, -ends_with("_feed")), ~ sub("_ref$", "_feed", .), ends_with("_ref")), cluster = "pst_id", weights = ~inv_n_obs)
model11 <- feols(same_school ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename_with(select(analysis_data, -ends_with("_feed")), ~ sub("_ref$", "_feed", .), ends_with("_ref")), cluster = "pst_id", weights = ~inv_n_obs)
model12 <- feols(advantage_index ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = rename_with(select(analysis_data, -ends_with("_feed")), ~ sub("_ref$", "_feed", .), ends_with("_ref")), cluster = "pst_id", weights = ~inv_n_obs)

# Create combined regression table
models <- list(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12)
etable(models)

cm <- c(
  "no_afi_mentioned_feed" = "\\hspace{3mm} No Area",
  "classroom_management_mentioned_feed" = "\\hspace{3mm} Classroom Management",
  "lesson_planning_mentioned_feed" = "\\hspace{3mm} Lesson Planning",
  "student_engagement_mentioned_feed" = "\\hspace{3mm} Student Engagement",
  "communication_mentioned_feed" = "\\hspace{3mm} Communication",
  "assessment_feedback_mentioned_feed" = "\\hspace{3mm} Assessment and Feedback",
  "student_comprehension_mentioned_feed" = "\\hspace{3mm} Student Comprehension",
  "differentiation_mentioned_feed" = "\\hspace{3mm} Differentiation",
  "other_mentioned_feed" = "\\hspace{3mm} Other Area"
)

table <- modelsummary(models = models,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm) %>%
  group_tt(i = list("\\textbf{Area for improvement}" = 1)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()

# Save to LaTeX
writeLines(table, file.path(output_path, "Pred_Outcomes.tex"))

#Variance Decomposition----
formula_rate <- avg_eval_score_std ~  observation_order + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order)
formula_len_feed <- n_sentences_feed ~  observation_order + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order) 
formula_str_feed <- strengths_mentioned_feed ~  observation_order + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order) 
formula_exp_feed <- specific_examples_feed ~  observation_order + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order) 
formula_afi_feed <- areas_for_growth_feed ~  observation_order + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order) 
formula_nxt_feed <- next_steps_feed ~  observation_order + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order)

# Estimate models using gstudy function
gstudy_rate <- gstudy(data = analysis_data, formula = formula_rate, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `Observation Score` = percent)
gstudy_len_feed <- gstudy(data = analysis_data, formula = formula_len_feed, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `# Sentences Feedback` = percent)
gstudy_str_feed <- gstudy(data = analysis_data, formula = formula_str_feed, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `Feedback Has Strength` = percent)
gstudy_exp_feed <- gstudy(data = analysis_data, formula = formula_exp_feed, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `Feedback Has Examples` = percent)
gstudy_afi_feed <- gstudy(data = analysis_data, formula = formula_afi_feed, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `Feedback Has Area for Improvement` = percent)
gstudy_nxt_feed <- gstudy(data = analysis_data, formula = formula_nxt_feed, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `Feedback Has Next Steps` = percent)

# Make results a table
table <- left_join(gstudy_rate, gstudy_len_feed) %>%
  left_join(gstudy_str_feed) %>%
  left_join(gstudy_exp_feed) %>%
  left_join(gstudy_afi_feed) %>%
  left_join(gstudy_nxt_feed) %>%
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
  # dplyr::select(Variable, `Observation Score`, `# Sentences Feedback`, `Feedback Has Area for Improvement`, `# Sentences Reflection`) %>%
  tinytable::tt() %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) 

# Save to LaTeX
writeLines(table, file.path(output_path, "Variance Decomposition.tex"))

# Relationship between feedback and reflection quality indicators----
model1 <- fit_clustered_model(strengths_mentioned_ref ~ strengths_mentioned_feed, data=analysis_data, cluster = "pst_id")
model2 <- fit_clustered_model(strengths_mentioned_ref ~ strengths_mentioned_feed + specific_examples_feed + areas_for_growth_feed + next_steps_feed, data=analysis_data, cluster = "pst_id")
model3 <- fit_clustered_model(specific_examples_ref ~ specific_examples_feed, data=analysis_data, cluster = "pst_id")
model4 <- fit_clustered_model(specific_examples_ref ~ strengths_mentioned_feed + specific_examples_feed + areas_for_growth_feed + next_steps_feed, data=analysis_data, cluster = "pst_id")
model5 <- fit_clustered_model(areas_for_growth_ref ~ areas_for_growth_feed, data=analysis_data, cluster = "pst_id")
model6 <- fit_clustered_model(areas_for_growth_ref ~ strengths_mentioned_feed + specific_examples_feed + areas_for_growth_feed + next_steps_feed, data=analysis_data, cluster = "pst_id")
model7 <- fit_clustered_model(next_steps_ref ~ next_steps_feed, data=analysis_data, cluster = "pst_id")
model8 <- fit_clustered_model(next_steps_ref ~ strengths_mentioned_feed + specific_examples_feed + areas_for_growth_feed + next_steps_feed, data=analysis_data, cluster = "pst_id")

# Extract the model objects for modelsummary
models <- list(model1$model, model2$model, model3$model, model4$model, model5$model, model6$model, model7$model, model8$model)

# Create a list of vcov matrices for modelsummary
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov, model5$vcov, model6$vcov, model7$vcov, model8$vcov)

cm <- c("strengths_mentioned_feed" = "\\hspace{3mm} PST Strengths",
        "specific_examples_feed" = "\\hspace{3mm} Specific Examples",
        "areas_for_growth_feed" = "\\hspace{3mm} Area for Improvement",
        "next_steps_feed" = "\\hspace{3mm} Actionable Steps")

table <- modelsummary(models = models, 
                      vcov = vcov_list,
                      exponentiate = TRUE,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.',
                      gof_map = gm) %>%
  group_tt(i = list("\\textbf{Supervisor Feedback}" = 1)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, file.path(output_path, "Predicting_Ref_w_Feedback.tex"))

# Relationship between feedback quality indicators and reflection length----
model1 <- bi_and_muti_variate_feols(
  dependent_var = "log(n_sentences_ref)",
  independent_vars = c("strengths_mentioned_feed", "specific_examples_feed", "areas_for_growth_feed", "next_steps_feed"),
  data = analysis_data,
  fixed_effects = c("pst_id"),
  cluster_var = "pst_id"
)

model2 <- bi_and_muti_variate_feols(
  dependent_var = "log(n_sentences_ref)",
  independent_vars = c("strengths_mentioned_feed + log(n_sentences_feed)", "specific_examples_feed + log(n_sentences_feed)", "areas_for_growth_feed + log(n_sentences_feed)", "next_steps_feed + log(n_sentences_feed)"),
  data = analysis_data,
  fixed_effects = c("pst_id"),
  cluster_var = "pst_id"
)

combined_table <- left_join(model1$table, rename(model2$table, univariate_results_2=univariate_results, multivariate_results_2 = multivariate_results)) %>%
  mutate(term = case_when(statistic == "std.error" ~ "",
                          term == "strengths_mentioned_feed" ~ "PST Strengths",
                          term == "specific_examples_feed" ~ "Specific Examples",
                          term == "areas_for_growth_feed" ~ "Areas for Improvement",
                          term == "next_steps_feed" ~ "Next Steps")) %>%
  select(-part, -statistic)

# Split the combined_table into chunks of two rows each
split_tables <- split(combined_table, ceiling(seq_along(1:nrow(combined_table))/2))

# Iterate over each chunk and create LaTeX tables
for (i in seq_along(split_tables)) {
  table_chunk <- tt(split_tables[[i]]) %>%
    save_tt("latex")
  
  # Clean the LaTeX table by extracting the relevant part
  table_chunk <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table_chunk, perl = TRUE) %>% replace_x_with_checkmark()
  
  # Define the file name for each chunk (e.g., Pred_Ref_AFR_w_Feed_AFR_part1.tex)
  file_name <- paste0(output_path, "/Predicting_Ref_Length_w_Feedback_", i, ".tex")
  
  # Save the LaTeX table
  writeLines(table_chunk, file_name)
}