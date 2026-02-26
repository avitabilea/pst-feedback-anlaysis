#ReadMe----
#Purpose: Generating tables for analysis
#Author: Andrew Avitabile

#General----
#Load packages
pacman::p_load(conflicted, here, tidyverse, sandwich, lmtest, modelsummary, tinytable, car, gtheory, fixest, 
               vtable, marginaleffects, glue, broom, lme4, broom.mixed)

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

# Formula for getting clustered standard errors out of log-binomial model
fit_clustered_model <- function(formula, data, cluster_var) {
  # Convert the formula to a character string to analyze it
  formula_str <- as.character(formula)
  
  # Determine the dependent variable name
  dep_var <- formula_str[2]
  
  # Count the number of coefficients by creating a model matrix
  model_matrix <- model.matrix(formula, data)
  n_coefs <- ncol(model_matrix)
  
  # Calculate the mean of the dependent variable
  mean_dep_var <- mean(data[[dep_var]], na.rm = TRUE)
  
  if (mean_dep_var <= 0 || mean_dep_var >= 1) {
    stop("Mean of dependent variable must be between 0 and 1 for binomial model with log link.")
  }
  
  # Fit the model using glm with binomial log link
  model <- glm(
    formula = formula, 
    data = data, 
    family = binomial(link = "log"),
    start = c(log(mean_dep_var), rep(0, n_coefs - 1)) # YOUR original starting values
  )
  
  # Clustered standard errors
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
models <- list(model1$model, model2$model, model3$model, model4$model)

# Create a list of vcov matrices for modelsummary
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov)

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
models <- list(model1$model, model2$model, model3$model, model4$model)
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov)

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
models <- list(model1$model, model2$model, model3$model, model4$model)
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov)

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
model1 <- fit_clustered_model(strengths_mentioned_feed ~ st_advantage_index + st_suspension_quartile, analysis_data, "pst_id")
model2 <- fit_clustered_model(specific_examples_feed ~ st_advantage_index + st_suspension_quartile, analysis_data, "pst_id")
model3 <- fit_clustered_model(areas_for_growth_feed ~ st_advantage_index + st_suspension_quartile, analysis_data, "pst_id")
model4 <- fit_clustered_model(next_steps_feed ~ st_advantage_index + st_suspension_quartile, analysis_data, "pst_id")
model5 <- fit_clustered_model(strengths_mentioned_ref ~ st_advantage_index + st_suspension_quartile, analysis_data, "pst_id")
model6 <- fit_clustered_model(specific_examples_ref ~ st_advantage_index + st_suspension_quartile, analysis_data, "pst_id")
model7 <- fit_clustered_model(areas_for_growth_ref ~ st_advantage_index + st_suspension_quartile, analysis_data, "pst_id")
model8 <- fit_clustered_model(next_steps_ref ~ st_advantage_index + st_suspension_quartile, analysis_data, "pst_id")

# Extract models and vcov
models <- list(model1$model, model2$model, model3$model, model4$model)
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov)

cm <- c("st_advantage_index" = "\\hspace{3mm} Standardized Advantage Index",
        "st_suspension_quartile2" = "\\hspace{3mm} 2nd",
        "st_suspension_quartile3" = "\\hspace{3mm} 3rd",
        "st_suspension_quartile4" = "\\hspace{3mm} 4th")
table <- modelsummary(models = models,
                      vcov = vcov_list,
                      exponentiate = TRUE,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.') %>%
  group_tt(i = list("\\hspace{1mm} \\textit{Suspension Quartile (Base = 1)}" = 3)) %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, file.path(output_path, "Pred_feedback_4_placement_school.tex"))

# PST demographics
model1 <- fit_clustered_model(strengths_mentioned_feed ~ factor(sex) + factor(race) + factor(sat_score_cat) + factor(degreegpr_quartile_new), analysis_data, "pst_id")
model2 <- fit_clustered_model(specific_examples_feed ~ factor(sex) + factor(race) + factor(sat_score_cat) + factor(degreegpr_quartile_new), analysis_data, "pst_id")
model3 <- fit_clustered_model(areas_for_growth_feed ~ factor(sex) + factor(race) + factor(sat_score_cat) + factor(degreegpr_quartile_new), analysis_data, "pst_id")
model4 <- fit_clustered_model(next_steps_feed ~ factor(sex) + factor(race) + factor(sat_score_cat) + factor(degreegpr_quartile_new), analysis_data, "pst_id")
model5 <- fit_clustered_model(strengths_mentioned_ref ~ factor(sex) + factor(race) + factor(sat_score_cat) + factor(degreegpr_quartile_new), analysis_data, "pst_id")
model6 <- fit_clustered_model(specific_examples_ref ~ factor(sex) + factor(race) + factor(sat_score_cat) + factor(degreegpr_quartile_new), analysis_data, "pst_id")
model7 <- fit_clustered_model(areas_for_growth_ref ~ factor(sex) + factor(race) + factor(sat_score_cat) + factor(degreegpr_quartile_new), analysis_data, "pst_id")
model8 <- fit_clustered_model(next_steps_ref ~ factor(sex) + factor(race) + factor(sat_score_cat) + factor(degreegpr_quartile_new), analysis_data, "pst_id")

# Extract models and vcov
models <- list(model1$model, model2$model, model3$model, model4$model)
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov)

outcome_means <- tibble::tribble(
  ~term,           ~model1,                                             ~model2,                                           ~model3,                                          ~model4,                                      
  "Outcome Mean",  mean(analysis_data$strengths_mentioned_feed, na.rm = TRUE), mean(analysis_data$specific_examples_feed, na.rm = TRUE), mean(analysis_data$areas_for_growth_feed, na.rm = TRUE), mean(analysis_data$next_steps_feed, na.rm = TRUE), 
)

cm <- c("factor(sex)Male" = "\\hspace{3mm} Male",
        "factor(race)Hispanic" = "\\hspace{3mm} Hispanic",
        "factor(race)Black" = "\\hspace{3mm} Black",
        "factor(race)Asian" = "\\hspace{3mm} Asian",
        "factor(sat_score_cat)<1000" = "\\hspace{3mm} Less than 1000",
        "factor(sat_score_cat)1300-1600" = "\\hspace{3mm} 1300 to 1600",
        "factor(degreegpr_quartile_new)1" = "\\hspace{3mm} GPA: Bottom Quartile",
        "factor(degreegpr_quartile_new)3" = "\\hspace{3mm} GPA: Top Quartile")
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
                    "\\hspace{1mm} \\textit{SAT Score (Base = 1000 to 1290)}" = 9,
                    "\\hspace{1mm} \\textit{GPA (Base = Middle 50\\%)}" = 13)) %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, file.path(output_path, "Pred_feedback_5_demographics.tex"))

#PST AFI with Supervisor AFI----
model1 <- fit_clustered_model(classroom_management_mentioned_ref ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + no_afi_mentioned_feed + other_mentioned_feed, analysis_data, "pst_id")
model2 <- fit_clustered_model(lesson_planning_mentioned_ref ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + no_afi_mentioned_feed + other_mentioned_feed, analysis_data, "pst_id")
model3 <- fit_clustered_model(student_engagement_mentioned_ref ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + no_afi_mentioned_feed + other_mentioned_feed, analysis_data, "pst_id")
model4 <- fit_clustered_model(communication_mentioned_ref ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + no_afi_mentioned_feed + other_mentioned_feed, analysis_data, "pst_id")
model5 <- fit_clustered_model(assessment_feedback_mentioned_ref ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + no_afi_mentioned_feed + other_mentioned_feed, analysis_data, "pst_id")
model6 <- fit_clustered_model(student_comprehension_mentioned_ref ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + no_afi_mentioned_feed + other_mentioned_feed, analysis_data, "pst_id")
model7 <- fit_clustered_model(differentiation_mentioned_ref ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + no_afi_mentioned_feed + other_mentioned_feed, analysis_data, "pst_id")
model8 <- fit_clustered_model(no_afi_mentioned_ref ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + no_afi_mentioned_feed + other_mentioned_feed, analysis_data, "pst_id")

# Extract models and vcov
models <- list(model1$model, model2$model, model3$model, model4$model, model5$model, model6$model, model7$model, model8$model)
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov, model5$vcov, model6$vcov, model7$vcov, model8$vcov)

# Create coefficient map for nice labels
cm <- c(
  "classroom_management_mentioned_feed" = "\\hspace{3mm} Classroom Management",
  "lesson_planning_mentioned_feed" = "\\hspace{3mm} Lesson Planning",
  "student_engagement_mentioned_feed" = "\\hspace{3mm} Student Engagement",
  "communication_mentioned_feed" = "\\hspace{3mm} Communication",
  "assessment_feedback_mentioned_feed" = "\\hspace{3mm} Assessment and Feedback",
  "student_comprehension_mentioned_feed" = "\\hspace{3mm} Student Comprehension",
  "differentiation_mentioned_feed" = "\\hspace{3mm} Differentiation",
  "no_afi_mentioned_feed" = "\\hspace{3mm} None"
)

outcome_means <- tibble::tribble(
  ~term, ~model1, ~model2, ~model3, ~model4, ~model5, ~model6, ~model7, ~model8,                                      
  "Outcome Mean", 
  mean(analysis_data$classroom_management_mentioned_ref, na.rm = TRUE), 
  mean(analysis_data$lesson_planning_mentioned_ref, na.rm = TRUE), 
  mean(analysis_data$student_engagement_mentioned_ref, na.rm = TRUE), 
  mean(analysis_data$communication_mentioned_ref, na.rm = TRUE), 
  mean(analysis_data$assessment_feedback_mentioned_ref, na.rm = TRUE), 
  mean(analysis_data$student_comprehension_mentioned_feed, na.rm = TRUE), 
  mean(analysis_data$differentiation_mentioned_ref, na.rm = TRUE), 
  mean(analysis_data$no_afi_mentioned_ref, na.rm = TRUE), 
)

table <- modelsummary(models = models,
             vcov = vcov_list,
             exponentiate = TRUE,
             stars = c('*' = .05, '**' = .01, '***' = .001),
             coef_map = cm,
             gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
             gof_map = gm,
             add_rows = outcome_means)  %>%
  group_tt(i = list("\\textbf{Area for Improvement}" = 1)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()

# Save to LaTeX
writeLines(table, file.path(output_path, "Pred_ref_AFI_w_feed_AFI.tex"))

#Predicting evaluation score----
# Estimate models
model1 <- feols(avg_eval_score_std ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id")
# model2 <- feols(avg_eval_score_std ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | pst_id + observation_order, data = analysis_data, cluster = "pst_id")
model3 <- feols(avg_eval_score_std ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = rename_with(select(analysis_data, -ends_with("_feed")), ~ sub("_ref$", "_feed", .), ends_with("_ref")), cluster = "pst_id")
# model4 <- feols(avg_eval_score_std ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | pst_id + observation_order, data = rename_with(select(analysis_data, -ends_with("_feed")), ~ sub("_ref$", "_feed", .), ends_with("_ref")), cluster = "pst_id")

models <- list(model1, model3)
etable(models)

# Variables to compare against classroom_management_mentioned_feed
vars <- c("lesson_planning_mentioned_feed", "differentiation_mentioned_feed", "assessment_feedback_mentioned_feed", "student_engagement_mentioned_feed", "student_comprehension_mentioned_feed", "communication_mentioned_feed", "other_mentioned_feed" )

# Create F-test hypotheses
hypothesis_vec <- glue("classroom_management_mentioned_feed = {vars}")

# Run F-tests for each model
f_results <- map(models, ~hypotheses(.x, hypothesis = hypothesis_vec))

# Determine which coefficients differ significantly from classroom_management_mentioned_feed
get_dag_flags <- function(f_test_result) {
  # Return a named logical vector indicating which variables should get a "†"
  f_test_df <- as_tibble(f_test_result)
  f_test_df |>
    filter(p.value < 0.05) |>
    pull(term) |>
    str_extract("[^=\\s]+$") |>  # Extract right-hand side variable names
    unique()
}

# Create a named list: each element contains vars to annotate for that model
dag_vars <- map(f_results, get_dag_flags)

# Store the dag_vars in a global environment for use in tidy_custom
for (i in 1:length(models)) {
  assign(paste0("dag_vars_", i), dag_vars[[i]])
}

# Create coefficient map for nice labels
cm <- c(
  "classroom_management_mentioned_feed" = "\\hspace{3mm} Classroom Management",
  "lesson_planning_mentioned_feed" = "\\hspace{3mm} Lesson Planning",
  "student_engagement_mentioned_feed" = "\\hspace{3mm} Student Engagement",
  "communication_mentioned_feed" = "\\hspace{3mm} Communication",
  "assessment_feedback_mentioned_feed" = "\\hspace{3mm} Assessment and Feedback",
  "student_comprehension_mentioned_feed" = "\\hspace{3mm} Student Comprehension",
  "differentiation_mentioned_feed" = "\\hspace{3mm} Differentiation"
)

# Make a tidy_custom function to put the dagger next to coeficients that are statistically different from Classroom Management at the 5-percent level
# tidy_custom.fixest <- function(x) {
#   # Get original tidy results
#   out <- generics::tidy(x)
#   
#   # Get model index from object
#   model_index <- as.numeric(gsub(".*_([0-9]+)$", "\\1", deparse(substitute(x))))
#   
#   # If model_index is NA, try to determine it from models list
#   if (is.na(model_index)) {
#     for (i in 1:length(models)) {
#       if (identical(x, models[[i]])) {
#         model_index <- i
#         break
#       }
#     }
#   }
#   
#   # Get relevant dag_vars
#   dag_variables <- get(paste0("dag_vars_", model_index))
#   
#   # Add dag column
#   out$dag <- ifelse(out$term %in% dag_variables, "$\\dag$", "")
#   
#   return(out)
# }

# Use modelsummary with the custom tidy function
table <- modelsummary(
  models = models,
  coef_map = cm,
  stars = c('*' = .05, '**' = .01, '***' = .001),
  gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
  gof_map = gm,
  estimate = "{estimate}{stars}") %>%
  group_tt(i = list("\\textbf{Area for Improvement}" = 1)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()

# Save to LaTeX
writeLines(table, file.path(output_path, "Pred_Eval_Score.tex"))

#Predicting outcomes----
# Run all regressions
model1 <- feols(examscore_std_ppr ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(examscore_std_req ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(ever_enter_teaching ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(enter_teaching_imm ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model5 <- feols(same_school ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model6 <- feols(advantage_index ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)

# Create combined regression table
models <- list(model1,model2,model3,model4,model5)
etable(models)

cm <- c(
  "classroom_management_mentioned_feed" = "\\hspace{3mm} Classroom Management",
  "lesson_planning_mentioned_feed" = "\\hspace{3mm} Lesson Planning",
  "student_engagement_mentioned_feed" = "\\hspace{3mm} Student Engagement",
  "communication_mentioned_feed" = "\\hspace{3mm} Communication",
  "assessment_feedback_mentioned_feed" = "\\hspace{3mm} Assessment and Feedback",
  "student_comprehension_mentioned_feed" = "\\hspace{3mm} Student Comprehension",
  "differentiation_mentioned_feed" = "\\hspace{3mm} Differentiation"
)

table <- modelsummary(models = models,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm) %>%
  group_tt(i = list("\\textbf{Area for Improvement}" = 1)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()

# Save to LaTeX
writeLines(table, file.path(output_path, "Pred_Outcomes.tex"))

#Variance Decomposition----
# formula_rate <- avg_eval_score_std ~  observation_order + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order)
# formula_len_feed <- n_sentences_feed ~  observation_order + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order) 
# formula_str_feed <- strengths_mentioned_feed ~  observation_order + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order) 
# formula_exp_feed <- specific_examples_feed ~  observation_order + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order) 
# formula_afi_feed <- areas_for_growth_feed ~  observation_order + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order) 
# formula_nxt_feed <- next_steps_feed ~  observation_order + (1|certification) + (1|pst_id) + (1|st_school_id) + (1|st_school_id:observation_order) + (1|st_school_id:supervisor_id) + (1|supervisor_id) + (1|supervisor_id:observation_order) + (1|st_school_id:supervisor_id:observation_order)

# formula_rate <- avg_eval_score_std ~ observation_order + (1|pst_id) + (1|supervisor_id)
# formula_len_feed <- n_sentences_feed ~ observation_order + (1|pst_id) + (1|supervisor_id)
# formula_str_feed <- strengths_mentioned_feed ~ observation_order + (1|pst_id) + (1|supervisor_id)
# formula_exp_feed <- specific_examples_feed ~ observation_order + (1|pst_id) + (1|supervisor_id)
# formula_afi_feed <- areas_for_growth_feed ~ observation_order + (1|pst_id) + (1|supervisor_id)
# formula_nxt_feed <- next_steps_feed ~ observation_order + (1|pst_id) + (1|supervisor_id)

# gstudy_rate <- gstudy(data = analysis_data, formula = formula_rate, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `Observation Score` = percent)
# gstudy_len_feed <- gstudy(data = analysis_data, formula = formula_len_feed, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `# Sentences Feedback` = percent)
# gstudy_str_feed <- gstudy(data = analysis_data, formula = formula_str_feed, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `Feedback Has Strength` = percent)
# gstudy_exp_feed <- gstudy(data = analysis_data, formula = formula_exp_feed, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `Feedback Has Examples` = percent)
# gstudy_afi_feed <- gstudy(data = analysis_data, formula = formula_afi_feed, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `Feedback Has Area for Improvement` = percent)
# gstudy_nxt_feed <- gstudy(data = analysis_data, formula = formula_nxt_feed, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))) %>% pluck("components") %>% dplyr::select(source, `Feedback Has Next Steps` = percent)

# Helper function to extract variance components and format output
extract_variance_components <- function(model, label) {
  vc <- as.data.frame(VarCorr(model))
  vc <- vc %>%
    select(grp = grp, var1 = vcov) %>%
    rename(source = grp, variance = var1)
  
  # Check model type: if it's glmer (no residual), add pi^2/3
  if (!"Residual" %in% vc$source) {
    vc <- add_row(vc, source = "Residual", variance = (pi^2) / 3)
  }
  
  total_var <- sum(vc$variance)
  vc <- vc %>%
    mutate(percent = 100 * variance / total_var,
           !!label := percent) %>%
    select(source, !!label)
  
  return(vc)
}

# Fit models
mod_rate <- lmer(avg_eval_score_std ~ observation_order + (1 | pst_id) + (1 | supervisor_id), data = analysis_data)
mod_len_feed <- lmer(n_sentences_feed ~ observation_order + (1 | pst_id) + (1 | supervisor_id), data = analysis_data)
mod_str_feed <- glmer(strengths_mentioned_feed ~ observation_order + (1 | pst_id) + (1 | supervisor_id), data = analysis_data, family = binomial)
mod_exp_feed <- glmer(specific_examples_feed ~ observation_order + (1 | pst_id) + (1 | supervisor_id), data = analysis_data, family = binomial)
mod_afi_feed <- glmer(areas_for_growth_feed ~ observation_order + (1 | pst_id) + (1 | supervisor_id), data = analysis_data, family = binomial)
mod_nxt_feed <- glmer(next_steps_feed ~ observation_order + (1 | pst_id) + (1 | supervisor_id), data = analysis_data, family = binomial)

# Extract components
vc_rate <- extract_variance_components(mod_rate, "Observation Score")
vc_len_feed <- extract_variance_components(mod_len_feed, "# Sentences Feedback")
vc_str_feed <- extract_variance_components(mod_str_feed, "Feedback Has Strength")
vc_exp_feed <- extract_variance_components(mod_exp_feed, "Feedback Has Examples")
vc_afi_feed <- extract_variance_components(mod_afi_feed, "Feedback Has Area for Improvement")
vc_nxt_feed <- extract_variance_components(mod_nxt_feed, "Feedback Has Next Steps")

# Join and format
table <- full_join(vc_rate, vc_len_feed, by = "source") %>%
  full_join(vc_str_feed, by = "source") %>%
  full_join(vc_exp_feed, by = "source") %>%
  full_join(vc_afi_feed, by = "source") %>%
  full_join(vc_nxt_feed, by = "source") %>%
  mutate(Variable = case_when(
    source == "observation_order" ~ "Order",
    source == "pst_id" ~ "Preservice Teacher",
    source == "supervisor_id" ~ "Supervisor",
    source == "Residual" ~ "Residual",
    TRUE ~ source  # fallback for any remaining terms
  )) %>%
  relocate(Variable) %>%
  select(-source) %>%
  mutate(across(where(is.numeric), ~round(.x, 1)))

# Create LaTeX table
tt_table <- tinytable::tt(table) %>%
  save_tt("latex")

# Extract content between \midrule and \bottomrule
latex_output <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", tt_table, perl = TRUE)

# Save LaTeX to file
writeLines(latex_output, file.path(output_path, "Variance_Decomposition.tex"))

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
feols(log(n_sentences_ref) ~ strengths_mentioned_feed + specific_examples_feed + areas_for_growth_feed + next_steps_feed, analysis_data)

model1 <- bi_and_muti_variate_feols(
  dependent_var = "log(n_sentences_ref)",
  independent_vars = c("strengths_mentioned_feed", "specific_examples_feed", "areas_for_growth_feed", "next_steps_feed"),
  data = analysis_data,
  fixed_effects = c("pst_id"),
  cluster_var = "pst_id"
)

model2 <- bi_and_muti_variate_feols(
  dependent_var = "log(n_sentences_ref)",
  independent_vars = c("strengths_mentioned_feed", "specific_examples_feed", "areas_for_growth_feed", "next_steps_feed", "log(n_sentences_feed+0.1)"),
  data = analysis_data,
  fixed_effects = c("pst_id"),
  cluster_var = "pst_id"
)

combined_table <- left_join(model1$table, rename(model2$table, univariate_results_2=univariate_results, multivariate_results_2 = `(1)`)) %>%
  mutate(term = case_when(statistic == "std.error" ~ "",
                          term == "strengths_mentioned_feed" ~ "PST Strengths",
                          term == "specific_examples_feed" ~ "Specific Examples",
                          term == "areas_for_growth_feed" ~ "Area for Improvement",
                          term == "next_steps_feed" ~ "Next Steps")) %>%
  select(-statistic, -multivariate_results)

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

# Descriptive table----
analysis_data %>%
  select(pst_id, sex, race, sat_score_cat, gpa_z_cat, degreegpr_quartile_new, faminc, father_colldeg, mother_colldeg, certification,
         pst_blup_std, sup_blup_std, sch_blup_std, st_advantage_index, st_suspension_quartile, examscore_std_ppr, 
         examscore_std_req, ever_enter_teaching, enter_teaching_imm, same_school, advantage_index) %>%
  distinct() %>%
  # Convert all factors character first so na_if works
  mutate(across(c(sex, race, sat_score_cat, gpa_z_cat, degreegpr_quartile_new, faminc, father_colldeg, mother_colldeg, st_suspension_quartile), as.character)) %>%
  # Replace special missing strings with actual NA
  mutate(across(c(sex, race, sat_score_cat, gpa_z_cat, degreegpr_quartile_new, faminc, father_colldeg, mother_colldeg, st_suspension_quartile), ~na_if(.x, "Missing"))) %>%
  mutate(faminc = na_if(faminc, "No response"), faminc = na_if(faminc, "Unknown")) %>%
  # Now convert to factors with correct levels (excluding "Missing")
  mutate(
    race = factor(race, levels = c("Asian", "Black", "Hispanic", "Other", "White, Non-Hispanic")),
    sat_score_cat = factor(sat_score_cat, levels = c("<1000", "1000-1290", "1300-1600")),
    gpa_z_cat = factor(gpa_z_cat, levels = c("Less than -1 SD", "-1 to 1 SD", "More than 1 SD")),
    faminc = factor(faminc, levels = c("<$80k", "$80k-150k", ">$150k", ">$80k, 2009 only")),
    father_colldeg = factor(father_colldeg, levels = c("Yes", "No")),
    mother_colldeg = factor(mother_colldeg, levels = c("Yes", "No")),
    ever_enter_teaching = factor(ever_enter_teaching),
    enter_teaching_imm = factor(enter_teaching_imm),
    same_school = factor(same_school),
    degreegpr_quartile_new = factor(degreegpr_quartile_new, levels = c("1", "2", "3"), labels = c("Bottom Quartile", "Middle 50%", "Top Quartile")),
    st_suspension_quartile = factor(st_suspension_quartile, levels = c("1", "2", "3", "4"))
  ) %>%
  sumtable(
    vars = c("certification", "sex", "race", "sat_score_cat", "degreegpr_quartile_new", "mother_colldeg", "father_colldeg", "faminc", "pst_blup_std", "sup_blup_std", 
            "sch_blup_std", "st_advantage_index", "st_suspension_quartile", 
            "examscore_std_ppr", "examscore_std_req", "ever_enter_teaching", "enter_teaching_imm", "same_school", "advantage_index"),
    labels = c("Certification Area", "Sex", "Race", "SAT Score", "College GPA", "Mother Has College Degree", "Father Has College Degree", "Family Income", "PST BLUP", "Supervisor BLUP", 
               "Placement School BLUP", "Standardized Advantage Index (Placement School)", "Suspensions Quartile (Placement School)",
              "PPR Exam", "Content Exam", "Entry", "Immediate Entry", "Entry Same School", "School Advantage Index"),
    summ = c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
    summ.names = c("N", "Mean", "SD", "Min.", "Max."),
    out = "latex",
    fit.page = '0.65\\textwidth',
    anchor = "tab:descriptive_table",
    # note = "Note: Table reports summary statistics for the analytic sample. The ``N'' column shows the total count for each variable and its subgroups, while the ``Percent'' column displays the corresponding percentages.",
    file = paste0(output_path, "/Descriptive_Table.tex"))

#Appendix tables----
# Outcomes Table
model1 <- feols(examscore_std_ppr ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed + classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | sat_score_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(examscore_std_req ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed + classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | sat_score_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(ever_enter_teaching ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed + classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | sat_score_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(enter_teaching_imm ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed +classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | sat_score_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model5 <- feols(same_school ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed +classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | sat_score_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model6 <- feols(advantage_index ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed + classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | sat_score_cat + race + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model7 <- feols(examscore_std_ppr ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | sat_score_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model8 <- feols(examscore_std_req ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | sat_score_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model9 <- feols(ever_enter_teaching ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | sat_score_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model10 <- feols(enter_teaching_imm ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | sat_score_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model11 <- feols(same_school ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | sat_score_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model12 <- feols(advantage_index ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | sat_score_cat + race + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)

# Create combined regression table
models <- list(model1,model2,model3,model4,model5,model7,model8,model9,model10,model11)
etable(models)

cm <- c(
  "classroom_management_mentioned_feed" = "\\hspace{3mm} Classroom Management",
  "lesson_planning_mentioned_feed" = "\\hspace{3mm} Lesson Planning",
  "student_engagement_mentioned_feed" = "\\hspace{3mm} Student Engagement",
  "communication_mentioned_feed" = "\\hspace{3mm} Communication",
  "assessment_feedback_mentioned_feed" = "\\hspace{3mm} Assessment and Feedback",
  "student_comprehension_mentioned_feed" = "\\hspace{3mm} Student Comprehension",
  "differentiation_mentioned_feed" = "\\hspace{3mm} Differentiation",
  "no_afi_mentioned_feed" = "\\hspace{3mm} None",
  # "other_mentioned_feed" = "\\hspace{3mm} Other Area",
  "classroom_management_mentioned_ref" = "\\hspace{3mm}  Classroom Management",
  "lesson_planning_mentioned_ref" = "\\hspace{3mm}  Lesson Planning",
  "student_engagement_mentioned_ref" = "\\hspace{3mm}  Student Engagement",
  "communication_mentioned_ref" = "\\hspace{3mm}  Communication",
  "assessment_feedback_mentioned_ref" = "\\hspace{3mm}  Assessment and Feedback",
  "student_comprehension_mentioned_ref" = "\\hspace{3mm}  Student Comprehension",
  "differentiation_mentioned_ref" = "\\hspace{3mm}  Differentiation",
  "no_afi_mentioned_ref" = "\\hspace{3mm}  None"
  # "other_mentioned_ref" = "\\hspace{3mm}  Other Area"
)

table <- modelsummary(models = models,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm) %>%
  group_tt(i = list("\\textbf{Supervisor Feedback}" = 1,
                    "\\textbf{PST Reflections}" = 15)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()

# Save to LaTeX
writeLines(table, file.path(output_path, "Pred_Outcomes_Appendix.tex"))

# Evaluation Score
model1 <- feols(avg_eval_score_std ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed + classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id")
model2 <- feols(avg_eval_score_std ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed + classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | pst_id + observation_order, data = analysis_data, cluster = "pst_id")

# Create combined regression table
models <- list(model1)
etable(models)

cm <- c(
  "classroom_management_mentioned_feed" = "\\hspace{3mm} Classroom Management",
  "lesson_planning_mentioned_feed" = "\\hspace{3mm} Lesson Planning",
  "student_engagement_mentioned_feed" = "\\hspace{3mm} Student Engagement",
  "communication_mentioned_feed" = "\\hspace{3mm} Communication",
  "assessment_feedback_mentioned_feed" = "\\hspace{3mm} Assessment and Feedback",
  "student_comprehension_mentioned_feed" = "\\hspace{3mm} Student Comprehension",
  "differentiation_mentioned_feed" = "\\hspace{3mm} Differentiation",
  "no_afi_mentioned_feed" = "\\hspace{3mm} None",
  # "other_mentioned_feed" = "\\hspace{3mm} Other Area",
  "classroom_management_mentioned_ref" = "\\hspace{3mm}  Classroom Management",
  "lesson_planning_mentioned_ref" = "\\hspace{3mm}  Lesson Planning",
  "student_engagement_mentioned_ref" = "\\hspace{3mm}  Student Engagement",
  "communication_mentioned_ref" = "\\hspace{3mm}  Communication",
  "assessment_feedback_mentioned_ref" = "\\hspace{3mm}  Assessment and Feedback",
  "student_comprehension_mentioned_ref" = "\\hspace{3mm}  Student Comprehension",
  "differentiation_mentioned_ref" = "\\hspace{3mm}  Differentiation",
  "no_afi_mentioned_ref" = "\\hspace{3mm}  None"
  # "other_mentioned_ref" = "\\hspace{3mm}  Other Area"
)

# Continue with table creation
table <- modelsummary(models = models,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm) %>%
  group_tt(i = list("\\textbf{Supervisor Feedback}" = 1,
                    "\\textbf{PST Reflection}" = 15)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()

# Save to LaTeX
writeLines(table, file.path(output_path, "Pred_Eval_Score_Appendix.tex"))

#Exiting the profession----
# Feedback
model1 <- feols(exit1 ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(exit2 ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(exit3 ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(exit4 ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model5 <- feols(leavesch1 ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model6 <- feols(leavesch2 ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model7 <- feols(leavesch3 ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model8 <- feols(leavesch4 ~ classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
models <- list(model1,model2,model3,model4,model5,model6,model7,model8)
etable(models)

cm <- c(
  "classroom_management_mentioned_feed" = "\\hspace{3mm} Classroom Management",
  "lesson_planning_mentioned_feed" = "\\hspace{3mm} Lesson Planning",
  "student_engagement_mentioned_feed" = "\\hspace{3mm} Student Engagement",
  "communication_mentioned_feed" = "\\hspace{3mm} Communication",
  "assessment_feedback_mentioned_feed" = "\\hspace{3mm} Assessment and Feedback",
  "student_comprehension_mentioned_feed" = "\\hspace{3mm} Student Comprehension",
  "differentiation_mentioned_feed" = "\\hspace{3mm} Differentiation"
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
writeLines(table, file.path(output_path, "Pred_Attrition.tex"))

# Reflections
model1 <- feols(exit1 ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(exit2 ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(exit3 ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(exit4 ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model5 <- feols(leavesch1 ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model6 <- feols(leavesch2 ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model7 <- feols(leavesch3 ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model8 <- feols(leavesch4 ~ classroom_management_mentioned_ref + lesson_planning_mentioned_ref + differentiation_mentioned_ref + assessment_feedback_mentioned_ref + student_engagement_mentioned_ref + student_comprehension_mentioned_ref + communication_mentioned_ref + other_mentioned_ref | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
models <- list(model1,model2,model3,model4,model5,model6,model7,model8)
etable(models)

cm <- c(
  "classroom_management_mentioned_ref" = "\\hspace{3mm} Classroom Management",
  "lesson_planning_mentioned_ref" = "\\hspace{3mm} Lesson Planning",
  "student_engagement_mentioned_ref" = "\\hspace{3mm} Student Engagement",
  "communication_mentioned_ref" = "\\hspace{3mm} Communication",
  "assessment_feedback_mentioned_ref" = "\\hspace{3mm} Assessment and Feedback",
  "student_comprehension_mentioned_ref" = "\\hspace{3mm} Student Comprehension",
  "differentiation_mentioned_ref" = "\\hspace{3mm} Differentiation"
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
writeLines(table, file.path(output_path, "Pred_Attrition_Reflections.tex"))