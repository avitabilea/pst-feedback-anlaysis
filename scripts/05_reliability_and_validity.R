#ReadMe----
#Purpose: Testing reliability and validity of coding
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

# Define a function to fit clustered models
fit_clustered_model <- function(formula, data, cluster_var = NULL) {
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
  
  # Apply appropriate variance-covariance estimation
  if (!is.null(cluster_var)) {
    # Use clustered standard errors if cluster variable is provided
    robust_vcov <- vcovCL(model, cluster = data[[cluster_var]])
  } else {
    # Default to heteroskedasticity-robust standard errors if no cluster variable
    robust_vcov <- vcovHC(model, type = "HC1")
  }
  
  # Return model with appropriate variance-covariance matrix
  robust_model <- list(
    model = model,
    vcov = robust_vcov
  )
  
  return(robust_model)
}

#Load data
analysis_data <- readRDS(here("processed data", "analysis_data.RDS"))

#Validity----
library(haven)
qual_coding <- read_dta("C:/Users/Andre/Dropbox/Andrew and Brendan Shared Folder/PST Feedback Text/data/qual coding/Refl_CTO_response_analysis_07082024.dta") %>%
  filter(!(nocriticism == 1 & (lessoncycle == 1 | lessonconnections == 1 | studentcomprehension == 1 | lessondelivery == 1 | praise == 1 | transitions == 1 | attention == 1 | nonverbaltechniques == 1 | corrections == 1)))

#Combine data sources----
qual_coding_lim <- select(qual_coding, observationid, crm, Codes1, 
                          nocriticism, AcademicsCategory, BehaviorsCategory, lessoncycle, 
                          lessonconnections, studentcomprehension, lessondelivery, praise, transitions, 
                          attention, nonverbaltechniques, corrections)

matched_data <- inner_join(analysis_data, qual_coding_lim) 

#Regressions----
model1 <- fit_clustered_model(classroom_management_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data)
model2 <- fit_clustered_model(lesson_planning_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data)
model3 <- fit_clustered_model(student_engagement_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data)
model4 <- fit_clustered_model(communication_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data)
model5 <- fit_clustered_model(assessment_feedback_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data)
model6 <- fit_clustered_model(student_comprehension_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data)
model7 <- fit_clustered_model(differentiation_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data)
model8 <- fit_clustered_model(other_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data)
model9 <- fit_clustered_model(no_afi_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data)

# Extract the model objects for modelsummary
models <- list(model1$model, model2$model, model3$model, model4$model, model5$model, model6$model, model7$model, model8$model, model9$model)

# Create a list of vcov matrices for modelsummary
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov, model5$vcov, model6$vcov, model7$vcov, model8$vcov, model9$vcov)

# Renaming coefficients
cm <- c(
  "praise" = "\\hspace{3mm} Praise",
  "transitions" = "\\hspace{3mm} Transitions",
  "attention" = "\\hspace{3mm} Attention",
  "nonverbaltechniques" = "\\hspace{3mm} Non-verbal Techniques",
  "corrections" = "\\hspace{3mm} Corrections",
  "lessoncycle" = "\\hspace{3mm} Lesson Cycle",
  "lessonconnections" = "\\hspace{3mm} Lesson Connections",
  "studentcomprehension" = "\\hspace{3mm} Student Comprehension",
  "lessondelivery" = "\\hspace{3mm} Lesson Delivery",
  "nocriticism" = "\\hspace{1mm} No criticism"
)

# Use model summary to create a nice table
table <- modelsummary(models = models,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      vcov = vcov_list,
                      coef_map = cm,
                      exponentiate = T,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm) %>%
  group_tt(i = list("\\textbf{Monitoring Student Behavior}" = 1, "\\textbf{Instructional Development}" = 10)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE)

# Save to LaTeX
writeLines(table, "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Pred_LLM_w_Qual.tex")

gm_presentation <- tibble::tribble(
  ~raw,                ~clean,          ~fmt,
  "nobs",               "\\midrule N",             0,
  "Mean.DV.",           "Outcome Mean",  3,
  "FE: programcohort",  "Program $\\times$ cohort FE", 0,
  "FE: supervisor_id",  "Supervisor FE", 0,
  "FE: uin",  "PST FE", 0,
  "FE: observation_order", "Order FE", 0,
  "FE: cohort_cl", "Cohort FE", 0)

# Use model summary to create a nice table
table <- modelsummary(models = models,
                      estimate="{estimate}{stars}",
                      exponentiate = T,
                      statistic = NULL,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm_presentation) %>%
  group_tt(i = list("\\textbg{Kwok et al. (2024) Coding}" = 1, "\\hspace{1mm} \\textit{Monitoring Student Behavior}" = 2, "\\hspace{1mm} \\textit{Instructional Development}" = 7)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE)

# Save to LaTeX
writeLines(table, "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Pred_LLM_w_Qual_no_se.tex")

#Reliability----
# Feedback
feedback_1 <- read.csv(here("processed data", "2025.03.05 - Feedback Analysis.csv")) 
feedback_2 <- read.csv(here("processed data", "2025.03.07 - Feedback Analysis.csv")) 

feedback_1 <- feedback_1 %>% select(-c(text, area_for_improvement)) %>% pivot_longer(cols = -observationid) %>% rename(value_1=value)
feedback_2 <- feedback_2 %>% select(-c(text, area_for_improvement)) %>% pivot_longer(cols = -observationid) %>% rename(value_2=value)

merged_data_feed <- left_join(feedback_1, feedback_2, by = c("observationid", "name")) %>%
  mutate(agreement = ifelse(value_1==value_2, 1, 0)) %>%
  mutate(task = ifelse(name %in% c("specific_examples", "next_steps", "strengths_mentioned", "areas_for_growth"), "Quality Indicator", "Area for Improvement"))

agreement_feed <- merged_data_feed %>%
  group_by(task, name) %>%
  summarize(agreement_rate = mean(agreement)*100)

# Feedback
reflection_1 <- read.csv(here("processed data", "2025.03.06 - Reflections Analysis.csv")) 
reflection_2 <- read.csv(here("processed data", "2025.03.09 - Reflections Analysis.csv")) 

reflection_1 <- reflection_1 %>% select(-c(text, area_for_improvement)) %>% pivot_longer(cols = -observationid) %>% rename(value_1=value)
reflection_2 <- reflection_2 %>% select(-c(text, area_for_improvement)) %>% pivot_longer(cols = -observationid) %>% rename(value_2=value)

merged_data_ref <- left_join(reflection_1, reflection_2, by = c("observationid", "name")) %>%
  mutate(agreement = ifelse(value_1==value_2, 1, 0)) %>%
  mutate(task = ifelse(name %in% c("specific_examples", "next_steps", "strengths_mentioned", "areas_for_growth"), "Quality Indicator", "Area for Improvement"))

agreement_ref <- merged_data_ref %>%
  group_by(task, name) %>%
  summarize(agreement_rate = mean(agreement)*100) %>%
  select(-task)

table <- bind_cols(agreement_feed, agreement_ref) %>%
  # mutate(name = "Agreement rate (\\%)") %>%
  mutate(across(where(is.numeric), ~round(.x, 1))) %>%
  # relocate(name, task) %>%
  tt() %>%
  save_tt(output="latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE)

writeLines(table, file.path(output_path, "Agreement_Rates.tex"))

#Validity 2.0----
# Take a random sample of 100 observations from feedback and reflections. Code them manually. Compare to ChatGPT codes.

set.seed(939599)
feedback_sample <- read.csv(here("processed data", "2025.03.05 - Feedback Analysis.csv")) %>% 
  sample_n(size = 100) %>%
  select(-area_for_improvement) %>%
  mutate(across(-c(text, observationid), ~ NA))

reflection_sample <- read.csv(here("processed data", "2025.03.06 - Reflections Analysis.csv")) %>% 
  sample_n(size = 100) %>%
  select(-area_for_improvement) %>%
  mutate(across(-c(text, observationid), ~ NA))
  
# Export list
openxlsx::write.xlsx(list("Feedback" = feedback_sample, "Reflection" = reflection_sample), 
                     file = here("validity", "validity_coding.xlsx"), 
                     overwrite = TRUE)

# Upload
manual_coding_feed <- openxlsx::read.xlsx(here("validity", "validity_coding - KV.xlsx"), sheet = "Feedback", startRow = 2) %>%
  rename(observationid=X1) %>%
  # select(-X2) %>%
  pivot_longer(cols = -c(observationid, X2), values_to = "chatgpt_code") 

manual_coding_ref <- openxlsx::read.xlsx(here("validity", "validity_coding - KV.xlsx"), sheet = "Reflection", startRow = 2) %>%
  rename(observationid=X1) %>%
  # select(-X2) %>%
  pivot_longer(cols = -c(observationid, X2), values_to = "chatgpt_code") 

feedback_sample_nonmissing <- read.csv(here("processed data", "2025.03.05 - Feedback Analysis.csv")) %>% 
  select(-c(area_for_improvement, text)) %>%
  pivot_longer(cols = -observationid, values_to = "manual_code") 

reflection_sample_nonmissing <- read.csv(here("processed data", "2025.03.06 - Reflections Analysis.csv")) %>% 
  select(-c(area_for_improvement, text)) %>%
  pivot_longer(cols = -observationid, values_to = "manual_code") 

validity_data_feed <- feedback_sample_nonmissing %>%
  inner_join(manual_coding_feed, by = c("observationid", "name")) %>%
  mutate(task = ifelse(name %in% c("specific_examples", "next_steps", "strengths_mentioned", "areas_for_growth"), "Quality Indicator", "Area for Improvement"),
         task = factor(task, levels = c("Quality Indicator", "Area for Improvement")),
         name = case_when(name == "areas_for_growth" ~ "Area for Improvement",
                          name == "next_steps" ~ "Next Steps",
                          name == "specific_examples" ~ "Specific Examples",
                          name == "strengths_mentioned" ~ "PST Strengths",
                          name == "assessment_feedback_mentioned" ~ "Assessment \\& Feedback",
                          name == "classroom_management_mentioned" ~ "Classroom Management",
                          name == "communication_mentioned" ~ "Communication",
                          name == "differentiation_mentioned" ~ "Differentiation",
                          name == "lesson_planning_mentioned" ~ "Lesson Planning",
                          name == "other_mentioned" ~ "Other",
                          name == "student_comprehension_mentioned" ~ "Student Comprehension",
                          name == "student_engagement_mentioned" ~ "Student Engagement"),
         agree = ifelse(chatgpt_code==manual_code, 1, 0)) 

validity_data_ref <- reflection_sample_nonmissing %>%
  inner_join(manual_coding_ref, by = c("observationid", "name")) %>%
  mutate(task = ifelse(name %in% c("specific_examples", "next_steps", "strengths_mentioned", "areas_for_growth"), "Quality Indicator", "Area for Improvement"),
         task = factor(task, levels = c("Quality Indicator", "Area for Improvement")),
         name = case_when(name == "areas_for_growth" ~ "Area for Improvement",
                          name == "next_steps" ~ "Next Steps",
                          name == "specific_examples" ~ "Specific Examples",
                          name == "strengths_mentioned" ~ "PST Strengths",
                          name == "assessment_feedback_mentioned" ~ "Assessment \\& Feedback",
                          name == "classroom_management_mentioned" ~ "Classroom Management",
                          name == "communication_mentioned" ~ "Communication",
                          name == "differentiation_mentioned" ~ "Differentiation",
                          name == "lesson_planning_mentioned" ~ "Lesson Planning",
                          name == "other_mentioned" ~ "Other",
                          name == "student_comprehension_mentioned" ~ "Student Comprehension",
                          name == "student_engagement_mentioned" ~ "Student Engagement"),
         agree = ifelse(chatgpt_code==manual_code, 1, 0)) 

# Check validity
validity_feed <- validity_data_feed %>%
  group_by(task, name) %>%
  summarize(agreement_rate = round(mean(agree)*100, 1)) %>%
  arrange(task, -agreement_rate)

validity_ref <- validity_data_ref %>%
  group_by(task, name) %>%
  summarize(agreement_rate = round(mean(agree)*100, 1)) %>%
  arrange(task, -agreement_rate)

validity_feed <- left_join(validity_feed, validity_ref, by = c("task", "name"))
  
table <- validity_feed %>%
  tinytable::tt() %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) 

# Save to LaTeX
writeLines(table, file.path(output_path, "Validity_Check_KV.tex"))