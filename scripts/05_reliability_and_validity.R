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
model1 <- feglm(classroom_management_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data, vcov = "hetero", family = binomial(link = "logit"))
model2 <- feglm(lesson_planning_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data, vcov = "hetero", family = binomial(link = "logit"))
model3 <- feglm(student_engagement_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data, vcov = "hetero", family = binomial(link = "logit"))
model4 <- feglm(communication_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data, vcov = "hetero", family = binomial(link = "logit"))
model5 <- feglm(assessment_feedback_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data, vcov = "hetero", family = binomial(link = "logit"))
model6 <- feglm(student_comprehension_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data, vcov = "hetero", family = binomial(link = "logit"))
model7 <- feglm(differentiation_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data, vcov = "hetero", family = binomial(link = "logit"))
model8 <- feglm(other_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data, vcov = "hetero", family = binomial(link = "logit"))
model9 <- feglm(no_afi_mentioned_feed ~ nocriticism + lessoncycle + lessonconnections + studentcomprehension + lessondelivery + praise + transitions + attention + nonverbaltechniques + corrections, data = matched_data, vcov = "hetero", family = binomial(link = "logit"))

models <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9)
etable(models)

# Renaming coefficients
cm <- c(
  "nocriticism" = "\\hspace{1mm} No criticism",
  "praise" = "\\hspace{3mm} Praise",
  "transitions" = "\\hspace{3mm} Transitions",
  "attention" = "\\hspace{3mm} Attention",
  "nonverbaltechniques" = "\\hspace{3mm} Non-verbal Techniques",
  "corrections" = "\\hspace{3mm} Corrections",
  "lessoncycle" = "\\hspace{3mm} Lesson Cycle",
  "lessonconnections" = "\\hspace{3mm} Lesson Connections",
  "studentcomprehension" = "\\hspace{3mm} Student Comprehension",
  "lessondelivery" = "\\hspace{3mm} Lesson Delivery"
)

# Use model summary to create a nice table
table <- modelsummary(models = models,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      exponentiate = T,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm) %>%
  group_tt(i = list("\\textbf{Monitoring Student Behavior}" = 3, "\\textbf{Instructional Development}" = 13)) %>%
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

merged_data <- left_join(feedback_1, feedback_2, by = c("observationid", "name")) %>%
  mutate(agreement = ifelse(value_1==value_2, 1, 0))

merged_data %>%
  summarize(agreement_rate = mean(agreement)*100)

# Feedback
reflection_1 <- read.csv(here("processed data", "2025.03.06 - Reflections Analysis.csv")) 
reflection_2 <- read.csv(here("processed data", "2025.03.09 - Reflections Analysis.csv")) 

reflection_1 <- reflection_1 %>% select(-c(text, area_for_improvement)) %>% pivot_longer(cols = -observationid) %>% rename(value_1=value)
reflection_2 <- reflection_2 %>% select(-c(text, area_for_improvement)) %>% pivot_longer(cols = -observationid) %>% rename(value_2=value)

merged_data <- left_join(reflection_1, reflection_2, by = c("observationid", "name")) %>%
  mutate(agreement = ifelse(value_1==value_2, 1, 0))

merged_data %>%
  summarize(agreement_rate = mean(agreement)*100)

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

# Check validity