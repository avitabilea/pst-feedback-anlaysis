#ReadMe----
#Purpose: Generating tables for analysis
#Author: Andrew Avitabile

#General----
#Load packages
pacman::p_load(conflicted, here, tidyverse, sandwich, lmtest, modelsummary, tinytable, car, fixest, showtext)

# Remove everything
rm(list=ls())

# Set output filepath - YOU'LL NEED TO UPDATE THIS 
output_path <- "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables"

#Add fonts
font_add(family = "LMRoman", regular = here("lmroman10-regular.otf"))
showtext_auto()

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
gm_presentation <- tibble::tribble(
  ~raw,                ~clean,          ~fmt,
  "nobs",               "\\midrule N",             0,
  "Mean.DV.",           "Outcome Mean",  3,
  "FE: pst_id",  "PST FE", 0)

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
outcome_means <- tibble::tribble(
  ~term,           ~model1,                                             ~model2,                                           ~model3,                                          ~model4,
  "Outcome Mean",  mean(analysis_data$strengths_mentioned_feed, na.rm = TRUE), mean(analysis_data$specific_examples_feed, na.rm = TRUE), mean(analysis_data$areas_for_growth_feed, na.rm = TRUE), mean(analysis_data$next_steps_feed, na.rm = TRUE)
)

# Observation order
model1 <- fit_clustered_model(strengths_mentioned_feed ~ factor(observation_order), analysis_data, "pst_id")
model2 <- fit_clustered_model(specific_examples_feed ~ factor(observation_order), analysis_data, "pst_id")
model3 <- fit_clustered_model(areas_for_growth_feed ~ factor(observation_order), analysis_data, "pst_id")
model4 <- fit_clustered_model(next_steps_feed ~ factor(observation_order), analysis_data, "pst_id")

# Extract the model objects for modelsummary
models <- list(model1$model, model2$model, model3$model, model4$model)

# Create a list of vcov matrices for modelsummary
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov)

cm <- c("factor(observation_order)2" = "\\hspace{3mm} 2",
        "factor(observation_order)3" = "\\hspace{3mm} 3",
        "factor(observation_order)4" = "\\hspace{3mm} 4")
table <- modelsummary(models = models, 
                      estimate = "{estimate} ({std.error}){stars}", 
                      statistic = NULL,
                      vcov = vcov_list,
                      exponentiate = TRUE,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.') %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, file.path(output_path, "Pred_feedback_1_obs_order_AEFP.tex"))

# Cert area
model1 <- fit_clustered_model(strengths_mentioned_feed ~ factor(certification), analysis_data, "pst_id")
model2 <- fit_clustered_model(specific_examples_feed ~ factor(certification), analysis_data, "pst_id")
model3 <- fit_clustered_model(areas_for_growth_feed ~ factor(certification), analysis_data, "pst_id")
model4 <- fit_clustered_model(next_steps_feed ~ factor(certification), analysis_data, "pst_id")

# Extract models and vcov
models <- list(model1$model, model2$model, model3$model, model4$model)
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov)

cm <- c("factor(certification)4-8 English/SS" = "\\hspace{3mm} 4-8 English/SS",
        "factor(certification)4-8 Math/Sci" = "\\hspace{3mm} 4-8 Math/Sci",
        "factor(certification)HS Subjects" = "\\hspace{3mm} HS Subjects")
table <- modelsummary(models = models,
                      estimate = "{estimate} ({std.error}){stars}", 
                      statistic = NULL,
                      vcov = vcov_list,
                      exponentiate = TRUE,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.') %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, file.path(output_path, "Pred_feedback_2_cert_area_AEFP.tex"))

# BLUPs
model1 <- fit_clustered_model(strengths_mentioned_feed ~ sup_blup_std + pst_blup_std + sch_blup_std, analysis_data, "pst_id")
model2 <- fit_clustered_model(specific_examples_feed ~ sup_blup_std + pst_blup_std + sch_blup_std, analysis_data, "pst_id")
model3 <- fit_clustered_model(areas_for_growth_feed ~ sup_blup_std + pst_blup_std + sch_blup_std, analysis_data, "pst_id")
model4 <- fit_clustered_model(next_steps_feed ~ sup_blup_std + pst_blup_std + sch_blup_std, analysis_data, "pst_id")

# Extract models and vcov
models <- list(model1$model, model2$model, model3$model, model4$model)
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov)

cm <- c("pst_blup_std" = "\\hspace{3mm} PST",
        "sup_blup_std" = "\\hspace{3mm} Supervisor",
        "sch_blup_std" = "\\hspace{3mm} Placement School")
table <- modelsummary(models = models,
                      estimate = "{estimate} ({std.error}){stars}", 
                      statistic = NULL,
                      vcov = vcov_list,
                      exponentiate = TRUE,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.') %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, file.path(output_path, "Pred_feedback_3_blups_AEFP.tex"))

# Student teaching school
model1 <- fit_clustered_model(strengths_mentioned_feed ~ st_advantage_index + suspensions_instances + st_local, analysis_data, "pst_id")
model2 <- fit_clustered_model(specific_examples_feed ~ st_advantage_index + suspensions_instances + st_local, analysis_data, "pst_id")
model3 <- fit_clustered_model(areas_for_growth_feed ~ st_advantage_index + suspensions_instances + st_local, analysis_data, "pst_id")
model4 <- fit_clustered_model(next_steps_feed ~ st_advantage_index + suspensions_instances + st_local, analysis_data, "pst_id")

# Extract models and vcov
models <- list(model1$model, model2$model, model3$model, model4$model)
vcov_list <- list(model1$vcov, model2$vcov, model3$vcov, model4$vcov)

cm <- c("st_advantage_index" = "\\hspace{3mm} Standardized Advantage Index",
        "suspensions_instances" = "\\hspace{3mm} Standardized Suspensions",
        "st_local" = "\\hspace{3mm} Local District")
table <- modelsummary(models = models,
                      estimate = "{estimate} ({std.error}){stars}", 
                      statistic = NULL,
                      vcov = vcov_list,
                      exponentiate = TRUE,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = '.') %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE)
table <- substr(table,1,nchar(table)-6)
writeLines(table, file.path(output_path, "Pred_feedback_4_placement_school_AEFP.tex"))


#Predicting program and labor market outcomes----
# Program outcomes
model1 <- feols(avg_eval_score_std ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(avg_eval_score_std ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | pst_id + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(examscore_std_ppr ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(examscore_std_req ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)

# Create combined regression table
models <- list(model1,model2,model3,model4)
etable(models)

cm <- c(
  "no_afi_mentioned_feed" = "\\hspace{3mm} No Area",
  "classroom_management_mentioned_feed" = "\\hspace{3mm} Classroom Management",
  "student_engagement_mentioned_feed" = "\\hspace{3mm} Student Engagement",
  "lesson_planning_mentioned_feed" = "\\hspace{3mm} Lesson Planning",
  "communication_mentioned_feed" = "\\hspace{3mm} Communication",
  "assessment_feedback_mentioned_feed" = "\\hspace{3mm} Assessment and Feedback",
  "area_for_improvement_feedStudent Comprehension" = "\\hspace{3mm} Student Comprehension",
  "differentiation_mentioned_feed" = "\\hspace{3mm} Differentiation",
  "other_mentioned_feed" = "\\hspace{3mm} Other Area"
)

table <- modelsummary(models = models,
                      estimate = "{estimate} ({std.error}){stars}", 
                      statistic = NULL,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm_presentation) %>%
  group_tt(i = list("\\textbf{Area for improvement}" = 1)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()

# Save to LaTeX
writeLines(table, file.path(output_path, "Pred_Program_Outcomes_AEFP.tex"))

# Labor market outcomes
model1 <- feols(ever_enter_teaching ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model2 <- feols(enter_teaching_imm ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model3 <- feols(same_school ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + faminc + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model4 <- feols(advantage_index ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)
model5 <- feols(leave_teaching_years_1_to_4 ~ no_afi_mentioned_feed + classroom_management_mentioned_feed + lesson_planning_mentioned_feed + differentiation_mentioned_feed + assessment_feedback_mentioned_feed + student_engagement_mentioned_feed + student_comprehension_mentioned_feed + communication_mentioned_feed + other_mentioned_feed | sat_score_cat + gpa_z_cat + race + sex + mother_colldeg + father_colldeg + supervisor_id + programcohort + observation_order, data = analysis_data, cluster = "pst_id", weights = ~inv_n_obs)

# Create combined regression table
models <- list(model1,model3,model5)
etable(models)

table <- modelsummary(models = models,
                      estimate = "{estimate} ({std.error}){stars}", 
                      statistic = NULL,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm_presentation) %>%
  group_tt(i = list("\\textbf{Area for improvement}" = 1)) %>%
  save_tt("latex")

table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()

# Save to LaTeX
writeLines(table, file.path(output_path, "Pred_Labor_Market_Outcomes_AEFP.tex"))

#Plot feedback areas for improvement----
plot_data <- analysis_data %>%
  mutate(feedback_score = rowMeans(across(c("no_afi_mentioned_feed", "classroom_management_mentioned_feed", "lesson_planning_mentioned_feed", "differentiation_mentioned_feed", "assessment_feedback_mentioned_feed", "student_engagement_mentioned_feed", "student_comprehension_mentioned_feed", "communication_mentioned_feed", "other_mentioned_feed")), na.rm = TRUE))

# Calculate overall means
overall_means <- plot_data %>%
  summarize(across(c("no_afi_mentioned_feed", "classroom_management_mentioned_feed", "lesson_planning_mentioned_feed", "differentiation_mentioned_feed", "assessment_feedback_mentioned_feed", "student_engagement_mentioned_feed", "student_comprehension_mentioned_feed", "communication_mentioned_feed", "other_mentioned_feed"),
                   ~mean(.x, na.rm = TRUE), .names = "overall_{.col}"))

# Calculate PST-level means (PSTs with at least one occurrence)
pst_means <- plot_data %>%
  group_by(pst_id) %>%
  summarize(across(c("no_afi_mentioned_feed", "classroom_management_mentioned_feed", "lesson_planning_mentioned_feed", "differentiation_mentioned_feed", "assessment_feedback_mentioned_feed", "student_engagement_mentioned_feed", "student_comprehension_mentioned_feed", "communication_mentioned_feed", "other_mentioned_feed"),
                   ~as.numeric(any(.x == 1, na.rm = TRUE)))) %>%
  summarize(across(-pst_id, ~mean(.x, na.rm = TRUE), .names = "pst_{.col}"))

# Combine and reshape data for plotting
plot_data_combined <- bind_rows(
  pivot_longer(overall_means, 
               cols = everything(), 
               names_to = "Category", 
               values_to = "Mean") %>%
    mutate(Type = "Feedback Level"),
  pivot_longer(pst_means, 
               cols = everything(), 
               names_to = "Category", 
               values_to = "Mean") %>%
    mutate(Type = "PST Level")
) %>%
  mutate(
    Category = str_remove(Category, "^(overall_|pst_)"),
    Category = str_replace_all(Category, "_", " ") %>% str_to_title()
  ) %>%
  mutate(Category = gsub(" Mentioned Feed", "", Category),
         Category = ifelse(Category=="No Afi", "No Area for Improvement", Category),
         Category = ifelse(Category=="Assessment Feedback", "Assessment and Feedback", Category),
         Category = factor(Category, levels = c("Other", "Differentiation", "Student Comprehension", "Assessment and Feedback", "Student Engagement", "Communication", "Lesson Planning", "Classroom Management", "No Area for Improvement")))

# Create bar plot
plot_data_combined %>%
  filter(Type == "Feedback Level") %>%
  ggplot(aes(x = reorder(Category, Mean), y = Mean, fill = Type)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(x = NULL, y = "% of Feedback", fill = NULL) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05), labels = scales::percent_format()) +
  scale_fill_manual(values = c("Feedback Level" = "#FF8C42", "PST Level" = "#FF8C42")) +
  theme(
    text = element_text(family = "LMRoman", color = "black", size = 16),
    plot.caption = element_text(hjust = 0),
    axis.text = element_text(color = "black", size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
rm(plot_data_combined, overall_means, pst_means)
ggsave(file.path(output_path, "AFI Bar Chart - AEFP.pdf"), width = 6, height = 4)

#Plot feedback descriptions----
plot_data <- analysis_data %>%
  mutate(feedback_score = rowMeans(across(c("specific_examples_feed", "next_steps_feed", "strengths_mentioned_feed", "areas_for_growth_feed")), na.rm = TRUE))

# Calculate overall means
overall_means <- plot_data %>%
  summarize(across(c("specific_examples_feed", "next_steps_feed", "strengths_mentioned_feed", "areas_for_growth_feed"),
                   ~mean(.x, na.rm = TRUE), .names = "overall_{.col}"))

# Calculate PST-level means (PSTs with at least one occurrence)
pst_means <- plot_data %>%
  group_by(pst_id) %>%
  summarize(across(c("specific_examples_feed", "next_steps_feed", "strengths_mentioned_feed", "areas_for_growth_feed"),
                   ~as.numeric(any(.x == 1, na.rm = TRUE)))) %>%
  summarize(across(-pst_id, ~mean(.x, na.rm = TRUE), .names = "pst_{.col}"))

# Combine and reshape data for plotting
plot_data_combined <- bind_rows(
  pivot_longer(overall_means, 
               cols = everything(), 
               names_to = "Category", 
               values_to = "Mean") %>%
    mutate(Type = "All Feedback"),
  pivot_longer(pst_means, 
               cols = everything(), 
               names_to = "Category", 
               values_to = "Mean") %>%
    mutate(Type = "PSTs")
) %>%
  mutate(
    Category = str_remove(Category, "^(overall_|pst_)"),
    Category = str_replace_all(Category, "_", " ") %>% str_to_title()
  ) %>%
  mutate(Category = case_when(Category=="Strengths Mentioned Feed" ~ "PST Strengths",
                              Category=="Specific Examples Feed" ~ "Specific Examples",
                              Category=="Areas For Growth Feed" ~ "Area for Improvement",
                              Category=="Next Steps Feed" ~ "Actionable Steps"),
         Category = factor(Category, levels = c("Actionable Steps", "Area for Improvement", "Specific Examples", "PST Strengths")))

# Create bar plot
plot_data_combined %>%
  filter(Type == "All Feedback") %>%
  ggplot(aes(x = reorder(Category, Mean), y = Mean, fill = Type)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(x = NULL, y = "% of Feedback", fill = NULL) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05), labels = scales::percent_format()) +
  scale_fill_manual(values = c("All Feedback" = "#FF8C42", "PSTs" = "#FF8C42")) +
  theme(
    text = element_text(family = "LMRoman", color = "black", size = 16),
    plot.caption = element_text(hjust = 0),
    axis.text = element_text(color = "black", size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  scale_x_discrete(
    limits = c("Actionable Steps", "Area for Improvement", "Specific Examples", "PST Strengths")
  )
rm(plot_data_combined, overall_means, pst_means)
ggsave(file.path(output_path, "Feedback Bar Chart - AEFP.pdf"), width = 6, height = 4)

#Share of times supervisors provide feedback with no AFI----
analysis_data %>%
  group_by(supervisor_id) %>%
  summarize(
    no_area_for_improvement = sum(no_afi_mentioned_feed),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    sup_share_no_area = no_area_for_improvement / n,
    supervisor_id = factor(supervisor_id, levels = supervisor_id[order(-sup_share_no_area)])
  ) %>%
  ggplot(aes(x = supervisor_id, y = sup_share_no_area)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#FF8C42", width = 0.3) +
  theme_minimal() +
  labs(
    y = "% of Supervisors' Feedback with No Area for Improvement",
    x = "Supervisor"
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0.04),
    breaks = seq(0.0, 1, by = 0.25),
    limits = c(0.0, 1),
    labels = scales::percent_format()
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    text = element_text(family = "LMRoman", color = "black", size = 24),
    plot.caption = element_text(hjust = 0),
    axis.text = element_text(color = "black", size = 24),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold", size = 24),
    axis.text.y = element_blank()
  )
ggsave(file.path(output_path, "Supervisor pct no area for improvement.pdf"), width = 10, height = 5)