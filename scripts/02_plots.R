#ReadMe----
#Purpose: Generating plots for analysis
#Author: Andrew Avitabile

#General----
#Load packages
pacman::p_load(conflicted, here, tidyverse, showtext, scales)

# Remove everything
rm(list=ls())

# Conflict prefer
conflict_prefer(name = "filter", winner = "dplyr")

#Add fonts
font_add(family = "LMRoman", regular = here("lmroman10-regular.otf"))
showtext_auto()

# Set output filepath - YOU'LL NEED TO UPDATE THIS 
output_path <- "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables"

#Load data
analysis_data <- readRDS(here("processed data", "analysis_data.RDS"))

#Plot length----
# Feedback sentence count histogram
ggplot(analysis_data, aes(x=n_sentences_feed)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth=1, fill="#FF8C42") +
  theme_minimal() +
  xlab("Sentences") +
  ylab("Proportion") +
  scale_x_continuous(expand = c(0,0), limits = c(0,55)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text=element_text(family="LMRoman", color = "black", size = 24), plot.caption = element_text(hjust = 0), axis.text = element_text(color = "black", size = 24)) +
  geom_vline(xintercept = median(analysis_data$n_sentences_feed), linetype = "dashed", color = "black")
ggsave(file.path(output_path, "Sentences Per Feedback Hist.pdf"), width = 6, height = 4)

# Reflection sentence count histogram
ggplot(analysis_data, aes(x=n_sentences_ref)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth=1, fill="#1F4E79") +
  theme_minimal() +
  xlab("Sentences") +
  ylab("Proportion") +
  scale_x_continuous(expand = c(0,0), limits = c(0,55)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text=element_text(family="LMRoman", color = "black", size = 24), plot.caption = element_text(hjust = 0), axis.text = element_text(color = "black", size = 24)) +
  geom_vline(xintercept = median(analysis_data$n_sentences_ref, na.rm=T), linetype = "dashed", color = "black")
ggsave(file.path(output_path, "Sentences Per Reflection Hist.pdf"), width = 6, height = 4)

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
ggplot(plot_data_combined, aes(x = reorder(Category, Mean), y = Mean, fill = Type)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(x = NULL, y = "% of Feedback or PSTs", fill = NULL) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05), labels = scales::percent_format()) +
  scale_fill_manual(values = c("All Feedback" = "#FF8C42", "PSTs" = "#1F4E79")) +
  theme(
    text = element_text(family = "LMRoman", color = "black", size = 16),
    plot.caption = element_text(hjust = 0),
    axis.text = element_text(color = "black", size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  scale_x_discrete(
    limits = c("Actionable Steps", "Area for Improvement", "Specific Examples", "PST Strengths")
  )
rm(plot_data_combined, overall_means, pst_means)
ggsave(file.path(output_path, "Feedback Bar Chart.pdf"), width = 6, height = 4)

#Plot reflection descriptions----
plot_data <- analysis_data %>%
  mutate(feedback_score = rowMeans(across(c("specific_examples_ref", "next_steps_ref", "strengths_mentioned_ref", "areas_for_growth_ref")), na.rm = TRUE))

# Calculate overall means
overall_means <- plot_data %>%
  summarize(across(c("specific_examples_ref", "next_steps_ref", "strengths_mentioned_ref", "areas_for_growth_ref"),
                   ~mean(.x, na.rm = TRUE), .names = "overall_{.col}"))

# Calculate PST-level means (PSTs with at least one occurrence)
pst_means <- plot_data %>%
  group_by(pst_id) %>%
  summarize(across(c("specific_examples_ref", "next_steps_ref", "strengths_mentioned_ref", "areas_for_growth_ref"),
                   ~as.numeric(any(.x == 1, na.rm = TRUE)))) %>%
  summarize(across(-pst_id, ~mean(.x, na.rm = TRUE), .names = "pst_{.col}"))

# Combine and reshape data for plotting
plot_data_combined <- bind_rows(
  pivot_longer(overall_means, 
               cols = everything(), 
               names_to = "Category", 
               values_to = "Mean") %>%
    mutate(Type = "All Reflections"),
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
  mutate(Category = case_when(Category=="Strengths Mentioned Ref" ~ "PST Strengths",
                              Category=="Specific Examples Ref" ~ "Specific Examples",
                              Category=="Areas For Growth Ref" ~ "Area for Improvement",
                              Category=="Next Steps Ref" ~ "Actionable Steps"),
         Category = factor(Category, levels = c("Actionable Steps", "Area for Improvement", "Specific Examples", "PST Strengths")))

# Create bar plot
ggplot(plot_data_combined, aes(x = reorder(Category, Mean), y = Mean, fill = Type)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(x = NULL, y = "% of Reflections or PSTs", fill = NULL) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05), labels = scales::percent_format()) +
  scale_fill_manual(values = c("All Reflections" = "#FF8C42", "PSTs" = "#1F4E79")) +
  theme(
    text = element_text(family = "LMRoman", color = "black", size = 16),
    plot.caption = element_text(hjust = 0),
    axis.text = element_text(color = "black", size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  scale_x_discrete(
    limits = c("Actionable Steps", "Area for Improvement", "Specific Examples", "PST Strengths")
  )
rm(plot_data_combined, overall_means, pst_means)
ggsave(file.path(output_path, "Reflections Bar Chart.pdf"), width = 6, height = 4)

#Plot area for improvement----
plot_data <- analysis_data %>%
  mutate(feedback_score = rowMeans(across(c("no_afi_mentioned_feed", "classroom_management_mentioned_feed", "lesson_planning_mentioned_feed", "differentiation_mentioned_feed", "assessment_feedback_mentioned_feed", "student_engagement_mentioned_feed", "student_comprehension_mentioned_feed", "communication_mentioned_feed", "other_mentioned_feed")), na.rm = TRUE)) %>%
  mutate(feedback_score = rowMeans(across(c("no_afi_mentioned_ref", "classroom_management_mentioned_ref", "lesson_planning_mentioned_ref", "differentiation_mentioned_ref", "assessment_feedback_mentioned_ref", "student_engagement_mentioned_ref", "student_comprehension_mentioned_ref", "communication_mentioned_ref", "other_mentioned_ref")), na.rm = TRUE))

# Calculate overall means
overall_means <- plot_data %>%
  summarize(across(c("no_afi_mentioned_feed", "classroom_management_mentioned_feed", "lesson_planning_mentioned_feed", "differentiation_mentioned_feed", "assessment_feedback_mentioned_feed", "student_engagement_mentioned_feed", "student_comprehension_mentioned_feed", "communication_mentioned_feed", "other_mentioned_feed",
                     "no_afi_mentioned_ref", "classroom_management_mentioned_ref", "lesson_planning_mentioned_ref", "differentiation_mentioned_ref", "assessment_feedback_mentioned_ref", "student_engagement_mentioned_ref", "student_comprehension_mentioned_ref", "communication_mentioned_ref", "other_mentioned_ref"),
                   ~mean(.x, na.rm = TRUE), .names = "overall_{.col}"))

# Calculate PST-level means (PSTs with at least one occurrence)
pst_means <- plot_data %>%
  group_by(pst_id) %>%
  summarize(across(c("no_afi_mentioned_feed", "classroom_management_mentioned_feed", "lesson_planning_mentioned_feed", "differentiation_mentioned_feed", "assessment_feedback_mentioned_feed", "student_engagement_mentioned_feed", "student_comprehension_mentioned_feed", "communication_mentioned_feed", "other_mentioned_feed",
                     "no_afi_mentioned_ref", "classroom_management_mentioned_ref", "lesson_planning_mentioned_ref", "differentiation_mentioned_ref", "assessment_feedback_mentioned_ref", "student_engagement_mentioned_ref", "student_comprehension_mentioned_ref", "communication_mentioned_ref", "other_mentioned_ref"),
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
  mutate(reflection = grepl("_ref", Category)) %>%
  mutate(
    Category = str_remove(Category, "^(overall_|pst_)"),
    Category = str_replace_all(Category, "_", " ") %>% str_to_title()
  ) %>%
  mutate(Category = gsub(" Mentioned Feed", "", Category),
         Category = gsub(" Mentioned Ref", "", Category),
         Category = ifelse(Category=="No Afi", "No Area for Improvement", Category),
         Category = ifelse(Category=="Assessment Feedback", "Assessment and Feedback", Category),
         Category = factor(Category, levels = c("Other", "Differentiation", "Student Comprehension", "Assessment and Feedback", "Student Engagement", "Communication", "Lesson Planning", "Classroom Management", "No Area for Improvement")))

# Create bar plot - feedback
plot_data_combined %>%
  filter(reflection == F) %>%
  ggplot(aes(x = reorder(Category, Mean), y = Mean, fill = Type)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(x = NULL, y = "% of Feedback or PSTs", fill = NULL) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05), labels = scales::percent_format()) +
  scale_fill_manual(values = c("Feedback Level" = "#FF8C42", "PST Level" = "#1F4E79")) +
  theme(
    text = element_text(family = "LMRoman", color = "black", size = 16),
    plot.caption = element_text(hjust = 0),
    axis.text = element_text(color = "black", size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
ggsave(file.path(output_path, "Bar Chart Area for Improvement - Feedback.pdf"), width = 7, height = 4)

# Create bar plot - reflections
plot_data_combined %>%
  filter(reflection == T) %>%
  ggplot(aes(x = reorder(Category, Mean), y = Mean, fill = Type)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(x = NULL, y = "% of Reflections or PSTs", fill = NULL) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05), labels = scales::percent_format()) +
  scale_fill_manual(values = c("Feedback Level" = "#FF8C42", "PST Level" = "#1F4E79")) +
  theme(
    text = element_text(family = "LMRoman", color = "black", size = 16),
    plot.caption = element_text(hjust = 0),
    axis.text = element_text(color = "black", size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
rm(plot_data_combined, overall_means, pst_means)
ggsave(file.path(output_path, "Bar Chart Area for Improvement - Reflections.pdf"), width = 7, height = 4)

# What do PSTs mention when supervisors suggest other, nothing, or multiple areas for improvement?----
# Calculate share_same
share_same <- analysis_data %>%
  group_by(area_for_improvement_feed, area_for_improvement_ref) %>%
  summarize(n = n(), .groups = "drop") %>%
  mutate(same = ifelse(area_for_improvement_feed == area_for_improvement_ref, 1, 0)) %>%
  group_by(area_for_improvement_feed, same) %>%
  summarize(n = sum(n), .groups = "drop") %>%
  group_by(area_for_improvement_feed) %>%
  mutate(N = sum(n), share = n / N) %>%
  filter(same == 1) %>%
  select(area_for_improvement_ref=area_for_improvement_feed, n, N, share) %>%
  mutate(group = "Same Area")

# Calculate share_none
share_none <- analysis_data %>%
  group_by(area_for_improvement_feed, area_for_improvement_ref) %>%
  summarize(n = n(), .groups = "drop") %>%
  filter(area_for_improvement_feed == "None") %>%
  mutate(N = sum(n), share = n / N) %>%
  select(area_for_improvement_ref, n, N, share) %>%
  mutate(group = "No Area")

# Define a function to calculate the share for each area_for_improvement_ref (Other)
calculate_shares <- function(ref_area) {
  analysis_data %>%
    filter(area_for_improvement_feed != "None", area_for_improvement_feed != ref_area) %>%
    group_by(area_for_improvement_ref) %>%
    summarize(n = n(), .groups = "drop") %>%
    mutate(N = sum(n), share = n / N) %>%
    filter(area_for_improvement_ref == ref_area) %>%
    select(area_for_improvement_ref, n, N, share) %>%
    mutate(group = "Other Area")
}

# Get all unique values of area_for_improvement_ref
unique_areas <- analysis_data %>%
  filter(area_for_improvement_feed != "None") %>%
  pull(area_for_improvement_ref) %>%
  unique()

# Calculate share_other for each unique area_for_improvement_ref
share_other <- map_dfr(unique_areas, calculate_shares)

# Combine all results into one dataset
final_result <- bind_rows(share_same, share_none, share_other) %>%
  mutate(group=factor(group, levels = c("Same Area", "No Area", "Other Area"))) %>%
  mutate(area_for_improvement_ref = factor(area_for_improvement_ref, 
                                           levels = c("Classroom Management", "Lesson Planning", "Student Engagement", "Communication", "Assessment and Feedback", "Student Comprehension", "Differentiation", "Other", "Multiple Areas", "None"))) %>%
  mutate(
    se = sqrt((share * (1 - share)) / N),
    moe = 1.96 * se, # 95% confidence interval
    ci_lower = pmax(0, share - moe), # Ensuring lower bound is not negative
    ci_upper = pmin(1, share + moe) # Ensuring upper bound is not above 1
  )

final_result %>%
  ggplot(aes(
    x = area_for_improvement_ref, y = share,
    fill = group
  )) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) +
  geom_errorbar(aes(
    ymin = ci_lower, ymax = ci_upper,
    group = group
  ), position = position_dodge(width = 0.9), width = 0.25) +
  coord_flip() +
  theme_minimal() +
  labs(
    y = "Percent of Reflections",
    x = "Area for Improvement in PST Reflection"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
    limits = c(0.0, 0.63),
    labels = percent_format()
  ) +
  theme(
    text = element_text(family = "LMRoman", color = "black", size = 24),
    plot.caption = element_text(hjust = 0),
    axis.text = element_text(color = "black", size = 24),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold", size = 24)
  ) +
  scale_fill_manual(values = c(
    "Other Area" = "#1F4E79",
    "No Area" = "grey",
    "Same Area" = "#FF8C42"
  ))
ggsave(file.path(output_path, "AFI_Ref_when_no_AFI_Feedback.pdf"), width = 11, height = 7.5)

#Share of times supervisors provide no feedback----
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
  geom_bar(stat = "identity", position = "dodge", fill = "#FF8C42", width = 0.5) +
  theme_minimal() +
  labs(
    y = "% of Feedback with No Area for Improvement",
    x = "Supervisor"
  ) +
  scale_y_continuous(
    expand = c(0, 0.01),
    breaks = seq(0.0, 1, by = 0.25),
    limits = c(0.0, 1),
    labels = scales::percent_format()
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    text = element_text(family = "LMRoman", color = "black", size = 24),
    plot.caption = element_text(hjust = 0),
    axis.text = element_text(color = "black", size = 24),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold", size = 24),
    axis.text.x = element_blank()
  )
ggsave(file.path(output_path, "Supervisor pct no area for improvement.pdf"), width = 11, height = 7.5)

#How do outcomes look for the most and least strict supervisors----
analysis_data %>%
  group_by(supervisor_id, sup_blup_std) %>%
  summarize(
    no_area_for_improvement = sum(no_afi_mentioned_feed),
    n_obs_sup = n(),
    prop_no_area_for_improvement = no_area_for_improvement / n_obs_sup,
    .groups = "drop"  # This removes the grouping after summarizing
  ) %>%
  ungroup() %>%
  ggplot(aes(x = sup_blup_std, y = prop_no_area_for_improvement)) +  # Removed size from here
  geom_point(aes(size = n_obs_sup)) +  # Added size here
  scale_size(range = c(0, 12)) +
  geom_smooth(
    method = "lm", 
    color = "#FF8C42", 
    se = FALSE, 
    linewidth = 1, 
    aes(group = 1)
  ) +
  theme_minimal() +
  labs(
    x = "Supervisor BLUP (Standardized)",
    y = "% of Obs. with No Area for Improvement",
    size = "Number of Observations"
  ) +
  scale_y_continuous(
    expand = c(0, 0), 
    breaks = c(0.0, 0.25, 0.5, 0.75, 1), 
    limits = c(-0.02, 1.02),
    labels = scales::percent_format()
  ) +
  theme(
    text = element_text(family = "LMRoman", color = "black", size = 24),
    plot.caption = element_text(hjust = 0),
    axis.text = element_text(color = "black", size = 24),
    legend.position = "bottom",
    legend.title = element_text(size = 24),
    strip.text = element_text(face = "bold", size = 24)
  )
ggsave(file.path(output_path, "Supervisor BLUPS vs AFI.pdf"), width = 11, height = 7.5)