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

#Observation-level descriptions----
plot_data <- analysis_data %>%
  mutate(feedback_score = rowMeans(across(c("specific_examples_feed", "next_steps_feed", "strengths_mentioned_feed", "areas_for_growth_feed",
                                            "specific_examples_ref", "next_steps_ref", "strengths_mentioned_ref", "areas_for_growth_ref")), na.rm = TRUE))

# Calculate overall means
overall_means <- plot_data %>%
  summarize(across(c("specific_examples_feed", "next_steps_feed", "strengths_mentioned_feed", "areas_for_growth_feed",
                     "specific_examples_ref", "next_steps_ref", "strengths_mentioned_ref", "areas_for_growth_ref"),
                   ~mean(.x, na.rm = TRUE), .names = "overall_{.col}"))

# Calculate PST-level means (PSTs with at least one occurrence)
pst_means <- plot_data %>%
  group_by(pst_id) %>%
  summarize(across(c("specific_examples_feed", "next_steps_feed", "strengths_mentioned_feed", "areas_for_growth_feed",
                     "specific_examples_ref", "next_steps_ref", "strengths_mentioned_ref", "areas_for_growth_ref"),
                   ~as.numeric(any(.x == 1, na.rm = TRUE)))) %>%
  summarize(across(-pst_id, ~mean(.x, na.rm = TRUE), .names = "pst_{.col}"))

# Combine and reshape data for plotting
plot_data_combined <- bind_rows(
  pivot_longer(overall_means, 
               cols = everything(), 
               names_to = "Category", 
               values_to = "Mean") %>%
    mutate(Type = "Observation-Level"),
  pivot_longer(pst_means, 
               cols = everything(), 
               names_to = "Category", 
               values_to = "Mean") %>%
    mutate(Type = "PST-Level")
) %>%
  mutate(
    Category = str_remove(Category, "^(overall_|pst_)"),
    Category = str_replace_all(Category, "_", " ") %>% str_to_title()
  ) %>%
  mutate(document = ifelse(grepl("Feed", Category), "Supervisor Feedback", "PST Reflections"),
         Category = str_remove(str_remove(Category, " Feed"), " Ref"),
         Category = case_when(Category=="Strengths Mentioned" ~ "PST Strengths",
                              Category=="Specific Examples" ~ "Specific Examples",
                              Category=="Areas For Growth" ~ "Area for Improvement",
                              Category=="Next Steps" ~ "Actionable Steps"),
         Category = factor(Category, levels = c("Actionable Steps", "Area for Improvement", "Specific Examples", "PST Strengths")))

# Create bar plot
plot_data_combined %>%
  filter(Type == "Observation-Level") %>%
  ggplot(aes(x = reorder(Category, Mean), y = Mean, fill = document)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(x = NULL, y = "% of Observations", fill = NULL) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05), labels = scales::percent_format()) +
  scale_fill_manual(values = c("PST Reflections" = "#1F4E79", "Supervisor Feedback" = "#FF8C42"), guide = guide_legend(reverse = TRUE)) +
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
ggsave(file.path(output_path, "Observation-Level Bar Chart.pdf"), width = 6, height = 4)

#PST-level descriptions----
# Create bar plot
plot_data_combined %>%
  filter(Type == "PST-Level") %>%
  ggplot(aes(x = reorder(Category, Mean), y = Mean, fill = document)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(x = NULL, y = "% of PSTs", fill = NULL) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.05), labels = scales::percent_format()) +
  scale_fill_manual(values = c("PST Reflections" = "#1F4E79", "Supervisor Feedback" = "#FF8C42"), guide = guide_legend(reverse = TRUE)) +
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
ggsave(file.path(output_path, "PST-Level Bar Chart.pdf"), width = 6, height = 4)

#Plot area for improvement----
plot_data <- analysis_data %>%
  mutate(feedback_score = rowMeans(across(c("no_afi_mentioned_feed", "classroom_management_mentioned_feed", "lesson_planning_mentioned_feed", "differentiation_mentioned_feed", "assessment_feedback_mentioned_feed", "student_engagement_mentioned_feed", "student_comprehension_mentioned_feed", "communication_mentioned_feed", "other_mentioned_feed")), na.rm = TRUE)) %>%
  mutate(feedback_score = rowMeans(across(c("no_afi_mentioned_ref", "classroom_management_mentioned_ref", "lesson_planning_mentioned_ref", "differentiation_mentioned_ref", "assessment_feedback_mentioned_ref", "student_engagement_mentioned_ref", "student_comprehension_mentioned_ref", "communication_mentioned_ref", "other_mentioned_ref")), na.rm = TRUE)) %>%
  # Make things NA if there's no area for improvement
  mutate(across(c("classroom_management_mentioned_feed", "lesson_planning_mentioned_feed", "differentiation_mentioned_feed", "assessment_feedback_mentioned_feed", "student_engagement_mentioned_feed", "student_comprehension_mentioned_feed", "communication_mentioned_feed", "other_mentioned_feed"),
                ~ifelse(no_afi_mentioned_feed==1, NA, .x))) %>%
  mutate(across(c("classroom_management_mentioned_ref", "lesson_planning_mentioned_ref", "differentiation_mentioned_ref", "assessment_feedback_mentioned_ref", "student_engagement_mentioned_ref", "student_comprehension_mentioned_ref", "communication_mentioned_ref", "other_mentioned_ref"),
                ~ifelse(no_afi_mentioned_ref==1, NA, .x)))

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
    mutate(Type = "Observation-Level"),
  pivot_longer(pst_means, 
               cols = everything(), 
               names_to = "Category", 
               values_to = "Mean") %>%
    mutate(Type = "PST-Level")
) %>%
  mutate(reflection = ifelse(grepl("_ref", Category), "PST Reflections", "Supervisor Feedback")) %>%
  mutate(
    Category = str_remove(Category, "^(overall_|pst_)"),
    Category = str_replace_all(Category, "_", " ") %>% str_to_title()
  ) %>%
  mutate(Category = gsub(" Mentioned Feed", "", Category),
         Category = gsub(" Mentioned Ref", "", Category),
         Category = ifelse(Category=="No Afi", "No Area for Improvement", Category),
         Category = ifelse(Category=="Assessment Feedback", "Assessment and Feedback", Category),
         Category = factor(Category, levels = c("Other", "Differentiation", "Student Comprehension", "Assessment and Feedback", "Student Engagement", "Communication", "Lesson Planning", "Classroom Management", "No Area for Improvement")))

# Get the desired order based on PST Reflections' mean
category_order <- plot_data_combined %>%
  filter(Type == "Observation-Level", reflection == "PST Reflections") %>%
  arrange(Mean) %>%
  pull(Category)

# Create bar plot
plot_data_combined %>%
  filter(Type == "Observation-Level" & Category != "Other" & Category != "No Area for Improvement") %>%
  mutate(Category = factor(Category, levels = category_order)) %>%
  ggplot(aes(x = Category, y = Mean, fill = reflection)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  labs(x = NULL, y = "% of Observations", fill = NULL) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.75), labels = scales::percent_format()) +
  scale_fill_manual(values = c("PST Reflections" = "#1F4E79", "Supervisor Feedback" = "#FF8C42"), guide = guide_legend(reverse = TRUE)) +
  theme(text = element_text(family = "LMRoman", color = "black", size = 16),
    plot.caption = element_text(hjust = 0),
    axis.text = element_text(color = "black", size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom")

ggsave(file.path(output_path, "Bar Chart Area for Improvement - Observation.pdf"), width = 7, height = 4)

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

#Share of times supervisors provide feedback----
# Prepare data and keep plotting order
plot_data <- analysis_data %>%
  mutate(afi_mentioned_feed = ifelse(no_afi_mentioned_feed==0, 1, 0)) %>%
  group_by(supervisor_id) %>%
  summarize(
    area_for_improvement = sum(afi_mentioned_feed),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    sup_share_area = area_for_improvement / n,
    # If supervisors never write feedback with an area for improvement, make this 0.001 so they show up in the graph
    sup_share_area = ifelse(sup_share_area == 0, 0.0025, sup_share_area)
  ) %>%
  arrange(desc(sup_share_area)) %>%
  mutate(
    supervisor_id = factor(supervisor_id, levels = supervisor_id)
  )

# For example, X\% of supervisors provide an area for improvement less than 25\% of the time and X\% never did.
plot_data %>%
  mutate(lt_25 = ifelse(sup_share_area < 0.25, 1, 0),
         never = ifelse(sup_share_area == 0.0025, 1, 0)) %>%
  summarize(across(c(lt_25, never), mean))

# Bar count and quartile positions
n_bars <- nrow(plot_data)
quartile_positions <- quantile(1:n_bars, probs = c(0.25, 0.5, 0.75), names = FALSE)
quartile_labels <- c("75th percentile", "50th percentile", "25th percentile")

# Plot
ggplot(plot_data, aes(x = supervisor_id, y = sup_share_area)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#FF8C42", width = 0.5) +
  
  # Dashed lines at quartiles
  geom_vline(
    xintercept = quartile_positions,
    linetype = "dashed",
    color = "black"
  ) +
  
  # Adjust text labels position slightly upwards
  annotate(
    "text",
    x = quartile_positions+3,
    y = 0.875,  # slightly moved up from 1.02
    label = quartile_labels,
    angle = 0,  # horizontal text
    hjust = 0.5,  # center horizontally
    size = 6,
    family = "LMRoman"
  ) +
  
  coord_flip() +
  theme_minimal() +
  labs(
    y = "% of Supervisor's Feedback with an Area for Improvement",
    x = "Supervisor"
  ) +
  scale_y_continuous(
    expand = c(0, 0.04),
    breaks = c(0,0.25, 0.5, 0.75, 1),  # setting major breaks at .25, .5, .75, and 1
    limits = c(0.0, 1),
    labels = scales::percent_format()
  ) +
  theme(
    # Remove vertical gridlines (which appear horizontal when flipped)
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Keep other aesthetics
    panel.grid.minor.x = element_blank(),
    text = element_text(family = "LMRoman", color = "black", size = 24),
    plot.caption = element_text(hjust = 0),
    axis.text = element_text(color = "black", size = 24),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold", size = 24),
    axis.text.y = element_blank()
  )
ggsave(file.path(output_path, "Supervisor pct no area for improvement.pdf"), width = 11, height = 7.5)

#How do outcomes look for the most and least strict supervisors----
analysis_data %>%
  mutate(afi_mentioned_feed = ifelse(no_afi_mentioned_feed==0, 1, 0)) %>%
  group_by(supervisor_id, sup_blup_std) %>%
  summarize(
    area_for_improvement = sum(afi_mentioned_feed),
    n_obs_sup = n(),
    prop_area_for_improvement = area_for_improvement / n_obs_sup,
    .groups = "drop"  # This removes the grouping after summarizing
  ) %>%
  ungroup() %>%
  ggplot(aes(x = sup_blup_std, y = prop_area_for_improvement)) +  # Removed size from here
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
    y = "% of Obs. with an Area for Improvement",
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