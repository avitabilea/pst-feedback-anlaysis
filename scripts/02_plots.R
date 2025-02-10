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
ggsave("C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Sentences Per Feedback Hist.pdf", width = 6, height = 4)

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
ggsave("C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Sentences Per Reflection Hist.pdf", width = 6, height = 4)

#Plot feedback descriptions----
plot_data <- analysis_data %>%
  mutate(feedback_score = rowMeans(across(c("specific_examples", "next_steps", "strengths_mentioned",
                                            "areas_for_growth", "specificity", "actionability",
                                            "balance", "growth_oriented", "standards_aligned")),
                                   na.rm = TRUE))

# Calculate overall means
overall_means <- plot_data %>%
  summarize(across(c("specific_examples", "next_steps", "strengths_mentioned", "areas_for_growth"),
                   ~mean(.x, na.rm = TRUE), .names = "overall_{.col}"))

# Calculate PST-level means (PSTs with at least one occurrence)
pst_means <- plot_data %>%
  group_by(pst_id) %>%
  summarize(across(c("specific_examples", "next_steps", "strengths_mentioned", "areas_for_growth"),
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
  mutate(Category = case_when(Category=="Strengths Mentioned" ~ "PST Strengths",
                              Category=="Specific Examples" ~ "Specific Examples",
                              Category=="Areas For Growth" ~ "Area for Improvement",
                              Category=="Next Steps" ~ "Next Steps"))

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
  )
rm(plot_data_combined, overall_means, pst_means)
ggsave("C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Feedback Bar Chart.pdf", width = 6, height = 4)

#Plot area for improvement----
# Data preparation for supervisor feedback
plot_data_1 <- analysis_data %>% 
  group_by(area_for_improvement) %>%
  summarize(n=n()) %>%
  arrange(-n) %>%
  ungroup %>%
  mutate(pct=n/sum(n)) %>%
  rename(skill=area_for_improvement) %>%
  mutate(type = "Supervisor Feedback")

# Data preparation for PST reflection
plot_data_2 <- analysis_data %>% 
  group_by(area_for_improvement_ref) %>%
  summarize(n=n()) %>%
  arrange(-n) %>%
  ungroup %>%
  mutate(pct=n/sum(n)) %>%
  rename(skill=area_for_improvement_ref) %>%
  mutate(type = "PST Reflection")

# Combine and process the data
plot_data <- bind_rows(plot_data_1, plot_data_2) %>%
  mutate(sd = pct*(1-pct), 
         se = sd/sqrt(n), 
         ci95_plus = pct+1.96*se, 
         ci95_minus = pct-1.96*se) %>%
  mutate(skill=case_when(
    skill=="none" ~ "No Area",
    skill=="multiple" ~ "Multiple Areas",
    skill=="other" ~ "Other Area",
    TRUE ~ skill
  )) %>%
  # Ensure all skills appear in both types
  complete(skill, type, fill = list(n = 0, pct = 0, sd = 0, se = 0, ci95_plus = 0, ci95_minus = 0)) %>%
  mutate(type = factor(type, levels = c("Supervisor Feedback", "PST Reflection")))

rm(plot_data_1, plot_data_2)

# Create the plot
plot_data %>%
  # Reorder based on total percentage across both types
  group_by(skill) %>%
  mutate(total_pct = sum(pct)) %>%
  ungroup() %>%
  mutate(skill = fct_reorder(skill, total_pct)) %>%
  ggplot(aes(x = skill, y = pct, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ci95_minus, ymax = ci95_plus), 
                colour = "black", alpha = 1, size = 0.5, 
                width = 0.25, position = position_dodge(0.9)) +
  theme_minimal() +
  coord_flip() +
  labs(y = "Percent of Feedback/Reflections", 
       x = "Area for Improvement") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5), 
                     limits = c(0.0, 0.58),
                     labels = scales::percent_format()) +
  theme(text = element_text(family = "LMRoman", color = "black", size = 24),
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(color = "black", size = 24),
        legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold", size = 24)) +
  scale_fill_manual(values = c("Supervisor Feedback" = "#FF8C42", 
                               "PST Reflection" = "#1F4E79"))
ggsave("C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Bar Chart Area for Improvement.pdf", width = 11, height = 7.5)

# What do PSTs mention when supervisors suggest other, nothing, or multiple areas for improvement?----
# Calculate share_same
share_same <- analysis_data %>%
  group_by(area_for_improvement, area_for_improvement_ref) %>%
  summarize(n = n(), .groups = "drop") %>%
  mutate(same = ifelse(area_for_improvement == area_for_improvement_ref, 1, 0)) %>%
  group_by(area_for_improvement, same) %>%
  summarize(n = sum(n), .groups = "drop") %>%
  group_by(area_for_improvement) %>%
  mutate(N = sum(n), share = n / N) %>%
  filter(same == 1) %>%
  select(area_for_improvement_ref=area_for_improvement, n, N, share) %>%
  mutate(group = "Same Area")

# Calculate share_none
share_none <- analysis_data %>%
  group_by(area_for_improvement, area_for_improvement_ref) %>%
  summarize(n = n(), .groups = "drop") %>%
  filter(area_for_improvement == "None") %>%
  mutate(N = sum(n), share = n / N) %>%
  select(area_for_improvement_ref, n, N, share) %>%
  mutate(group = "No Area")

# Define a function to calculate the share for each area_for_improvement_ref (Other)
calculate_shares <- function(ref_area) {
  analysis_data %>%
    filter(area_for_improvement != "None", area_for_improvement != ref_area) %>%
    group_by(area_for_improvement_ref) %>%
    summarize(n = n(), .groups = "drop") %>%
    mutate(N = sum(n), share = n / N) %>%
    filter(area_for_improvement_ref == ref_area) %>%
    select(area_for_improvement_ref, n, N, share) %>%
    mutate(group = "Other Area")
}

# Get all unique values of area_for_improvement_ref
unique_areas <- analysis_data %>%
  filter(area_for_improvement != "None") %>%
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
ggsave("C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/AFI_Ref_when_no_AFI_Feedback.pdf", width = 11, height = 7.5)

#Share of times supervisors provide no feedback----
analysis_data %>%
  group_by(supervisor_id) %>%
  summarize(
    no_area_for_improvement = sum(afi_9),
    n = n()
  ) %>%
  # filter(n>=10) %>%
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
ggsave("C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Supervisor pct no area for improvement.pdf", width = 11, height = 7.5)

#What does supervisor feedback include if no area for improvement? 
plot_data <- analysis_data %>% 
  mutate(no_www_or_afi = factor(no_www_or_afi, levels = c("none", "other", "retelling", "praise", "feedback"), labels = c( "Other", "Other", "Retelling of Lesson", "PST Praise", "Has Area for Improvement"))) %>%
  group_by(no_www_or_afi) %>%
  summarize(n=n()) %>%
  arrange(-n) %>%
  ungroup %>%
  mutate(pct=n/sum(n)) %>%
  rename(skill=no_www_or_afi)

# Create the plot
plot_data %>%
  # Reorder based on total percentage across both types
  group_by(skill) %>%
  mutate(total_pct = sum(pct)) %>%
  ungroup() %>%
  # mutate(skill = fct_reorder(skill, total_pct)) %>%
  ggplot(aes(x = skill, y = pct)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#FF8C42") +
  theme_minimal() +
  coord_flip() +
  labs(y = "Percent of Feedback", 
       x = "Broad Description of Text") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5), 
                     limits = c(0.0, 0.58),
                     labels = scales::percent_format()) +
  theme(text = element_text(family = "LMRoman", color = "black", size = 24),
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(color = "black", size = 24),
        legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold", size = 24))
ggsave("C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Bar Chart Broad Description of Text.pdf", width = 11, height = 7.5)

#How do outcomes look for the most and least strict supervisors----
analysis_data %>%
  group_by(supervisor_id, sup_blup_std) %>%
  summarize(
    no_area_for_improvement = sum(afi_9),
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
ggsave("C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Supervisor BLUPS vs AFI.pdf", width = 11, height = 7.5)

test <- analysis_data %>%
  group_by(supervisor_id, sup_blup_std) %>%
  summarize(
    no_area_for_improvement = sum(afi_9),
    n_obs_sup = n(),
    prop_no_area_for_improvement = no_area_for_improvement / n_obs_sup,
    .groups = "drop"  # This removes the grouping after summarizing
  )
feols(prop_no_area_for_improvement ~ sup_blup_std, test, vcov = "hetero", weights = ~n_obs_sup)

#Plot Observations with Feedback Containing an Area for Improvement----
analysis_data %>%
  mutate(area_for_improvement = fct_recode(area_for_improvement, "Assessment & Feedback" = "Assessment and Feedback")) %>%
  mutate(N = n_distinct(pst_id)) %>%
  group_by(pst_id, N) %>%
  count(area_for_improvement, name = "no_rows", .drop = F) %>%
  group_by(area_for_improvement, no_rows, N) %>%
  summarize(count = sum(n()), .groups = "drop") %>%
  mutate(pct = count / N) %>%
  group_by(area_for_improvement) %>%
  mutate(non_zero_pct_sum = sum(pct[no_rows != 0]),
         label_position = mean(no_rows[no_rows != 0])) %>% # Center the label over the non-zero bars
  ggplot(aes(x = no_rows, y = pct,
             fill = factor(no_rows == 0),
             color = factor(no_rows == 0))) +
  geom_bar(stat = "identity", position = "dodge") +
  # Add an annotation with the sum centered over the non-zero bars, styled in blue
  geom_text(data = . %>% distinct(area_for_improvement, non_zero_pct_sum, label_position),
            aes(x = 2, y = 0.35, label = scales::percent(non_zero_pct_sum, accuracy = 0.1)),
            inherit.aes = FALSE,
            size = 10,
            family = "LMRoman",
            hjust = 0.5,
            color = "#1F4E79") +  # Blue text to match the blue bars
  scale_fill_manual(values = c("FALSE" = "#1F4E79", "TRUE" = "#FF8C42")) +
  scale_color_manual(values = c("FALSE" = "#1F4E79", "TRUE" = "#FF8C42")) +
  facet_wrap(~area_for_improvement, nrow = 2, strip.position = "bottom", axis.labels = "all") +
  theme_minimal() +
  labs(y = "Percent of PSTs", 
       x = "Number of Observations with Feedback Containing an Area for Improvement") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0, 1, by = 0.25),
                     limits = c(0.0, 1.15),
                     labels = scales::percent_format()) +
  theme(text = element_text(family = "LMRoman", color = "black", size = 28),
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(color = "black", size = 28),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 28),
        panel.grid.minor = element_blank())
ggsave("C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Obs Containing Each AFI.pdf", width = 25, height = 10)

#Plot Observations with Reflections Containing an Area for Improvement----
analysis_data %>%
  mutate(area_for_improvement_ref = fct_recode(area_for_improvement_ref,"Assessment & Feedback" = "Assessment and Feedback")) %>%
  mutate(N=n_distinct(pst_id)) %>%
  group_by(pst_id, N) %>%
  count(area_for_improvement_ref, name = "no_rows", .drop = F) %>%
  group_by(area_for_improvement_ref, no_rows, N) %>%
  summarize(count = sum(n()), .groups = "drop") %>%
  mutate(pct = count/N) %>%
  ggplot(aes(x=no_rows, y=pct, 
             fill = factor(no_rows == 0),
             color = factor(no_rows == 0))) +
  geom_bar(stat = "identity", position = "dodge") +
  # Add label for orange bars
  geom_text(aes(label = ifelse(no_rows == 0, 
                               scales::percent(pct, accuracy = 0.1), 
                               "")),
            vjust = -0.5,
            size = 10,
            family = "LMRoman") +  
  scale_fill_manual(values = c("FALSE" = "#1F4E79", "TRUE" = "#FF8C42")) +
  scale_color_manual(values = c("FALSE" = "#1F4E79", "TRUE" = "#FF8C42")) +
  facet_wrap(~area_for_improvement_ref, nrow = 2, strip.position = "bottom", axis.labels = "all") +
  theme_minimal() +
  labs(y = "Percent of PSTs", 
       x = "Observations with Feedback Containing an Area for Improvement") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0, 1, by = 0.25),
                     limits = c(0.0, 1.1),
                     labels = scales::percent_format()) +
  theme(text = element_text(family = "LMRoman", color = "black", size = 28),
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(color = "black", size = 28),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 28),
        panel.grid.minor = element_blank())
ggsave("C:/Users/yaj3ma/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Obs Containing Each AFI Ref.pdf", width = 25, height = 10)