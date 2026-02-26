#ReadMe----
#Purpose: Pull examples to be used in paper
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

#Load data
analysis_data <- readRDS(here("processed data", "analysis_data.RDS"))

#Examples----
# Example of "low-quality" feedback
test <- filter(analysis_data, specific_examples_feed==0 & areas_for_growth_feed==0 & next_steps_feed==0) %>%
  # select(text_feedback) %>%
  filter(text_feedback=="'s instructional skills in the classroom are good .   Growth in confidence and pacing has been seen .")

# Example of "high-quality" feedback
filter(analysis_data, specific_examples_feed==1 & areas_for_growth_feed==1 & next_steps_feed==1) %>%
  select(text_feedback) %>%
  sample_n(1)

# Example of "Classroom Management" feedback
filter(analysis_data, classroom_management_mentioned_feed==1) %>%
  select(text_feedback) %>%
  sample_n(1)

# Example of "reflection"
set.seed(12345)
filter(analysis_data, areas_for_growth_feed==1 & next_steps_feed==0 & n_sentences_feed<=4 & next_steps_ref==1 & n_sentences_ref<=3) %>%
  select(text_feedback, text_reflection) %>%
  sample_n(1)

filter(analysis_data, grepl("rapport with students and staff .   Excellent planning leading to effective lessons", text_feedback)) %>%
  select(strengths_mentioned_feed, specific_examples_feed, areas_for_growth_feed, next_steps_feed)

filter(analysis_data, grepl("rapport with students and staff .   Excellent planning leading to effective lessons", text_feedback)) %>%
  select(strengths_mentioned_ref, specific_examples_ref, areas_for_growth_ref, next_steps_ref)

# Pull five random examples of feedback/reflections add with each quality indicator
set.seed(939599)
examples_quality <- bind_rows(
  # Examples that contain strengths 
  analysis_data %>% 
    filter(strengths_mentioned_feed == 1, n_sentences_feed <= 5) %>% 
    sample_n(5) %>% 
    select(text_feedback) %>% 
    mutate(definition = ifelse(row_number() == 1, "A general comment highlighting what the preservice teacher is doing well, such as preparation, demeanor, or instructional strategies.", "")) %>%
    mutate(cleaned_feedback = text_feedback %>% str_replace_all("!\\s+!", "!!") %>% str_replace_all(" ;", ";") %>% str_replace_all(" ' ", "'") %>% str_replace_all(" : ", ":") %>% str_replace_all(" ([,\\.\\!\\?])", "\\1") %>% str_replace_all("([,\\.\\!\\?])(\\S)", "\\1 \\2") %>% str_squish()),
  # Examples that contain examples
  analysis_data %>% 
    filter(specific_examples_feed == 1, n_sentences_feed <= 5) %>% 
    sample_n(5) %>% 
    select(text_feedback) %>% 
    mutate(definition = ifelse(row_number() == 1, "A concrete description of an observed behavior, instructional move, or classroom moment that illustrates the teacher’s practice.", "")) %>%
    mutate(cleaned_feedback = text_feedback %>% str_replace_all("!\\s+!", "!!") %>% str_replace_all(" ;", ";") %>% str_replace_all(" ' ", "'") %>% str_replace_all(" : ", ":") %>% str_replace_all(" ([,\\.\\!\\?])", "\\1") %>% str_replace_all("([,\\.\\!\\?])(\\S)", "\\1 \\2") %>% str_squish()),
  # Examples that areas for growth examples
  analysis_data %>% 
    filter(areas_for_growth_feed == 1, n_sentences_feed <= 5,  grepl("Bell work was written", text_feedback)==F) %>% 
    sample_n(5) %>% 
    select(text_feedback) %>% 
    mutate(definition = ifelse(row_number() == 1, "A statement identifying something the preservice teacher needs to work on, typically tied to a specific teaching skill.", "")) %>%
    mutate(cleaned_feedback = text_feedback %>% str_replace_all("%", "\\\\%") %>% str_replace_all("!\\s+!", "!!") %>% str_replace_all(" ;", ";") %>% str_replace_all(" ' ", "'") %>% str_replace_all(" : ", ":") %>% str_replace_all(" ([,\\.\\!\\?])", "\\1") %>% str_replace_all("([,\\.\\!\\?])(\\S)", "\\1 \\2") %>% str_squish()),
  # Examples that contain next steps
  analysis_data %>% 
    filter(next_steps_feed == 1, n_sentences_feed <= 5) %>% 
    sample_n(5) %>% 
    select(text_feedback) %>% 
    mutate(definition = ifelse(row_number() == 1, "A clear, actionable suggestion or strategy the preservice teacher should try to improve their teaching.", "")) %>%
    mutate(cleaned_feedback = text_feedback %>% str_replace_all("!\\s+!", "!!") %>% str_replace_all(" ;", ";") %>% str_replace_all(" ' ", "'") %>% str_replace_all(" : ", ":") %>% str_replace_all(" ([,\\.\\!\\?])", "\\1") %>% str_replace_all("([,\\.\\!\\?])(\\S)", "\\1 \\2") %>% str_squish())
) %>%
  mutate(row_n = rep(paste("&", 1:5), 4)) %>%
  select(definition, row_n, cleaned_feedback) %>%
  tt() %>%
  group_tt(i = list("\\textbf{Strengths}: &" = 1,
                    "\\textbf{Specific Examples}: &" = 6,
                    "\\textbf{Area for Improvement}: &" = 11,
                    "\\textbf{Specific Next Steps}: &" = 16)) %>%
  save_tt("latex")

examples_quality <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", examples_quality, perl = TRUE)

# Save to LaTeX
writeLines(examples_quality, file.path(output_path, "Examples_Quality_Indicators.tex"))

set.seed(939599)
examples_afi <- bind_rows(
  # Examples that contain classroom management
  analysis_data %>% 
    filter(classroom_management_mentioned_feed == 1, n_sentences_feed <= 5) %>% 
    sample_n(5) %>% 
    select(text_feedback) %>% 
    mutate(definition = ifelse(row_number() == 1, "The teacher needs to improve routines, transitions, behavioral expectations, or ability to maintain student focus.", "")) %>%
    mutate(cleaned_feedback = text_feedback %>% str_replace_all("Chantal", "") %>% str_replace_all("Chelsie", "") %>% str_replace_all("!\\s+!", "!!") %>% str_replace_all(" ;", ";") %>% str_replace_all(" ' ", "'") %>% str_replace_all(" : ", ":") %>% str_replace_all(" ([,\\.\\!\\?])", "\\1") %>% str_replace_all("([,\\.\\!\\?])(\\S)", "\\1 \\2") %>% str_squish()),
  
  # Examples that contain lesson planning
  analysis_data %>% 
    filter(lesson_planning_mentioned_feed == 1, n_sentences_feed <= 5, grepl("effective motivation", text_feedback)==F) %>% 
    sample_n(5) %>% 
    select(text_feedback) %>% 
    mutate(definition = ifelse(row_number() == 1, "The teacher needs to better sequence content, articulate objectives, or integrate lesson components coherently.", "")) %>%
    mutate(cleaned_feedback = text_feedback %>% str_replace_all("Chantal", "") %>% str_replace_all("Chelsie", "") %>% str_replace_all("!\\s+!", "!!") %>% str_replace_all(" ;", ";") %>% str_replace_all(" ' ", "'") %>% str_replace_all(" : ", ":") %>% str_replace_all(" ([,\\.\\!\\?])", "\\1") %>% str_replace_all("([,\\.\\!\\?])(\\S)", "\\1 \\2") %>% str_squish()),
  
  # Examples that contain student engagement
  analysis_data %>% 
    filter(student_engagement_mentioned_feed == 1, n_sentences_feed <= 5) %>% 
    sample_n(5) %>% 
    select(text_feedback) %>% 
    mutate(definition = ifelse(row_number() == 1, "The teacher needs to enhance monitoring, participation, motivation, or inclusion of all students during instruction.", "")) %>%
    mutate(cleaned_feedback = text_feedback %>% str_replace_all("Chantal", "") %>% str_replace_all("Chelsie", "") %>% str_replace_all("!\\s+!", "!!") %>% str_replace_all(" ;", ";") %>% str_replace_all(" ' ", "'") %>% str_replace_all(" : ", ":") %>% str_replace_all(" ([,\\.\\!\\?])", "\\1") %>% str_replace_all("([,\\.\\!\\?])(\\S)", "\\1 \\2") %>% str_squish()),
  
  # Examples that contain communication
  analysis_data %>% 
    filter(communication_mentioned_feed == 1, n_sentences_feed <= 5, grepl("You were caring", text_feedback)==F, grepl("cycle", text_feedback)==F) %>% 
    sample_n(5) %>% 
    select(text_feedback) %>% 
    mutate(definition = ifelse(row_number() == 1, "The teacher needs to improve clarity, voice projection, presence, or responsiveness in instructional delivery. May also include non-verbal communication skills.", "")) %>%
    mutate(cleaned_feedback = text_feedback %>% str_replace_all("Chantal", "") %>% str_replace_all("Chelsie", "") %>% str_replace_all("!\\s+!", "!!") %>% str_replace_all(" ;", ";") %>% str_replace_all(" ' ", "'") %>% str_replace_all(" : ", ":") %>% str_replace_all(" ([,\\.\\!\\?])", "\\1") %>% str_replace_all("([,\\.\\!\\?])(\\S)", "\\1 \\2") %>% str_squish()),
  
  # Examples that contain assessment & feedback
  analysis_data %>% 
    filter(assessment_feedback_mentioned_feed == 1, n_sentences_feed <= 5) %>% 
    sample_n(5) %>% 
    select(text_feedback) %>% 
    mutate(definition = ifelse(row_number() == 1, "The teacher needs to strengthen their use of formative assessment or provide clearer, more effective feedback to students.", "")) %>%
    mutate(cleaned_feedback = text_feedback %>% str_replace_all("Chantal", "") %>% str_replace_all("Chelsie", "") %>% str_replace_all("!\\s+!", "!!") %>% str_replace_all(" ;", ";") %>% str_replace_all(" ' ", "'") %>% str_replace_all(" : ", ":") %>% str_replace_all(" ([,\\.\\!\\?])", "\\1") %>% str_replace_all("([,\\.\\!\\?])(\\S)", "\\1 \\2") %>% str_squish()),
  
  # Examples that contain student comprehension
  analysis_data %>% 
    filter(student_comprehension_mentioned_feed == 1, n_sentences_feed <= 5) %>% 
    sample_n(5) %>% 
    select(text_feedback) %>% 
    mutate(definition = ifelse(row_number() == 1, "The teacher should focus on ensuring students are understanding the material, including checking for understanding and adjusting instruction as needed.", "")) %>%
    mutate(cleaned_feedback = text_feedback %>% str_replace_all("Chantal", "") %>% str_replace_all("Chelsie", "") %>% str_replace_all("!\\s+!", "!!") %>% str_replace_all(" ;", ";") %>% str_replace_all(" ' ", "'") %>% str_replace_all(" : ", ":") %>% str_replace_all(" ([,\\.\\!\\?])", "\\1") %>% str_replace_all("([,\\.\\!\\?])(\\S)", "\\1 \\2") %>% str_squish()),
  
  # Examples that contain differentiation
  analysis_data %>% 
    filter(differentiation_mentioned_feed == 1, n_sentences_feed <= 5) %>% 
    sample_n(5) %>% 
    select(text_feedback) %>% 
    mutate(definition = ifelse(row_number() == 1, "The teacher should work on adjusting instruction to meet diverse learner needs through varied methods or supports.", "")) %>%
    mutate(cleaned_feedback = text_feedback %>% str_replace_all("Chantal", "")%>% str_replace_all("Chelsie", "") %>% str_replace_all("!\\s+!", "!!") %>% str_replace_all(" ;", ";") %>% str_replace_all(" ' ", "'") %>% str_replace_all(" : ", ":") %>% str_replace_all(" ([,\\.\\!\\?])", "\\1") %>% str_replace_all("([,\\.\\!\\?])(\\S)", "\\1 \\2") %>% str_squish()),
  
  # Examples that contain no area for improvement
  analysis_data %>% 
    filter(no_afi_mentioned_feed == 1, n_sentences_feed <= 5) %>% 
    sample_n(5) %>% 
    select(text_feedback) %>% 
    mutate(definition = ifelse(row_number() == 1, "Absence of any other area for improvement.", "")) %>%
    mutate(cleaned_feedback = text_feedback %>% str_replace_all("Chantal", "") %>% str_replace_all("Charli", "") %>% str_replace_all("Chelsie", "") %>% str_replace_all("!\\s+!", "!!") %>% str_replace_all(" ;", ";") %>% str_replace_all(" ' ", "'") %>% str_replace_all(" : ", ":") %>% str_replace_all(" ([,\\.\\!\\?])", "\\1") %>% str_replace_all("([,\\.\\!\\?])(\\S)", "\\1 \\2") %>% str_squish())
) %>%
  mutate(row_n = rep(paste("&", 1:5), 8)) %>%
  select(definition, row_n, cleaned_feedback) %>%
  tt() %>%
  group_tt(i = list("\\textbf{Classroom Management} &" = 1,
                    "\\textbf{Lesson Planning} &" = 6,
                    "\\textbf{Student Engagement} &" = 11,
                    "\\textbf{Communication} &" = 16,
                    "\\textbf{Assessment and Feedback} &" = 21,
                    "\\textbf{Student Comprehension} &" = 26,
                    "\\textbf{Differentiation} &" = 31,
                    "\\textbf{None} &" = 36)) %>%
  save_tt("latex")

examples_afi <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", examples_afi, perl = TRUE)

# Save to LaTeX
writeLines(examples_afi, file.path(output_path, "Examples_AFI.tex"))