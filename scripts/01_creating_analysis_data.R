#ReadMe----
#Purpose: Putting together analysis data
#Author: Andrew Avitabile

#General----
#Load packages
pacman::p_load(conflicted, here, tidyverse, openxlsx, lme4)

# Remove everything
rm(list=ls())

# Conflict prefer
conflict_prefer(name = "filter", winner = "dplyr")

#Import data----
# Publicly-available observation-level file
pst_data <- read.xlsx(here("raw data", "PST Data.xlsx")) %>% mutate(suspensions_instances = as.numeric(suspensions_instances))

# LLM-analyzed data
llm_coded_feedback <- read.csv(here("processed data", "2025.03.05 - Feedback Analysis.csv")) %>% 
  rename_with(~ if_else(.x == "observationid", .x, paste0(.x, "_feed"))) %>%
  select(-text_feed) %>%
  # Despite begging, need to reclassify some of the area_for_improvement categorizations (applies to very few)
  mutate(area_for_improvement_feed = case_when(area_for_improvement_feed %in% c("Closure", "Closure Techniques") ~ "Lesson Planning",
                                               area_for_improvement_feed %in% c("Questioning", "Questioning Strategies") ~ "Assessment and Feedback",
                                               area_for_improvement_feed == "Time Management" ~ "Classroom Management",
                                               area_for_improvement_feed == "Instructional Delivery" ~ "Communication",
                                               area_for_improvement_feed %in% c("Curriculum Familiarity", "Content Knowledge") ~ "other",
                                               TRUE ~ area_for_improvement_feed))

llm_coded_reflections <- read.csv(here("processed data", "2025.03.06 - Reflections Analysis.csv")) %>% 
  rename_with(~ if_else(.x == "observationid", .x, paste0(.x, "_ref"))) %>%
  select(-text_ref) %>%
  # Despite begging, need to reclassify some of the area_for_improvement categorizations (applies to very few)
  mutate(area_for_improvement_ref = case_when(area_for_improvement_ref %in% c("Closure", "lesson_planning") ~ "Lesson Planning",
                                              area_for_improvement_ref %in% c("Questioning", "Questioning Strategies") ~ "Assessment and Feedback",
                                              area_for_improvement_ref %in% c("Time Management", "time management", "time_management", "classroom_management") ~ "Classroom Management",
                                              area_for_improvement_ref %in% c("Organization", "Curriculum Familiarity", "Content Knowledge") ~ "other",
                                               TRUE ~ area_for_improvement_ref))

#Preparing data----
table(pst_data$race)
analysis_data <- pst_data %>%
  # Merge in LLM-analyzed data
  left_join(llm_coded_feedback, by = "observationid") %>%
  left_join(llm_coded_reflections, by = "observationid") %>%
  
  # Clean variables
  mutate(
    programcohort = paste(certification, cohort),
    
    # Change race to have other
    race = case_when(race == "Black" ~ "Black",
                     race == "Hispanic" ~ "Hispanic",
                     race == "White, Non-Hispanic" ~ "White, Non-Hispanic",
                     race == "Asian or Pacific Islander" ~ "Asian or Pacific Islander",
                     race == "Missing" ~ "Missing",
                     TRUE ~ "Other"),
    
    # Turn categorical variables into factors
    certification = relevel(factor(certification), ref = "EC-6"),
    sat_score_cat = relevel(factor(sat_score_cat), ref = "1000-1290"),
    race = relevel(factor(race), ref = "White, Non-Hispanic"),
    faminc = relevel(factor(faminc), ref = "$80k-150k"),
    sex = relevel(factor(sex), ref = "Female"),
    mother_colldeg = relevel(factor(mother_colldeg), ref = "Yes"),
    father_colldeg = relevel(factor(father_colldeg), ref = "Yes"),
    
    # Clean area for improvement variables 
    area_for_improvement_feed = factor(area_for_improvement_feed, 
                                  levels = c("none", "Classroom Management", "Lesson Planning", "Student Engagement", "Communication", "Assessment and Feedback", "Student Comprehension", "Differentiation", "multiple", "other"),
                                  labels = c("None", "Classroom Management", "Lesson Planning", "Student Engagement", "Communication", "Assessment and Feedback", "Student Comprehension", "Differentiation", "Multiple Areas", "Other")),
    area_for_improvement_feed = relevel(area_for_improvement_feed, ref = "None"), #Note that None is reference for the feedback, as it is most common
    area_for_improvement_ref = factor(area_for_improvement_ref, 
                                      levels = c("none", "Classroom Management", "Lesson Planning", "Student Engagement", "Communication", "Assessment and Feedback", "Student Comprehension", "Differentiation", "multiple", "other"),
                                      labels = c("None", "Classroom Management", "Lesson Planning", "Student Engagement", "Communication", "Assessment and Feedback", "Student Comprehension", "Differentiation", "Multiple Areas", "Other")),
    area_for_improvement_ref = relevel(area_for_improvement_ref, ref = "Classroom Management"), #Note that Classroom Management is reference for the reflections, as it is most common
    
    # Create binary area for improvement variables
    afi_1_feed = ifelse(area_for_improvement_feed == "Classroom Management", 1, 0),
    afi_2_feed = ifelse(area_for_improvement_feed == "Lesson Planning", 1, 0),
    afi_3_feed = ifelse(area_for_improvement_feed == "Student Engagement", 1, 0),
    afi_4_feed = ifelse(area_for_improvement_feed == "Communication", 1, 0),
    afi_5_feed = ifelse(area_for_improvement_feed == "Assessment and Feedback", 1, 0),
    afi_6_feed = ifelse(area_for_improvement_feed == "Student Comprehension", 1, 0),
    afi_7_feed = ifelse(area_for_improvement_feed == "Differentiation", 1, 0),
    afi_8_feed = ifelse(area_for_improvement_feed == "Other", 1, 0),
    afi_9_feed = ifelse(area_for_improvement_feed == "Multiple Areas", 1, 0),
    afi_10_feed = ifelse(area_for_improvement_feed == "None", 1, 0),
    afi_1_ref = ifelse(area_for_improvement_ref == "Classroom Management", 1, 0),
    afi_2_ref = ifelse(area_for_improvement_ref == "Lesson Planning", 1, 0),
    afi_3_ref = ifelse(area_for_improvement_ref == "Student Engagement", 1, 0),
    afi_4_ref = ifelse(area_for_improvement_ref == "Communication", 1, 0),
    afi_5_ref = ifelse(area_for_improvement_ref == "Assessment and Feedback", 1, 0),
    afi_6_ref = ifelse(area_for_improvement_ref == "Student Comprehension", 1, 0),
    afi_7_ref = ifelse(area_for_improvement_ref == "Differentiation", 1, 0),
    afi_8_ref = ifelse(area_for_improvement_ref == "Other", 1, 0),
    afi_9_ref = ifelse(area_for_improvement_ref == "Multiple Areas", 1, 0),
    afi_10_ref = ifelse(area_for_improvement_ref == "None", 1, 0),
    
    # Number of sentences in feedback and reflections
    n_sentences_feed = ifelse(str_trim(text_feedback) == ".", 0, 
                              ifelse(str_detect(str_trim(text_feedback), "\\w+$") & !str_detect(str_trim(text_feedback), "[.!?\\r\\n]$"), 1, 
                                     str_count(text_feedback, "(?<!\\.)[.!?\\r\\n]+(?!\\.)") - 
                                       str_count(text_feedback, "\\b\\d+\\.$"))),
    n_sentences_ref = ifelse(str_trim(text_reflection) == ".", 0, 
                             ifelse(str_detect(str_trim(text_reflection), "\\w+$") & !str_detect(str_trim(text_reflection), "[.!?\\r\\n]$"), 1, 
                                    str_count(text_reflection, "(?<!\\.)[.!?\\r\\n]+(?!\\.)") - 
                                      str_count(text_reflection, "\\b\\d+\\.$"))),
    
    # Add additional variables to be used in the analyses
    inv_n_obs = 1/nobservations
    ) %>%
  # No area for improvement flagged using binary indicators
  mutate(no_afi_mentioned_feed = if_else(
    rowSums(select(., classroom_management_mentioned_feed, 
                   lesson_planning_mentioned_feed, differentiation_mentioned_feed, 
                   assessment_feedback_mentioned_feed, student_engagement_mentioned_feed, 
                   student_comprehension_mentioned_feed, communication_mentioned_feed, 
                   other_mentioned_feed)) == 0, 1, 0),
    no_afi_mentioned_ref = if_else(
      rowSums(select(., classroom_management_mentioned_ref, 
                     lesson_planning_mentioned_ref, differentiation_mentioned_ref, 
                     assessment_feedback_mentioned_ref, student_engagement_mentioned_ref, 
                     student_comprehension_mentioned_ref, communication_mentioned_ref, 
                     other_mentioned_ref)) == 0, 1, 0))
  

# Estimate models predicting average evaluation score with random intercepts for supervisor, PST, and clinical teaching school
model <- lmer(avg_eval_score_std ~ factor(observation_order) + (1 | supervisor_id) + (1 | pst_id) + (1 | st_school_id), data = analysis_data)

# Get BLUPs from models
## These random effects coefficients represent the variance in standardized evaluation scores explained by supervisors, PSTs, and placement schools. The estimates are "shrunk" towards zero to adjust for sample size, similar to teacher value-added estimates.
sup_blup_rate <- ranef(model)$supervisor_id %>% rename(supervisor_blup=`(Intercept)`) %>% rownames_to_column(var = "supervisor_id") %>% mutate(supervisor_id=as.numeric(supervisor_id))
pst_blup_rate <- ranef(model)$pst_id %>% rename(pst_blup=`(Intercept)`) %>% rownames_to_column(var = "pst_id") %>% mutate(pst_id=as.numeric(pst_id))
sch_blup_rate <- ranef(model)$st_school_id %>% rename(sch_blup=`(Intercept)`) %>% rownames_to_column(var = "st_school_id") %>% mutate(st_school_id=as.numeric(st_school_id))

# Get model-derived sd's and standardize BLUPs
sd_sup <- as_tibble(VarCorr(model)) %>% filter(grp == "supervisor_id") %>% select(sdcor) %>% as.numeric
sd_pst <- as_tibble(VarCorr(model)) %>% filter(grp == "pst_id") %>% select(sdcor) %>% as.numeric
sd_sch <- as_tibble(VarCorr(model)) %>% filter(grp == "st_school_id") %>% select(sdcor) %>% as.numeric

# Standardize BLUPs to mean 0 SD 1 using model-derived standard deviations
sup_blup_std <- mutate(sup_blup_rate, sup_blup_std = (supervisor_blup-mean(supervisor_blup))/sd_sup) %>% select(supervisor_id, sup_blup_std)
pst_blup_std <- mutate(pst_blup_rate, pst_blup_std = (pst_blup-mean(pst_blup))/sd_pst) %>% select(pst_id, pst_blup_std)
sch_blup_std <- mutate(sch_blup_rate, sch_blup_std = (sch_blup-mean(sch_blup))/sd_sch) %>% select(st_school_id, sch_blup_std)

# Add BLUPs to analysis data
analysis_data <- analysis_data %>%
  left_join(sup_blup_std, by = "supervisor_id") %>%
  left_join(pst_blup_std, by = "pst_id") %>%
  left_join(sch_blup_std, by = "st_school_id")

#Save analysis data----
saveRDS(analysis_data, here("processed data", "analysis_data.RDS"))