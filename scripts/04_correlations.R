# ReadMe----
# Purpose: Generate correlation matrix for feedback topics
# Author: Andrew Avitabile

# General----
# Load required packages
pacman::p_load(tidyverse, showtext, here, corrplot, psych, reshape2)

# Add fonts
font_add(family = "LMRoman", regular = here("lmroman10-regular.otf"))
showtext_auto()

# Clear workspace
rm(list = ls())


analysis_data <- readRDS(here("processed data", "analysis_data.RDS"))

pst_level_data <- analysis_data %>%
  group_by(pst_id) %>%
  summarise(
    classroom_management_mentioned_feed = max(classroom_management_mentioned_feed, na.rm = TRUE),
    lesson_planning_mentioned_feed = max(lesson_planning_mentioned_feed, na.rm = TRUE),
    differentiation_mentioned_feed = max(differentiation_mentioned_feed, na.rm = TRUE),
    assessment_feedback_mentioned_feed = max(assessment_feedback_mentioned_feed, na.rm = TRUE),
    student_engagement_mentioned_feed = max(student_engagement_mentioned_feed, na.rm = TRUE),
    student_comprehension_mentioned_feed = max(student_comprehension_mentioned_feed, na.rm = TRUE),
    communication_mentioned_feed = max(communication_mentioned_feed, na.rm = TRUE),
    other_mentioned_feed = max(other_mentioned_feed, na.rm = TRUE),
    no_afi_mentioned_feed = max(no_afi_mentioned_feed, na.rm = TRUE)
  ) %>%
  ungroup()

data_clean <- pst_level_data %>%
  select(no_afi_mentioned_feed, classroom_management_mentioned_feed, lesson_planning_mentioned_feed, 
         student_engagement_mentioned_feed, communication_mentioned_feed, 
         assessment_feedback_mentioned_feed, student_comprehension_mentioned_feed, 
         differentiation_mentioned_feed, other_mentioned_feed)

# Convert columns to numeric
data_clean <- data_clean %>% mutate(across(everything(), as.numeric))

# Calculate the tetrachoric correlation matrix
corr <- psych::tetrachoric(data_clean)$rho

# Calculate p-values for the correlations
p.mat <- corr.p(r = corr, n = nrow(data_clean))$p

.get_upper_tri <- function(cormat, show.diag = FALSE) {
  if (is.null(cormat)) {
    return(cormat)
  }
  cormat[lower.tri(cormat)] <- NA
  if (!show.diag) {
    diag(cormat) <- NA
  }
  return(cormat)
}

p.mat <- .get_upper_tri(as.matrix(p.mat), show.diag = T) 
diag(p.mat) <- 1

corr <- .get_upper_tri(as.matrix(corr), show.diag = T) 
diag(p.mat) <- 1

corr <- reshape2::melt(corr, na.rm = TRUE, as.is = F)
colnames(corr) <- c("Var1", "Var2", "value")
corr$pvalue <- rep(NA, nrow(corr))
corr$signif <- rep(NA, nrow(corr))

corr <- as_tibble(corr) %>%
  mutate(Var2 = factor(Var2, levels = c("no_afi_mentioned_feed", "classroom_management_mentioned_feed", "lesson_planning_mentioned_feed", 
                                        "student_engagement_mentioned_feed", "communication_mentioned_feed", 
                                        "assessment_feedback_mentioned_feed", "student_comprehension_mentioned_feed", 
                                        "differentiation_mentioned_feed", "other_mentioned_feed"),
                       labels = c("No Area for Improvement, 1", "Classroom Management, 2", "Lesson Planning, 3", "Student Engagement, 4", 
                                  "Communication, 5", "Assessment and Feedback, 6", "Student Comprehension, 7", "Differentiation, 8", 
                                  "Other, 9"))) 

p.mat <- reshape2::melt(p.mat, na.rm = TRUE)
corr$coef <- corr$value
corr$pvalue <- p.mat$value
corr$signif <- as.numeric(p.mat$value <= 0.05)
p.mat <- subset(p.mat, p.mat$value > 0.05)
corr$value <- corr$value * corr$signif
colors = c("#E46726", "white", "#6D9EC1")
corr <- as_tibble(corr)
corr <- mutate(corr, label = round(x = coef, digits = 4))

#Remove diagonal
# corr <- filter(corr, coef!=1)

p <- ggplot2::ggplot(data = corr, mapping = ggplot2::aes(x = as.factor(Var1), 
                                                         y = as.factor(Var2), fill = coef)) +
  ggplot2::geom_tile(color = "grey") +
  ggplot2::geom_text(mapping = ggplot2::aes(x = Var1, y = Var2), parse = F, color = "black", size = 4, family = "LMRoman",
                     label = ifelse(corr$pvalue < 0.001, paste0(sprintf("%2.2f",round(corr$coef, 2)), "***"),
                                    ifelse(corr$pvalue < 0.01, paste0(sprintf("%2.2f",round(corr$coef, 2)), "**"),
                                           ifelse(corr$pvalue < 0.05, paste0(sprintf("%2.2f",round(corr$coef, 2)), "*"), paste0(sprintf("%2.2f",round(corr$coef, 2))))))) +
  ggplot2::scale_fill_gradient2(low = colors[1], high = colors[3], mid = colors[2], midpoint = 0, limit = c(-0.75,0.75), 
                                space = "Lab", name = "Tetrachoric\nCorrelation", na.value = "grey90") +
  theme(text = element_text(family="LMRoman", color = "black"), axis.text = element_text(color="black"), panel.grid = element_blank(), plot.caption = element_text(hjust=0)) +
  scale_x_discrete(labels = 1:11, position = "top") +
  scale_y_discrete(limits=rev) +
  labs(y=element_blank(), x=element_blank()) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5, size = 12, hjust = 0.5), axis.text.y = ggplot2::element_text(size = 12),
                 panel.background = element_rect(fill='transparent')) +
  ggplot2::coord_fixed()
p