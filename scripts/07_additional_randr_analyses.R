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

#1. What can we say about supervisors?----

# Those who are local give more critical feedback?
# Do supervisors become more critical over time?----
test <- readxl::read_excel("C:/Users/Andre/Dropbox/Andrew and Brendan Shared Folder/PST Feedback Text/data/cleaned data/eval_text_feedback.xlsx") %>%
  select(observationid, observationdate)

# Do supervisors become more critical over time?----
analysis_data <- left_join(analysis_data, test) %>%
  mutate(date = ymd(observationdate))

analysis_data <- analysis_data %>%
  arrange(supervisor_id, date) %>%
  group_by(supervisor_id) %>%
  mutate(
    supervisor_ob = row_number(),
    supervisor_ob_cat = case_when(
      supervisor_ob <= 4 ~ "<=4",
      supervisor_ob > 4 & supervisor_ob <= 12 ~ "5 to 12",
      supervisor_ob > 12 & supervisor_ob <= 52 ~ "13 to 52",
      supervisor_ob > 52  ~ "53+"
    ),
    # factor with levels in order, but NOT ordered
    supervisor_ob_cat = factor(supervisor_ob_cat,
                               levels = c("<=4", "5 to 12", "13 to 52", "53+")),
    .groups = "drop"
  )

test <- group_by(analysis_data, supervisor_id) %>%
  summarize(supervisor_ob = max(supervisor_ob))
summary(test$supervisor_ob)

# Regressions
reg1 <- feols(avg_eval_score_std ~ supervisor_ob_cat | supervisor_id + observation_order,
              vcov = "hetero",
              data = analysis_data)
reg2 <- feols(strengths_mentioned_feed ~ supervisor_ob_cat | supervisor_id + observation_order,
              vcov = "hetero",
              data = analysis_data)
reg3 <- feols(specific_examples_feed ~ supervisor_ob_cat | supervisor_id + observation_order,
              vcov = "hetero",
              data = analysis_data)
reg4 <- feols(areas_for_growth_feed ~ supervisor_ob_cat | supervisor_id + observation_order,
              vcov = "hetero",
              data = analysis_data)
reg5 <- feols(next_steps_feed ~ supervisor_ob_cat | supervisor_id + observation_order,
              vcov = "hetero",
              data = analysis_data)

etable(list(reg1, reg2, reg3, reg4, reg5))


# Run logit models
logit1 <- feglm(strengths_mentioned_feed ~ supervisor_ob_cat | supervisor_id,
                family = binomial("logit"),
                data = analysis_data)
logit2 <- feglm(specific_examples_feed ~ supervisor_ob_cat | supervisor_id,
                family = binomial("logit"),
                data = analysis_data)
logit3 <- feglm(areas_for_growth_feed ~ supervisor_ob_cat | supervisor_id,
                family = binomial("logit"),
                data = analysis_data)
logit4 <- feglm(next_steps_feed ~ supervisor_ob_cat | supervisor_id,
                family = binomial("logit"),
                data = analysis_data)

# Print coefficient tables in console with stars
modelsummary(
  list(logit1, logit2, logit3, logit4),
  exponentiate = TRUE,
  stars = TRUE,
  output = "markdown",
  gof_omit = '.',
  fmt = 3
)