#Relationship between feedback and supervisor ratings----

analysis_data <- analysis_data %>% group_by(supervisor_id) %>% mutate(n_obs_sup = 1/n(), avg_pst_sup_eval = mean(avg_pst_sup_eval, na.rm=T), avg_coop_sup_eval = mean(avg_coop_sup_eval, na.rm=T)) %>% ungroup() 
analysis_data <- analysis_data %>% mutate(avg_pst_sup_eval_std = scale(avg_pst_sup_eval), avg_coop_sup_eval_std = scale(avg_coop_sup_eval))

# Standardize evals within cohort and by supervisor
model1 <- feols(log(avg_pst_sup_eval) ~ strengths_mentioned_feed + specific_examples_feed + areas_for_growth_feed + next_steps_feed + sup_blup_std | observation_order + cohort, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model2 <- feols(log(avg_pst_sup_eval) ~ strengths_mentioned_feed + specific_examples_feed + areas_for_growth_feed + next_steps_feed + sup_blup_std | observation_order + cohort + st_school_id, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model3 <- feols(log(avg_coop_sup_eval) ~ strengths_mentioned_feed + specific_examples_feed + areas_for_growth_feed + next_steps_feed + sup_blup_std | observation_order + cohort, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model4 <- feols(log(avg_coop_sup_eval) ~ strengths_mentioned_feed + specific_examples_feed + areas_for_growth_feed + next_steps_feed + sup_blup_std | observation_order + cohort + st_school_id, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
multiariate_models <- list(model1,model2,model3,model4)
modelsummary(multiariate_models)










model1 <- feols(avg_pst_sup_eval_std ~ strengths_mentioned | observation_order + cohort, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model2 <- feols(avg_pst_sup_eval_std ~ specific_examples | observation_order + cohort, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model3 <- feols(avg_pst_sup_eval_std ~ areas_for_growth | observation_order + cohort, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model4 <- feols(avg_pst_sup_eval_std ~ next_steps | observation_order + cohort, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model5 <- feols(avg_pst_sup_eval_std ~ sup_blup_std | observation_order + cohort, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
univariate_models_1 <- list(model1,model2,model3,model4,model5)
model6 <- feols(avg_pst_sup_eval_std ~ strengths_mentioned | observation_order + cohort + st_school_id, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model7 <- feols(avg_pst_sup_eval_std ~ specific_examples| observation_order + cohort + st_school_id, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model8 <- feols(avg_pst_sup_eval_std ~ areas_for_growth | observation_order + cohort + st_school_id, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model9 <- feols(avg_pst_sup_eval_std ~ next_steps | observation_order + cohort + st_school_id, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model10 <- feols(avg_pst_sup_eval_std ~ sup_blup_std | observation_order + cohort + st_school_id, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
univariate_models_2 <- list(model6,model7,model8,model9,model10)
model1 <- feols(avg_coop_sup_eval_std ~ strengths_mentioned | observation_order + cohort, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model2 <- feols(avg_coop_sup_eval_std ~ specific_examples | observation_order + cohort, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model3 <- feols(avg_coop_sup_eval_std ~ areas_for_growth | observation_order + cohort, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model4 <- feols(avg_coop_sup_eval_std ~ next_steps | observation_order + cohort, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model5 <- feols(avg_coop_sup_eval_std ~ sup_blup_std | observation_order + cohort, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
univariate_models_3 <- list(model1,model2,model3,model4,model5)
model6 <- feols(avg_coop_sup_eval_std ~ strengths_mentioned | observation_order + cohort + st_school_id, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model7 <- feols(avg_coop_sup_eval_std ~ specific_examples| observation_order + cohort + st_school_id, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model8 <- feols(avg_coop_sup_eval_std ~ areas_for_growth | observation_order + cohort + st_school_id, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model9 <- feols(avg_coop_sup_eval_std ~ next_steps | observation_order + cohort + st_school_id, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
model10 <- feols(avg_coop_sup_eval_std ~ sup_blup_std | observation_order + cohort + st_school_id, data=analysis_data, cluster = "supervisor_id", weight = ~n_obs_sup)
univariate_models_4 <- list(model6,model7,model8,model9,model10)

cm <- c("strengths_mentioned" = "Strength",
        "specific_examples" = "Specific Examples",
        "areas_for_growth" = "Area for Improvement",
        "next_steps" = "Next Steps",
        "sup_blup_std" = "Supervisor BLUP")

univariate_table_1 <- modelsummary(univariate_models_1,
                                   stars = c('*' = .05, '**' = .01, '***' = .001),
                                   coef_map = cm,
                                   gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors|N|R2|FE|Mean',
                                   shape = "rbind",
                                   output = "data.frame") %>%
  rename(term=` `, univariate_1=`(1)`) %>%
  mutate(statistic = ifelse(term=="", "std.error", "estimate")) %>%
  mutate(term = ifelse(term=="", lag(term), term)) %>%
  bind_rows(tribble(~term, ~univariate_1, ~statistic,
                    "N", "-", "",
                    "$R^2$", "-", "",
                    "Cohort FE", "X", "",
                    "Order FE", "X", "",
                    "Placement School FE", "", ""))
univariate_table_2 <- modelsummary(univariate_models_2,
                                   stars = c('*' = .05, '**' = .01, '***' = .001),
                                   coef_map = cm,
                                   gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors|N|R2|FE|Mean',
                                   shape = "rbind",
                                   output = "data.frame") %>%
  rename(term=` `, univariate_2=`(1)`) %>%
  mutate(statistic = ifelse(term=="", "std.error", "estimate")) %>%
  mutate(term = ifelse(term=="", lag(term), term)) %>%
  bind_rows(tribble(~term, ~univariate_2, ~statistic,
                    "N", "-", "",
                    "$R^2$", "-", "",
                    "Cohort FE", "X", "",
                    "Order FE", "X", "",
                    "Placement School FE", "X", ""))
univariate_table_3 <- modelsummary(univariate_models_3,
                                   stars = c('*' = .05, '**' = .01, '***' = .001),
                                   coef_map = cm,
                                   gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors|N|R2|FE|Mean',
                                   shape = "rbind",
                                   output = "data.frame") %>%
  rename(term=` `, univariate_3=`(1)`) %>%
  mutate(statistic = ifelse(term=="", "std.error", "estimate")) %>%
  mutate(term = ifelse(term=="", lag(term), term)) %>%
  bind_rows(tribble(~term, ~univariate_3, ~statistic,
                    "N", "-", "",
                    "$R^2$", "-", "",
                    "Cohort FE", "X", "",
                    "Order FE", "X", "",
                    "Placement School FE", "", ""))
univariate_table_4 <- modelsummary(univariate_models_4,
                                   stars = c('*' = .05, '**' = .01, '***' = .001),
                                   coef_map = cm,
                                   gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors|N|R2|FE|Mean',
                                   shape = "rbind",
                                   output = "data.frame") %>%
  rename(term=` `, univariate_4=`(1)`) %>%
  mutate(statistic = ifelse(term=="", "std.error", "estimate")) %>%
  mutate(term = ifelse(term=="", lag(term), term)) %>%
  bind_rows(tribble(~term, ~univariate_4, ~statistic,
                    "N", "-", "",
                    "$R^2$", "-", "",
                    "Cohort FE", "X", "",
                    "Order FE", "X", "",
                    "Placement School FE", "X", ""))

multivariate_table <- modelsummary(models = multiariate_models,
                                   stars = c('*' = .05, '**' = .01, '***' = .001),
                                   coef_map = cm,
                                   gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                                   gof_map = gm_bi,
                                   output = "data.frame") 

combined_table <- full_join(univariate_table_1, multivariate_table, by = c("term", "statistic")) %>%
  full_join(univariate_table_2, by = c("term", "statistic")) %>%
  full_join(univariate_table_3, by = c("term", "statistic")) %>%
  full_join(univariate_table_4, by = c("term", "statistic")) %>%
  mutate(term = ifelse(statistic=="std.error", "", term)) %>%
  select(-c(statistic, part)) %>%
  select(term, `(1)`, univariate_1, `(2)`, univariate_2, `(3)`, univariate_3, `(4)`, univariate_4)

combined_table[is.na(combined_table)] <- "-"

# Split the combined_table into chunks of two rows each
split_tables <- split(combined_table, ceiling(seq_along(1:nrow(combined_table))/2))

# Iterate over each chunk and create LaTeX tables
for (i in seq_along(split_tables)) {
  table_chunk <- tt(split_tables[[i]]) %>%
    save_tt("latex")
  
  # Clean the LaTeX table by extracting the relevant part
  table_chunk <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table_chunk, perl = TRUE) %>% replace_x_with_checkmark()
  
  # Define the file name for each chunk (e.g., Pred_Ref_AFR_w_Feed_AFR_part1.tex)
  file_name <- paste0("C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/Pred_Sup_Ratings_", i, ".tex")
  
  # Save the LaTeX table
  writeLines(table_chunk, file_name)
}

#Relationship between feedback and evaluation scores----

model1 <- feols(avg_eval_score_std ~ strengths_mentioned | supervisor_id + observation_order + programcohort, data=analysis_data, cluster = "pst_id")
model2 <- feols(avg_eval_score_std ~ strengths_mentioned | pst_id + observation_order, data=analysis_data, cluster = "pst_id")
model3 <- feols(avg_eval_score_std ~ specific_examples | supervisor_id + observation_order + programcohort, data=analysis_data, cluster = "pst_id")
model4 <- feols(avg_eval_score_std ~ specific_examples | pst_id + observation_order, data=analysis_data, cluster = "pst_id")
model5 <- feols(avg_eval_score_std ~ areas_for_growth | supervisor_id + observation_order + programcohort, data=analysis_data, cluster = "pst_id")
model6 <- feols(avg_eval_score_std ~ areas_for_growth | pst_id + observation_order, data=analysis_data, cluster = "pst_id")
model7 <- feols(avg_eval_score_std ~ next_steps | supervisor_id + observation_order + programcohort, data=analysis_data, cluster = "pst_id")
model8 <- feols(avg_eval_score_std ~ next_steps | pst_id + observation_order, data=analysis_data, cluster = "pst_id")
model9 <- feols(avg_eval_score_std ~ strengths_mentioned + specific_examples + areas_for_growth + next_steps | supervisor_id + observation_order + programcohort, data=analysis_data, cluster = "pst_id")
model10 <- feols(avg_eval_score_std ~ strengths_mentioned + specific_examples + areas_for_growth + next_steps | pst_id + observation_order, data=analysis_data, cluster = "pst_id")
models <- list(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10)

cm <- c("strengths_mentioned" = "Strength",
        "specific_examples" = "Specific Examples",
        "areas_for_growth" = "Area for Improvement",
        "next_steps" = "Next Steps")
table <- modelsummary(models = models,
                      stars = c('*' = .05, '**' = .01, '***' = .001),
                      coef_map = cm,
                      gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors',
                      gof_map = gm) %>%
  save_tt("latex")
table <- sub("(?s).*?\\\\midrule(.*?)\\\\bottomrule.*", "\\1", table, perl = TRUE) %>% replace_x_with_checkmark()
writeLines(table, "C:/Users/Andre/Dropbox/Apps/Overleaf/PST Feedback Text Analysis/figures_and_tables/relationship_btw_eval_score_and_feedback.tex")