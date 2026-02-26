# Function to create a stacked table of bivariate and multivariate regression results
bi_and_muti_variate_feols <- function(
    dependent_var,         # String with dependent variable name
    independent_vars,      # Vector of independent variable names
    data,                  # Data frame
    fixed_effects = NULL,  # Vector of fixed effects
    cluster_var = NULL,    # Variable to cluster on
    weights = NULL,        # Optional weights variable for weighted regression
    stars = c('*' = .1, '**' = .05, '***' = .01)  # Significance levels
) {
  require(fixest)
  require(dplyr)
  require(modelsummary)
  
  # Create fixed effects formula part
  fe_part <- ""
  if (!is.null(fixed_effects) && length(fixed_effects) > 0) {
    fe_part <- paste0(" | ", paste(fixed_effects, collapse = " + "))
  }
  
  # Create all bivariate models (one for each independent variable)
  bivariate_models <- list()
  for (i in seq_along(independent_vars)) {
    formula_text <- paste0(dependent_var, " ~ ", independent_vars[i], fe_part)
    
    # Run the model with appropriate parameters
    if (!is.null(cluster_var) && !is.null(weights)) {
      bivariate_models[[i]] <- feols(as.formula(formula_text), data = data, cluster = cluster_var, weights = weights)
    } else if (!is.null(cluster_var)) {
      bivariate_models[[i]] <- feols(as.formula(formula_text), data = data, cluster = cluster_var)
    } else if (!is.null(weights)) {
      bivariate_models[[i]] <- feols(as.formula(formula_text), data = data, weights = weights)
    } else {
      bivariate_models[[i]] <- feols(as.formula(formula_text), data = data)
    }
  }
  
  # Create multivariate model (all independent variables together)
  formula_text <- paste0(dependent_var, " ~ ", paste(independent_vars, collapse = " + "), fe_part)
  
  # Run the model with appropriate parameters
  if (!is.null(cluster_var) && !is.null(weights)) {
    multivariate_model <- feols(as.formula(formula_text), data = data, cluster = cluster_var, weights = weights)
  } else if (!is.null(cluster_var)) {
    multivariate_model <- feols(as.formula(formula_text), data = data, cluster = cluster_var)
  } else if (!is.null(weights)) {
    multivariate_model <- feols(as.formula(formula_text), data = data, weights = weights)
  } else {
    multivariate_model <- feols(as.formula(formula_text), data = data)
  }
  
  # Get bivariate results using modelsummary directly to data.frame
  univariate_table <- modelsummary(
    bivariate_models, 
    stars = stars,
    gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors|N|R2|FE|Mean', 
    output = "data.frame"
  )
  
  # Get multivariate results using modelsummary directly to data.frame
  multivariate_table <- modelsummary(
    multivariate_model,
    stars = stars,
    gof_omit = 'DF|Deviance|AIC|BIC|RMSE|Std.Errors|N|R2|FE|Mean',
    output = "data.frame"
  )
  
  # Merge multivariate and univeraiate parts
  univariate_table <- univariate_table %>%
    pivot_longer(cols = -(part:statistic), names_to = "model", values_to = "value") %>%
    filter(!is.na(value) & value != "") %>%
    select(part, term, statistic, value)
  
  table <- left_join(univariate_table, multivariate_table) %>%
    rename(univariate_results = value, multivariate_results = (1))
  
  return(list(table = table, univariate_table = univariate_table, multivariate_table = multivariate_table))
}