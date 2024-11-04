#### Preamble ####
# Purpose: Fitting Model and Making Predictions.
# Author: Mariko Lee, Karen Riani, Cristina Su Lam
# Date: 23 October 2024
# License: MIT
# Pre-requisites: 
#   - Run 03-clean_data.R script
#   - The `rsample` package must be installed and loaded
#   - The `MLmetrics` package must be installed and loaded
#   - The `modelsummary` package must be installed and loaded
#   - The `performance` package must be installed and loaded
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(arrow)
library(rsample)
library(MLmetrics)
library(modelsummary)
library(performance)

#### Read upcoming presidential election forecast data ####
clean_president_polls <- read_parquet("data/02-analysis_data/clean_president_polls.parquet")


# Add weights to the dataset
clean_president_polls <- clean_president_polls %>%
  mutate(weight = (numeric_grade * (sample_size / mean(sample_size))),
         pollster = as.factor(pollster),
         state = as.factor(state)
         )

#### Bayesian Model ####
# Define priors
priors <- normal(0.5, 0.1, autoscale = TRUE)

# Fit model with complete dataset
set.seed(123)
bayesian_model <- stan_glmer(
  formula = is_harris ~ pct + (1 | pollster) + (1 | state),
  data = clean_president_polls,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  weights = weight,
  adapt_delta = 0.95
)

# Predict probabilities for Harris winning 
clean_president_polls <- clean_president_polls |> 
  mutate(
    predicted_prob_harris = posterior_predict(bayesian_model, newdata = clean_president_polls, type = "response") |> colMeans(),
    winner_harris = ifelse(predicted_prob_harris > 0.500, 1, 0)
  )
# Calculate the unweighted average predicted probability for Kamala Harris
overall_predicted_prob_harris <- mean(clean_president_polls$predicted_prob_harris)

# Convert to percentage
overall_percentage_harris <- overall_predicted_prob_harris * 100
overall_percentage_trump <- (1 - overall_predicted_prob_harris) * 100

# Print the overall results
cat("Overall Percentage for Kamala Harris:", overall_percentage_harris, "%\n")
cat("Overall Percentage for Donald Trump:", overall_percentage_trump, "%\n")

# Calculate the average predicted probability by state for Harris
state_predictions <- clean_president_polls %>%
  group_by(state) %>%
  summarize(
    avg_predicted_prob_harris = mean(predicted_prob_harris)
  )

# Determine the winner for each state based on the average probability
state_predictions <- state_predictions %>%
  mutate(
    state_winner = ifelse(avg_predicted_prob_harris > 0.5, "Harris", "Trump")
  )

# Count the number of states won by each candidate
overall_winner_summary <- state_predictions %>%
  count(state_winner)

# View the state-level predictions
print(state_predictions, n = Inf)

# View the summary of states won by each candidate
print(overall_winner_summary)

### Model Validation/Checking ###
# Set seed and create train/test split
set.seed(123)
train_indices <- sample(seq_len(nrow(clean_president_polls)), size = 0.7 * nrow(clean_president_polls))
training_data <- clean_president_polls[train_indices, ]
training_data <- training_data %>%
  mutate(pct_scaled = scale(pct))
testing_data <- clean_president_polls[-train_indices, ]

# Ensure test data has compatible pollster levels with training data
testing_data <- testing_data |>
  filter(pollster %in% unique(training_data$pollster))

# Fit `Bayesian model` with training dataset
model_validation_train <- stan_glmer(
  formula = is_harris ~ pct + (1 | pollster) + (1 | state),
  data = training_data,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  weights = weight,
  cores = 4,
  adapt_delta = 0.99,
  seed = 123
)

# Posterior predictive check
pp_check(model_validation_train)

# Make predictions on the test set for the Bayesian model
set.seed(123)
bayesian_predicted_data <- testing_data |> 
  select(pollster, state, start_date, end_date, is_harris, pct) |> 
  mutate(
    predicted_prob_harris = posterior_predict(model_validation_train, newdata = testing_data, type = "response") |> colMeans(),
    winner_harris = ifelse(predicted_prob_harris > 0.500, 1, 0)
  )

# Model evaluation metrics for Bayesian model
f1_score_b <- F1_Score(bayesian_predicted_data$is_harris, bayesian_predicted_data$winner_harris)
auc_value_b <- AUC(bayesian_predicted_data$is_harris, bayesian_predicted_data$winner_harris)
rmse_value_b <- RMSE(bayesian_predicted_data$is_harris, bayesian_predicted_data$predicted_prob_harris)

cat("Bayesian Model - F1 Score:", f1_score_b, "AUC:", auc_value_b, "RMSE:", rmse_value_b, "\n")

# Fit `Logistic Regression` model with training dataset
model_logistic_train <- glm(
  is_harris ~ pollster + state + pct,
  data = training_data,
  family = binomial(link = "logit"),
  weights = weight
)

check_collinearity(model_logistic_train)

# Make predictions on the test set for the logistic model
set.seed(123)
logistic_predicted_data <- testing_data |> 
  select(pollster, state, start_date, end_date, is_harris, pct) |> 
  mutate(
    predicted_prob_harris = predict(model_logistic_train, newdata = testing_data, type = "response"),
    winner_harris = ifelse(predicted_prob_harris > 0.500, 1, 0)
  )

# Model evaluation metrics for Logistic model
f1_score_l <- F1_Score(logistic_predicted_data$is_harris, logistic_predicted_data$winner_harris)
auc_value_l <- AUC(logistic_predicted_data$is_harris, logistic_predicted_data$winner_harris)
rmse_value_l <- RMSE(logistic_predicted_data$is_harris, logistic_predicted_data$predicted_prob_harris)

cat("Logistic Model - F1 Score:", f1_score_l, "AUC:", auc_value_l, "RMSE:", rmse_value_l, "\n")

#### Save model ####
saveRDS(
  bayesian_model,
  file = "models/first_model.rds"
)


