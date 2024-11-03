#### Preamble ####
# Purpose: Fitting Model and Making Predictions
# Author: Cristina Su Lam, Karen Riani, Mariko Lee
# Date: 23 October 2024
# License: MIT
# Pre-requisites: Run 03-clean_data.R script
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(arrow)
library(rstanarm)
library(pROC)


#### Read upcoming presidential election forecast data ####
clean_president_polls <- read_parquet("data/02-analysis_data/clean_president_polls.parquet")

# Convert categorical variables to factors
clean_president_polls$pollster <- as.factor(clean_president_polls$pollster)
clean_president_polls$state <- as.factor(clean_president_polls$state)

# Set seed for reproducibility
set.seed(123)

### Step 1: Prepare Data and Define Initial Model ###

# Add weights to the dataset
clean_president_polls <- clean_president_polls %>%
  mutate(weight = (numeric_grade * (sample_size / mean(sample_size))))


# Fit the initial logistic regression model
logistic_model <- glm(
  is_harris ~ pollster + state + sample_size + pct,
  data = clean_president_polls,
  family = binomial(link = "logit"),
  weights = weight
)

# Predict the probabilities of Harris winning for the entire dataset
clean_president_polls_log <- clean_president_polls %>%
  mutate(predicted_prob_harris = predict(logistic_model, type = "response"))

# Calculate the weighted average predicted probability for Kamala Harris
overall_predicted_prob_harris <- mean(clean_president_polls_log$predicted_prob_harris)

# Convert to percentage
overall_percentage_harris <- overall_predicted_prob_harris * 100
overall_percentage_trump <- (1 - overall_predicted_prob_harris) * 100

# Print the results
cat("Overall Percentage for Kamala Harris:", overall_percentage_harris, "%\n")
cat("Overall Percentage for Donald Trump:", overall_percentage_trump, "%\n")


# Calculate the average predicted probability by state
state_predictions <- clean_president_polls_log %>%
  group_by(state) %>%
  summarize(avg_predicted_prob_harris = mean(predicted_prob_harris))

# Determine the winner for each state based on the average probability
state_predictions <- state_predictions %>%
  mutate(state_winner = ifelse(avg_predicted_prob_harris > 0.5, "Harris", "Trump"))

# Count the number of states won by each candidate
overall_winner_summary <- state_predictions %>%
  count(state_winner)

# View results
state_predictions    
overall_winner_summary 

### Step 2: Train/Test Split for Model Validation ###

# Create training and test datasets (70% train, 30% test)
train_indices <- sample(seq_len(nrow(clean_president_polls)), size = 0.7 * nrow(clean_president_polls))
train_data <- clean_president_polls[train_indices, ]
test_data <- clean_president_polls[-train_indices, ]

### Step 3: Fit Logistic Regression Model on Training Set ###

# Fit a logistic regression model with interaction term on the training data
model_logistic_train <- glm(
  is_harris ~ pollster + state + sample_size + pct + pollster:pct,
  data = train_data,
  family = binomial(link = "logit"),
  weights = weight
)

# Remove unused levels from the model object
model_logistic_clean <- model_logistic_train
model_logistic_clean$xlevels$pollster <- intersect(levels(test_data$pollster), levels(train_data$pollster))
model_logistic_clean$xlevels$state <- intersect(levels(test_data$state), levels(train_data$state))

### Step 4: Make Predictions on Test Set ###

# Predict probabilities on the test set using the trained model
test_data <- test_data %>%
  mutate(predicted_prob_harris = predict(model_logistic_clean, newdata = test_data, type = "response"))

# Create a binary prediction (Harris wins if probability > 0.5)
test_data <- test_data %>%
  mutate(prediction = ifelse(predicted_prob_harris > 0.5, 1, 0))

### Step 5: Model Evaluation ###

# Calculate RMSE between actual outcomes and predicted probabilities
rmse_value1 <- sqrt(mean((test_data$is_harris - test_data$predicted_prob_harris)^2))
rmse_value1

# Calculate AUC 
auc_value1 <- auc(test_data$is_harris, test_data$predicted_prob_harris)
auc_value1


### Bayesian Model ###

### Step 1: Fit Initial Model on Full Dataset ###

# Specify priors
priors <- normal(0, 2.5, autoscale = TRUE)

# Define the Bayesian model formula with random effects for pollster and state
model_formula <- is_harris ~ sample_size + pct + (1 | pollster) + (1 | state)

# Fit the initial Bayesian model on the full dataset
bayesian_model <- stan_glmer(
  formula = model_formula,
  data = clean_president_polls,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  weights = weight,
  adapt_delta = 0.95
)

# Predict probabilities for Harris winning across all posterior draws
predicted_probs_matrix <- posterior_predict(bayesian_model, type = "response")

# Take the mean across posterior samples for each observation
# Transpose the matrix to get one mean per observation
clean_president_polls_bay <- clean_president_polls %>%
  mutate(predicted_prob_harris = rowMeans(t(predicted_probs_matrix)))

# Calculate the unweighted average predicted probability for Kamala Harris
overall_predicted_prob_harris <- mean(clean_president_polls_bay$predicted_prob_harris)

# Convert to percentage
overall_percentage_harris <- overall_predicted_prob_harris * 100
overall_percentage_trump <- (1 - overall_predicted_prob_harris) * 100

# Print the results
cat("Overall Percentage for Kamala Harris:", overall_percentage_harris, "%\n")
cat("Overall Percentage for Donald Trump:", overall_percentage_trump, "%\n")

# Calculate the average predicted probability by state for Harris
state_predictions <- clean_president_polls_bay %>%
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
print(state_predictions)

# View the summary of states won by each candidate
print(overall_winner_summary)






# Posterior predictive checks and plot for the initial model
pp_check(bayesian_model)
plot(bayesian_model, pars = "(Intercept)", prob = 0.95)

### Step 2: Train/Test Split for Model Validation ###

# Split the data into 70% training and 30% testing
train_indices <- sample(seq_len(nrow(clean_president_polls)), size = 0.7 * nrow(clean_president_polls))
train_data <- clean_president_polls[train_indices, ]
test_data <- clean_president_polls[-train_indices, ]

### Step 3: Fit the Model on Training Set ###

# Fit the Bayesian model on the training set only
bayesian_model_train <- stan_glmer(
  formula = model_formula,
  data = train_data,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  weights = weight,
  adapt_delta = 0.95
)

### Step 4: Make Predictions on Test Set ###

# Generate predicted probabilities on the test set using posterior predictive sampling
test_data <- test_data %>%
  mutate(predicted_prob_harris = posterior_predict(bayesian_model_train, newdata = test_data, draws = 1) %>% rowMeans())

# Create a binary prediction (Harris wins if probability > 0.5)
test_data <- test_data %>%
  mutate(prediction = ifelse(predicted_prob_harris > 0.5, 1, 0))


### Step 5: Model Validation ###

# Calculate RMSE between actual outcomes and predicted probabilities
rmse_value2 <- sqrt(mean((test_data$is_harris - test_data$predicted_prob_harris)^2))
print(rmse_value2)

# Calculate AUC
auc_value2 <- auc(test_data$is_harris, test_data$predicted_prob_harris)
auc_value2

#### Save model ####
saveRDS(
  model_logistic_clean,
  file = "models/first_model.rds"
)

saveRDS(
  bayesian_model,
  file = "models/second_model.rds"
)
