#### Preamble ####
# Purpose: Models
# Author: Cristina Su Lam, Karen Riani, Mariko Lee
# Date: 23 October 2024
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(arrow)
library(dplyr)
library(rstanarm)
library(pROC)

#### Read upcoming presidential election forecast data ####
clean_president_polls <- read_parquet("data/02-analysis_data/clean_president_polls.parquet")

# Convert categorical variables to factors
clean_president_polls$pollster <- as.factor(clean_president_polls$pollster)
clean_president_polls$state <- as.factor(clean_president_polls$state)

### Logistic Regression ###

# Weights
clean_president_polls <- clean_president_polls %>%
  mutate(weight = sample_size / mean(sample_size)) %>%
  mutate(weight = weight * numeric_grade)

model_logistic <- glm(
  is_harris ~ pollster + state + sample_size + pct,
  data = clean_president_polls,
  family = binomial(link = "logit"),
  weights = weight
)
model_logistic

# Predict the probabilities of Harris winning
clean_president_polls <- clean_president_polls %>%
  mutate(predicted_prob_harris = predict(model_logistic, type = "response"))


# Model Validation & Checking
set.seed(123)
train_indices <- sample(seq_len(nrow(clean_president_polls)), size = 0.7 * nrow(clean_president_polls))

# Create training and test datasets
train_data <- clean_president_polls[train_indices, ]
test_data <- clean_president_polls[-train_indices, ]

model_logistic_train <- glm(
  is_harris ~ pollster + state + sample_size + pct + pollster:pct,
  data = train_data,
  family = binomial(link = "logit"),
  weights = weight
)


# Remove unused levels from the model object
model_logistic_clean <- model_logistic
model_logistic_clean$xlevels$pollster <- intersect(levels(test_data$pollster), levels(train_data$pollster))
model_logistic_clean$xlevels$state <- intersect(levels(test_data$state), levels(train_data$state))

# Run the prediction
test_data <- test_data %>%
  mutate(predicted_prob_harris = predict(model_logistic_clean, newdata = test_data, type = "response"))

# Create a binary prediction (Harris wins if probability > 0.5)
test_data <- test_data %>%
  mutate(prediction = ifelse(predicted_prob_harris > 0.5, 1, 0))

# Calculate accuracy
accuracy <- mean(test_data$prediction == test_data$is_harris)
print(accuracy)

# Calculate AUC
roc_curve <- roc(test_data$is_harris, test_data$predicted_prob_harris)
auc_value <- auc(roc_curve)
print(auc_value)

### Bayesian Model ###

# Specify priors 
priors <- normal(0, 2.5, autoscale = TRUE)

# Model 
model_formula <- is_harris ~ sample_size + pct + (1 | pollster) + (1 | state)

# Fit the model
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

# Posterior predictive checks
pp_check(bayesian_model)

# Summarize the model
summary(bayesian_model)

# Plot random effects
plot(bayesian_model, pars = "(Intercept)", prob = 0.95)

#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)
