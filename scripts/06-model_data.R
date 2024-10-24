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

#### Read upcoming presidential election forecast data ####
clean_president_polls <- read_parquet("data/02-analysis_data/clean_president_polls.parquet")

### Logistic Regression ###

# Normalize weights to a reasonable scale
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
clean_president_polls


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
