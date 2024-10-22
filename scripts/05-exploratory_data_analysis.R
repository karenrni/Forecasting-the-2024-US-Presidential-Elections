#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Cristina Su Lam, Karen Riani, Mariko Lee
# Date: 
# Contact: 
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(arrow)
library(dplyr)

#### Read upcoming presidential election forecast data ####
upef2024_data <- 
  read_parquet(file = here::here("data/02-analysis_data/clean_president_polls.parquet"))

# Change 'pollster' and 'state' to factor variables
upef2024_data <- upef2024_data |>
  mutate(
    pollster = factor(pollster),
    state = factor(state)
  )

### Multi-level Regression ###
# Model 1
model_formula_1 <- cbind(round((pct / 100) * sample_size, 0), sample_size - round((pct / 100) * sample_size, 0)) ~ (1 | pollster) + (1 | state)

# Specify priors
priors <- normal(0, 2.5, autoscale = TRUE)

first_model <- stan_glmer(
  formula = model_formula_1,
  data = upef2024_data,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)

#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)
