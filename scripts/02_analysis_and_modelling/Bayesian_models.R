#### Preamble ####
# Purpose: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Author: Cristina Su Lam, Karen Riani, Mariko Lee
# Date: 10 October 2024 
# License: MIT
# Pre-requisites: Need to have downloaded the data
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(modelsummary)
library(rstanarm)
library(splines)

#### Read Data ####
upef2024_data <- 
  read_parquet(file = here::here("data/analysis_data/high_quality_harris.parquet"))

#### Bayesian models ####
# Change 'pollster' and 'state' to factor variables
upef2024_data <- upef2024_data |>
  mutate(
    pollster = factor(pollster),
    state = factor(state)
  )

### Multi-level Regression ###
# Model 1
model_formula_1 <- cbind(num_harris, sample_size - num_harris) ~ (1 | pollster)

# Model 2
model_formula_2 <- cbind(num_harris, sample_size - num_harris) ~ (1 | pollster) + (1 | state)

# Specify priors
priors <- normal(0, 2.5, autoscale = TRUE)

# Fit the models
bayesian_model_1 <- stan_glmer(
  formula = model_formula_1,
  data = upef2024_data,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)

bayesian_model_2 <- stan_glmer(
  formula = model_formula_2,
  data = upef2024_data,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)
# Note the warnings here and follow the instructions for dealing with it - come back and fix.

# Posterior predictive checks
pp_check(bayesian_model_1)
pp_check(bayesian_model_2)

# Summarize the model
summary(bayesian_model_1)
summary(bayesian_model_2)

# Plot random effects
plot(bayesian_model_1, pars = "(Intercept)", prob = 0.95)
plot(bayesian_model_2, pars = "(Intercept)", prob = 0.95)

# Model summary works the same as above for Bayesian models.


#### Bayesian models and splines ####
# Change date to be number of days since she declared - it's a counter not a date
upef2024_data <- upef2024_data |>
  mutate(
    end_date_num = as.numeric(end_date - min(end_date))
  )

# Fit Bayesian model with spline and pollster as fixed effect
# cf bayesian_model_1 and bayesian_model_2 where it's a random effect - note the different interpretations
spline_model <- stan_glm(
  pct ~ ns(end_date_num, df = 5) + pollster, # Change df for the number of "bits" - higher numbers - more "wiggly" - but then need to worry about overfitting.
  data = upef2024_data,
  family = gaussian(),
  prior = normal(0, 5),
  prior_intercept = normal(50, 10),
  seed = 1234,
  iter = 2000,
  chains = 4,
  refresh = 0
)

# Summarize the model
summary(spline_model)

# Posterior predictive checks
pp_check(spline_model)

# Predict and plot
# Create new data for prediction
new_data <- data.frame(
  end_date_num = seq(
    min(upef2024_data$end_date_num),
    max(upef2024_data$end_date_num),
    length.out = 100
  ),
  pollster = factor("YouGov", levels = levels(upef2024_data$pollster))
)

# Predict posterior draws
posterior_preds <- posterior_predict(spline_model, newdata = new_data)

# Summarize predictions
pred_summary <- new_data |>
  mutate(
    pred_mean = colMeans(posterior_preds),
    pred_lower = apply(posterior_preds, 2, quantile, probs = 0.025),
    pred_upper = apply(posterior_preds, 2, quantile, probs = 0.975)
  )

# Plot the spline fit
ggplot(high_quality, aes(x = end_date_num, y = pct, color = pollster)) +
  geom_point() +
  geom_line(
    data = pred_summary,
    aes(x = end_date_num, y = pred_mean),
    color = "blue",
    inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = pred_summary,
    aes(x = end_date_num, ymin = pred_lower, ymax = pred_upper),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  labs(
    x = "Days since earliest poll",
    y = "Percentage",
    title = "Poll Percentage over Time with Spline Fit"
  ) +
  theme_minimal()
