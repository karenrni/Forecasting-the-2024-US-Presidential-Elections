#### Preamble ####
# Purpose: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Author: Cristina Su Lam, Karen Riani, Mariko Lee
# Date: 10 October 2024 
# License: MIT
# Pre-requisites: Need to have downloaded the data
# Any other information needed? None

#### Workspace setup ####

#### Plot data ####
base_plot <- ggplot(high_quality_harris, aes(x = end_date, y = pct)) +
  theme_classic() +
  labs(y = "Harris percent", x = "Date")

# Plots poll estimates and overall smoothing
base_plot +
  geom_point() +
  geom_smooth()

# Color by pollster
# This gets messy - need to add a filter - see line 21
base_plot +
  geom_point(aes(color = pollster)) +
  geom_smooth() +
  theme(legend.position = "bottom")

# Facet by pollster
# Make the line 21 issue obvious
# Also - is there duplication???? Need to go back and check
base_plot +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(pollster))

# Color by pollscore
base_plot +
  geom_point(aes(color = factor(pollscore))) +
  geom_smooth() +
  theme(legend.position = "bottom")


#### Starter models ####
# Model 1: pct as a function of end_date
model_date <- lm(pct ~ end_date, data = high_quality_harris)

# Model 2: pct as a function of end_date and pollster
model_date_pollster <- lm(pct ~ end_date + pollster, data = high_quality_harris)

# Augment data with model predictions
high_quality_harris <- high_quality_harris |>
  mutate(
    fitted_date = predict(model_date),
    fitted_date_pollster = predict(model_date_pollster)
  )

# Plot model predictions
# Model 1
ggplot(high_quality_harris, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date), color = "blue", linetype = "dotted") +
  theme_classic() +
  labs(y = "Harris percent", x = "Date", title = "Linear Model: pct ~ end_date")

# Model 2
ggplot(high_quality_harris, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date_pollster), color = "blue", linetype = "dotted") +
  facet_wrap(vars(pollster)) +
  theme_classic() +
  labs(y = "Harris percent", x = "Date", title = "Linear Model: pct ~ end_date + pollster")

# All of the above would be in scripts - data cleaning scripts and modelling scripts. 
# This is an example of how you get a results table that you could put into your Quarto doc
modelsummary(models = list("Model 1" = model_date, "Model 2" = model_date_pollster))


#### Bayesian models ####
# Change 'pollster' and 'state' to factor variables
high_quality_harris <- high_quality_harris |>
  mutate(
    pollster = factor(pollster),
    state = factor(state)
  )

# Model 1
model_formula_1 <- cbind(num_harris, sample_size - num_harris) ~ (1 | pollster)

# Model 2
model_formula_2 <- cbind(num_harris, sample_size - num_harris) ~ (1 | pollster) + (1 | state)

# Specify priors
priors <- normal(0, 2.5, autoscale = TRUE)

# Fit the models
bayesian_model_1 <- stan_glmer(
  formula = model_formula_1,
  data = high_quality_harris,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95
)

bayesian_model_2 <- stan_glmer(
  formula = model_formula_2,
  data = high_quality_harris,
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
high_quality_harris <- high_quality_harris |>
  mutate(
    end_date_num = as.numeric(end_date - min(end_date))
  )

# Fit Bayesian model with spline and pollster as fixed effect
# cf bayesian_model_1 and bayesian_model_2 where it's a random effect - note the different interpretations
spline_model <- stan_glm(
  pct ~ ns(end_date_num, df = 5) + pollster, # Change df for the number of "bits" - higher numbers - more "wiggly" - but then need to worry about overfitting.
  data = high_quality_harris,
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
    min(high_quality_harris$end_date_num),
    max(high_quality_harris$end_date_num),
    length.out = 100
  ),
  pollster = factor("YouGov", levels = levels(high_quality_harris$pollster))
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
