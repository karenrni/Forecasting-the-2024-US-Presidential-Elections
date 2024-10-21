#### Preamble ####
# Purpose: Simulates Data
# Author: Cristina Su Lam, Karen Riani, Mariko Lee
# Date: 10 October 2024 
# License: MIT
# Pre-requisites: None
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)
library(tidymodels)
library(arrow)

#### Read Data ####

upef2024_data <- 
  read_parquet(file = here::here("data/analysis_data/polls_data.parquet"))
# here::here not necessary, just more robust 

#### Simulate data ####
set.seed(333)

## Split data into test and training sets
upef2024_split <- initial_split(upef2024_data, prop = 0.80)
upef2024_train <- training(upef2024_split)
upef2024_test <- testing(upef2024_split)

## Fit logistic model
upef2024_tidymodels <-
  logistic_reg(mode = "classification") |>
  set_engine("glm") |>
  fit(
    pct ~ pollster + is_state,
    data = upef2024_train
  )

upef2024_tidymodels

## Evaluate on test set.. this is not
upef2024_tidymodels |>
  predict(new_data = upef2024_test) |>
  cbind(upef2024_test) |>
  conf_mat(truth = pct, estimate = .pred_class)

## K-Fold Cross Validation



### Simulation Tests

# Confirm the start and end date
start_date <- as.Date("2024-01-01")
end_date <- as.Date("2024-00-00")

# Set the number of random dates you want to generate
number_of_dates <- 100

data <-
  tibble(
    dates = as.Date(
      runif(
        n = number_of_dates,
        min = as.numeric(start_date),
        max = as.numeric(end_date)
      ),
      origin = "1970-01-01"
    ),
    number_of_marriage = rpois(n = number_of_dates, lambda = 15)
  )


#### Write_csv
write_csv(data, file = "data/raw_data/simulated.csv")


