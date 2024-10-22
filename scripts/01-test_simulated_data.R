#### Preamble ####
# Purpose: Tests the structure and validity of the simulated American elections dataset.
# Author: Cristina Su Lam, Karen Riani, Mariko Lee
# Date: 10 October 2024 
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded
  # - 00-simulate_data.R must have been run
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)

simulated_data <- read_csv("data/00-simulated_data/simulated_data.csv")

# Test if the data was successfully loaded
if (exists("simulated_data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}

#### Test Simulated Data ####

expected_candidates <- c("Trump", "Harris")
expected_states <- state.name
expected_pollsters <- c("YouGov", "Siena/NYT", "The Washington Post", "Ipsos", "Quinnipiac")

# Test 1: Ensure there are no missing values
stopifnot(!any(is.na(simulate_data)))

# Test 2: Ensure `pct` values are within a valid range (0% to 100%)
stopifnot(all(simulate_data$pct >= 0 & simulate_data$pct <= 100))

# Test 3: Ensure `candidate_preference` contains only "Trump" and "Harris"
stopifnot(all(simulate_data$candidate_preference %in% expected_candidates))

# Test 4: Ensure `state` contains valid U.S. states (this assumes all `state.name` values)
stopifnot(all(simulate_data$state %in% expected_states))

# Test 5: Ensure `pollster` contains expected pollster names
stopifnot(all(simulate_data$pollster %in% expected_pollsters))

# Test 6: Check for duplicates in the simulated data
stopifnot(nrow(simulate_data) == nrow(distinct(simulate_data)))

# Test 7: Ensure `sample_size` is within a reasonable range (500 to 2000 as simulated)
stopifnot(all(simulate_data$sample_size >= 500 & simulate_data$sample_size <= 2000))
