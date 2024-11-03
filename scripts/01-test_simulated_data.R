#### Preamble ####
# Purpose: Tests the structure and validity of the Simulated Data about US 2024 Elections.
# Author: Mariko Lee, Karen Riani, Cristina Su Lam
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
stopifnot(!any(is.na(simulated_data)))

# Test 2: Ensure `pct` values are within a valid range (0% to 100%)
stopifnot(all(simulated_data$pct >= 0 & simulated_data$pct <= 100))

# Test 3: Ensure `candidate_preference` contains only "Trump" and "Harris"
stopifnot(all(simulated_data$candidate_preference %in% expected_candidates))

# Test 4: Ensure `state` contains valid U.S. states (this assumes all `state.name` values)
stopifnot(all(simulated_data$state %in% expected_states))

# Test 5: Ensure `pollster` contains expected pollster names
stopifnot(all(simulated_data$pollster %in% expected_pollsters))

# Test 6: Check for duplicates in the simulated data
stopifnot(nrow(simulated_data) == nrow(distinct(simulated_data)))

# Test 7: Ensure `sample_size` is within a reasonable range (500 to 2000 as simulated)
stopifnot(all(simulated_data$sample_size >= 500 & simulated_data$sample_size <= 2000))

# Test 8: Ensure `date` is within the expected range
stopifnot(all(simulated_data$date >= start_date & simulated_data$date <= end_date))

# Test 9: Check if state bias is applied correctly
biased_states <- simulated_data %>% 
  filter(state %in% c("California", "New York", "Texas", "Alabama")) %>% 
  group_by(state) %>% 
  summarize(avg_pct = mean(pct))

# Assuming California and New York should lean lower (e.g., < 50) and Texas and Alabama higher (e.g., > 50)
stopifnot(all(biased_states$avg_pct[biased_states$state %in% c("California", "New York")] < 50))
stopifnot(all(biased_states$avg_pct[biased_states$state %in% c("Texas", "Alabama")] > 50))

# Test 10: Check candidate support distribution
candidate_avg <- simulated_data %>% 
  group_by(candidate_preference) %>% 
  summarize(avg_pct = mean(pct))

# Assuming the support should be centered around 50
stopifnot(all(candidate_avg$avg_pct >= 40 & candidate_avg$avg_pct <= 60))

# Test 11: Ensure columns are of correct data types
stopifnot(is.character(simulated_data$state))
stopifnot(is.character(simulated_data$pollster))
stopifnot(is.character(simulated_data$candidate_preference))
stopifnot(is.numeric(simulated_data$pct))
stopifnot(is.numeric(simulated_data$sample_size))
stopifnot(is.Date(simulated_data$date))

# Test 12: Check pollster frequency distribution
pollster_counts <- simulated_data %>% 
  count(pollster)

# Ensure each pollster appears at least once (or adjust based on expectations)
stopifnot(all(pollster_counts$n > 0))

