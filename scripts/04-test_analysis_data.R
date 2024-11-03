#### Preamble ####
# Purpose: Some tests for Cleaned Dataset
# Author: Mariko Lee, Karen Riani, Cristina Su Lam
# Date: 10 October 2024 
# License: MIT
# Pre-requisites: Run 03-clean_data.R script
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(arrow)

clean_president_polls <- read_parquet("data/02-analysis_data/clean_president_polls.parquet")

#### Test data ####

# Test 1: Ensure there are no missing values in important columns
stopifnot(!any(is.na(clean_president_polls$pollster)))
stopifnot(!any(is.na(clean_president_polls$numeric_grade)))
stopifnot(!any(is.na(clean_president_polls$candidate_name)))
stopifnot(!any(is.na(clean_president_polls$pct)))
stopifnot(!any(is.na(clean_president_polls$sample_size)))
stopifnot(!any(is.na(clean_president_polls$state)))

# Test 2: Ensure numeric_grade values are within the expected range
stopifnot(all(clean_president_polls$numeric_grade >= 0.5 & clean_president_polls$numeric_grade <= 3.0))

# Test 3: Ensure state values are valid U.S. state names
expected_states <- c(state.name)
stopifnot(all(clean_president_polls$state %in% expected_states))

# Test 4: Ensure candidate_name contains only "Kamala Harris" and "Donald Trump"
expected_candidates <- c("Kamala Harris", "Donald Trump")
stopifnot(all(clean_president_polls$candidate_name %in% expected_candidates))

# Test 5: Ensure pct values are within the expected range (18 to 70)
stopifnot(all(clean_president_polls$pct >= 18 & clean_president_polls$pct <= 70))

# Test 6: Ensure sample_size values are within the expected range (111 to 26230)
stopifnot(all(clean_president_polls$sample_size >= 111 & clean_president_polls$sample_size <= 26230))

# Test 7: Ensure `end_date` is not before `start_date`
stopifnot(all(clean_president_polls$end_date >= clean_president_polls$start_date))

# Test 8: Ensure all polls are recent (within 2024 or later)
min_date <- as.Date("2024-01-01")
stopifnot(all(clean_president_polls$start_date >= min_date))
stopifnot(all(clean_president_polls$end_date >= min_date))

# Test 9: Check that each state has polls for both Kamala Harris and Donald Trump
state_candidate_check <- clean_president_polls %>%
  group_by(state) %>%
  summarize(candidates = n_distinct(candidate_name)) %>%
  pull(candidates)

stopifnot(all(state_candidate_check == 2))  

# Test 10: Ensure there are no duplicate rows
num_duplicates <- sum(duplicated(clean_president_polls))
stopifnot(num_duplicates == 0)
message("Number of duplicate observations: ", num_duplicates)

# Test 11: Check for a reasonable spread in `pct` values
pct_summary <- summary(clean_president_polls$pct)
stopifnot(pct_summary["Min."] >= 18, pct_summary["Max."] <= 70)
stopifnot(pct_summary["Mean"] > 40, pct_summary["Mean"] < 60)



