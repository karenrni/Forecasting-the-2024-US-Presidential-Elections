#### Preamble ####
# Purpose: Cleans the presidential poll data into an analysis data set
# Author: Cristina Su Lam, Karen Riani, Mariko Lee
# Date: 10 October 2024 
# License: MIT
# Pre-requisites: Need to have downloaded the data
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)
library(arrow)

#### Clean data ####
polls_data <- read_csv("data/raw_data/raw_president_polls.csv")

#polls_data <- polls_data %>%
 # janitor::clean_names()

# Handle missing values (example: remove incomplete rows)
#polls_data <- polls_data %>%
 # drop_na()

# Binary for state or national polls
polls_data <- polls_data %>%
  mutate(is_state = as.numeric(!is.na(state)))

# Categorize pct for Harris into High and Low Support
polls_data <- polls_data %>%
  mutate(pct = as.factor(ifelse(pct > 50, "High", "Low")))

# Filter for high quality pollsters and state-specific Harris polls
polls_data %>%
  filter(numeric_grade >= 3.0, 
         candidate_name == "Kamala Harris")

#### Save data ####

write_parquet(polls_data, "data/analysis_data/polls_data.parquet")
# delete analysis data csv 

