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
library(janitor)
library(lubridate)
library(broom)

#### Clean data ####
high_quality_harris <- read_csv("data/raw_data/raw_president_polls.csv") |>
  clean_names()

# Binary for state or national polls
high_quality_harris <- high_quality_harris %>%
  mutate(is_state = as.numeric(!is.na(state)),
  end_date = mdy(end_date))

# Categorize pct for Harris into High and Low Support in new col
high_quality_harris$pct_category <- ifelse(high_quality_harris$pct > 50, "High", "Low")
high_quality_harris$pct_category <- as.factor(high_quality_harris$pct_category)

# Filter for high quality pollsters and state-specific Harris polls
high_quality_harris %>%
  filter(
    numeric_grade >= 3.0, 
    candidate_name == "Kamala Harris"
  ) 

high_quality_harris <- high_quality_harris %>%
  filter(end_date >= as.Date("2024-07-21"))
high_quality_harris <- high_quality_harris %>%
  mutate(
    num_harris = round((pct / 100) * sample_size, 0) # Convert pct to actual count of Harris supporters
  )

#### Save data ####
write_parquet(high_quality_harris, "data/analysis_data/high_quality_harris.parquet")

# delete analysis data csv 
