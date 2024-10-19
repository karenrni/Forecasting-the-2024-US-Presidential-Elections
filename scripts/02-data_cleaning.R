#### Preamble ####
# Purpose: Cleans the presidential poll data into an analysis data set
# Author: Cristina Su Lam, Karen Riani, Mariko Lee
# Date: 10 October 2024 
# License: MIT
# Pre-requisites: Need to have downloaded the data
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)

#### Clean data ####
poll_data <- read_csv("data/raw_data/raw_president_polls.csv")

polls_data <- polls_data %>%
  janitor::clean_names()

# Handle missing values (example: remove incomplete rows)
poll_data <- poll_data %>%
  drop_na()

#### Save data ####
write_csv(analysis_data, "data/analysis_data/analysis_data.csv")