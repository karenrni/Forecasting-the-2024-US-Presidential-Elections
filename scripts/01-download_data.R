#### Preamble ####
# Purpose: Downloads and saves the data
# Authors: Cristina Su Lam, Karen Riani, Mariko Lee
# Date: 10 October 2024 
# License: MIT
# Pre-requisites: None
# Any other information needed? None

# Load necessary libraries
library(tidyverse)
library(readr)

# Define the path to the data
data_path <- "data/raw_data/raw_president_polls.csv"
         
# Read the CSV file into a data frame
polls_data <- read_csv(data_path)

# Display the first few rows of the data
head(polls_data)

