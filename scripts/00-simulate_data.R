#### Preamble ####
# Purpose: Simulates Data
# Author: Cristina Su Lam, Karen Riani, Mariko Lee
# Date: 10 October 2024 
# License: MIT
# Pre-requisites: None
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)
set.seed(123)

#### Simulate data ####
# Defining the states and candidates
states <- state.name  # U.S. state names
candidates <- c("Trump", "Harris")  # Presidential candidates

# Define a list of pollsters
pollsters <- c("YouGov", "Siena/NYT", "The Washington Post", "Ipsos", "Quinnipiac")

# Number of simulated polls per state
num_polls <- 10  # Simulating 10 polls per state

# Simulate sample sizes for each poll (between 500 and 2000 voters)
sample_sizes <- sample(500:2000, num_polls * length(states), replace = TRUE)

# Simulate percentage of support (pct) for each candidate (between 40% and 60%)
pct_support <- runif(num_polls * length(states), min = 40, max = 60)

# Create the simulated data frame
simulated_data <- tibble(
  state = rep(states, each = num_polls),  # Repeat each state for the number of polls
  pollster = sample(pollsters, num_polls * length(states), replace = TRUE),  # Random pollsters for each poll
  candidate_preference = sample(candidates, num_polls * length(states), replace = TRUE, prob = c(0.5, 0.5)),  # Equal chance for each candidate
  pct = pct_support,  # Simulated percentage support
  sample_size = sample_sizes  # Simulated sample sizes
)

# Plot 1: Pollster Frequencies (Number of Polls per Pollster)
ggplot(simulated_data, aes(y = pollster)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Number of Polls Conducted by Pollster", 
       x = "Count", 
       y = "Pollster") +
  theme_minimal()

# Plot 2: Bar Graph of Percentage Support (Average Support by Candidate)
ggplot(simulated_data, aes(x = candidate_preference, y = pct)) +
  stat_summary(fun = mean, geom = "bar", fill = "lightgreen") +
  labs(title = "Average Support by Candidate", 
       x = "Candidate", 
       y = "Average Percentage Support") +
  theme_minimal()

# Plot 3: Sample Size Distribution Across States
ggplot(simulated_data, aes(x = state, y = sample_size)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Distribution of Sample Sizes Across States", 
       x = "State", 
       y = "Sample Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#### Save data ####
write_csv(simulated_data, "data/00-simulated_data/simulated_data.csv")
