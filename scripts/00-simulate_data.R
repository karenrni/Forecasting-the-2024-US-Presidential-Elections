#### Preamble ####
# Purpose: Simulates Data
# Author: Mariko Lee, Karen Riani, Cristina Su Lam
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
candidates <- c("Trump", "Harris") 

# Define a list of pollsters
pollsters <- c("YouGov", "Siena/NYT", "The Washington Post", "Ipsos", "Quinnipiac")

# Number of simulated polls per state
num_polls <- 10  

# Simulate sample sizes for each poll (between 500 and 2000 voters)
sample_sizes <- sample(500:2000, num_polls * length(states), replace = TRUE)

# Simulate percentage of support (pct) for each candidate (between 40% and 60%)
pct_support <- runif(num_polls * length(states), min = 40, max = 60)

# Date
start_date <- as.Date("2024-07-21")
end_date <- as.Date("2024-10-31")
poll_dates <- sample(seq(start_date, end_date, by = "day"), num_polls * length(states), replace = TRUE)


# Create the simulated data frame
simulated_data <- tibble(
  state = rep(states, each = num_polls),
  pollster = sample(pollsters, num_polls * length(states), replace = TRUE),  
  candidate_preference = sample(candidates, num_polls * length(states), replace = TRUE, prob = c(0.5, 0.5)), 
  pct = pct_support, 
  sample_size = sample_sizes,
  date = poll_dates
)

# Vary Support by State (e.g., giving higher support to one candidate in traditionally "red" or "blue" states)
state_bias <- ifelse(simulated_data$state %in% c("California", "New York"), -3, 
                     ifelse(simulated_data$state %in% c("Texas", "Alabama"), 3, 0))
simulated_data$pct <- simulated_data$pct + state_bias


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

# Plot 4: Candidate Support Over Time
ggplot(simulated_data, aes(x = date, y = pct, color = candidate_preference)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.2)) +
  labs(title = "Support for Each Candidate Over Time",
       x = "Date",
       y = "Average Percentage Support",
       color = "Candidate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 5: Support Distribution by State and Candidate
# Filter for key states
key_states <- c("Florida", "Pennsylvania", "Ohio", "Texas", "California", "New York")
filtered_data <- simulated_data %>% filter(state %in% key_states)

ggplot(filtered_data, aes(x = state, y = pct, fill = candidate_preference)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  labs(title = "Distribution of Support in Key States for Each Candidate",
       x = "State",
       y = "Percentage Support",
       fill = "Candidate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("blue", "red"))

#### Save data ####
write_csv(simulated_data, "data/00-simulated_data/simulated_data.csv")
