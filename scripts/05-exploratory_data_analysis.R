#### Preamble ####
# Purpose: Exploratory Data Analysis for Cleaned Dataset
# Author: Mariko Lee, Karen Riani, Cristina Su Lam
# Date: 10 October 2024 
# License: MIT
# Pre-requisites: 
#   - Run 03-clean_data.R script
#   - The `rstanarm` package must be installed and loaded
#   - The `maps` package must be installed and loaded
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)
library(rstanarm) 
library(arrow)
library(maps) 

#### Read upcoming presidential election forecast data ####
clean_president_polls <- read_parquet("data/02-analysis_data/clean_president_polls.parquet")

# Summary statistics for pct, sample_size, and numeric_grade
summary_stats <- clean_president_polls %>%
  summarize(
    mean_pct = mean(pct, na.rm = TRUE),
    sd_pct = sd(pct, na.rm = TRUE),
    min_pct = min(pct, na.rm = TRUE),
    max_pct = max(pct, na.rm = TRUE),
    mean_sample_size = mean(sample_size, na.rm = TRUE),
    sd_sample_size = sd(sample_size, na.rm = TRUE),
    min_sample_size = min(sample_size, na.rm = TRUE),
    max_sample_size = max(sample_size, na.rm = TRUE),
    mean_numeric_grade = mean(numeric_grade, na.rm = TRUE),
    sd_numeric_grade = sd(numeric_grade, na.rm = TRUE),
    min_numeric_grade = min(numeric_grade, na.rm = TRUE),
    max_numeric_grade = max(numeric_grade, na.rm = TRUE)
  )

# Create a summary table in the desired format
summary_table <- data.frame(
  `Statistic` = c("Percentage Support", "Sample Size", "Numeric Grade"),
  `Mean` = c(
    format(round(summary_stats$mean_pct, 2), big.mark = ",", scientific = FALSE),
    format(round(summary_stats$mean_sample_size, 2), big.mark = ",", scientific = FALSE),
    format(round(summary_stats$mean_numeric_grade, 2), big.mark = ",", scientific = FALSE)
  ),
  `SD` = c(
    format(round(summary_stats$sd_pct, 2), big.mark = ",", scientific = FALSE),
    format(round(summary_stats$sd_sample_size, 2), big.mark = ",", scientific = FALSE),
    format(round(summary_stats$sd_numeric_grade, 2), big.mark = ",", scientific = FALSE)
  ),
  `Min` = c(
    format(summary_stats$min_pct, big.mark = ",", scientific = FALSE),
    format(summary_stats$min_sample_size, big.mark = ",", scientific = FALSE),
    format(summary_stats$min_numeric_grade, big.mark = ",", scientific = FALSE)
  ),
  `Max` = c(
    format(summary_stats$max_pct, big.mark = ",", scientific = FALSE),
    format(summary_stats$max_sample_size, big.mark = ",", scientific = FALSE),
    format(summary_stats$max_numeric_grade, big.mark = ",", scientific = FALSE)
  )
)

summary_table

# 1. Numeric Grade (Numeric) Distribution
ggplot(clean_president_polls, aes(x = pct, fill = candidate_name)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Candidate Support Percentages", 
       x = "Percentage", 
       y = "Frequency", 
       fill = "Candidate Name") +  
  facet_wrap(~ candidate_name)

# 2. Plot the distribution of pollsters 
# Filter pollsters with more than 20 counts
pollsters_over_20 <- clean_president_polls %>%
  group_by(pollster) %>%
  filter(n() > 20)

ggplot(pollsters_over_20, aes(x = reorder(pollster, -table(pollster)[pollster]))) +
  geom_bar(fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Polls by Pollster", x = "Pollster", y = "Number of Polls") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Plot the distribution of polls by state 
ggplot(clean_president_polls, aes(x = reorder(state, -table(state)[state]))) +
  geom_bar(fill = "green", alpha = 0.7) +
  labs(title = "Number of Polls by State", x = "State", y = "Number of Polls") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 4. Sample Size (Numeric) Distribution
ggplot(clean_president_polls, aes(x = sample_size)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "orange", color = "black", alpha = 0.7) +
  geom_density(color = "blue", size = 1) +  # Add a density curve
  labs(title = "Distribution of Poll Sample Sizes with Density Curve", 
       x = "Sample Size", 
       y = "Density") +
  scale_x_continuous(labels = scales::comma)

# 5. Plot for candidate_chosen variable
ggplot(clean_president_polls, aes(x = factor(candidate_chosen))) +
  geom_bar(fill = "purple", alpha = 0.7) +
  labs(title = "Distribution of Polls by Harris vs Trump", x = "Chosen Candidate", y = "Number of Polls") +
  scale_x_discrete(labels = c("0" = "Trump", "1" = "Harris"))

# 6. Map
# Summarize data to find the candidate with the highest percentage in each state
state_winners <- clean_president_polls %>%
  filter(candidate_name %in% c("Kamala Harris", "Donald Trump")) %>%
  group_by(state) %>%
  summarize(winner = candidate_name[which.max(pct)])  # Find the candidate with the highest pct

# Convert state names to lowercase to match map_data
state_winners$state <- tolower(state_winners$state)

# Load USA state map data
usa_states <- map_data("state")

# Merge and sort the winners with the USA map data
merged_data <- merge(usa_states, state_winners, by.x = "region", by.y = "state", sort = FALSE, all.x = TRUE)
merged_data <- merged_data[order(merged_data$group, merged_data$order), ]

# Plot the election map
ggplot(data = merged_data, aes(x = long, y = lat, group = group, fill = winner)) +
  geom_polygon(color = "black", size = 0.3) + 
  labs(fill = "Winner") +
  scale_fill_manual(values = c("Kamala Harris" = "blue", "Donald Trump" = "red")) +  # Blue for Harris, Red for Trump
  coord_fixed(1.3) + 
  labs(title = "Predicted 2024 Election Results by State", x = "Longitude", y = "Latitude") +
  theme_void() 



