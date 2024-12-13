#### Preamble ####
# Purpose: Download the presidential poll data from website.
# Author: Mariko Lee, Karen Riani, Cristina Su Lam
# Date: 10 October 2024 
# License: MIT
# Pre-requisites: The `readr` package must be installed and loaded
# Any other information needed? None

#### Workspace setup ####
library(readr)

#### Download/Read Dataset ####
raw_president_polls <- read_csv("https://projects.fivethirtyeight.com/polls/data/president_polls.csv")

#### Save Data ####
write_csv(raw_president_polls, "data/01-raw_data/raw_president_polls.csv") 

