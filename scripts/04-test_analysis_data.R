#### Preamble ####
# Purpose: Some tests for Cleaned Dataset
# Author: Cristina Su Lam, Karen Riani, Mariko Lee
# Date: 10 October 2024 
# License: MIT
# Pre-requisites: Run 03-clean_data.R script
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(testthat)
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

#### Test 2: Ensure numeric_grade values are within the expected range
stopifnot(all(clean_president_polls$numeric_grade >= 0.5 & clean_president_polls$numeric_grade <= 3.0))

#### Test 3: Ensure state values are either valid U.S. state names
expected_states <- c(state.name)
stopifnot(all(clean_president_polls$state %in% expected_states))

#### Test 4: Ensure candidate_name contains only "Kamala Harris" and "Donald Trump"
expected_candidates <- c("Kamala Harris", "Donald Trump")
stopifnot(all(clean_president_polls$candidate_name %in% expected_candidates))

#### Test 5: Ensure pct values are within the expected range (18 to 70)
stopifnot(all(clean_president_polls$pct >= 18 & clean_president_polls$pct <= 70))

#### Test 6: Ensure sample_size values are within the expected range (111 to 26230)
stopifnot(all(clean_president_polls$sample_size >= 111 & clean_president_polls$sample_size <= 26230))

#### Test 7: Ensure pollster values are within the known list of pollsters
expected_pollsters <- c(
  "TIPP", "InsiderAdvantage", "Trafalgar Group", "co/efficient", "Emerson", 
  "Siena/NYT", "YouGov", "Research Co.", "Ipsos", "Quinnipiac", 
  "Marist", "The Washington Post", "Redfield & Wilton Strategies", 
  "Morning Consult", "Pew", "Suffolk", "Big Village", "Noble Predictive Insights", 
  "Glengariff Group Inc.", "Christopher Newport U.", "Data for Progress", 
  "Cygnal", "Impact Research", "RMG Research", "St. Anselm", 
  "YouGov/Center for Working Class Politics", "UC Berkeley", "Fabrizio/Impact", 
  "Susquehanna", "Mitchell", "WPAi", "OnMessage Inc.", "Patriot Polling", 
  "Alaska Survey Research", "Leger", "HighGround", "National Research", 
  "Winthrop U.", "High Point University", "PPP", "McCourtney Institute/YouGov", 
  "SurveyUSA", "East Carolina University", "Lake Research", "Marquette Law School", 
  "Echelon Insights", "McLaughlin", "Victory Insights", "CNN/SSRS", 
  "AtlasIntel", "Virginia Commonwealth U.", "Beacon/Shaw", "Clarity", 
  "University of Maryland/Washington Post", "Slingshot Strategies", "Meredith College", 
  "Embold Research", "Remington", "Muhlenberg", "Tarrance", 
  "University of Massachusetts Lowell/YouGov", "Mason-Dixon", "MassINC Polling Group", 
  "Hart/POS", "HarrisX", "Angus Reid", "U. New Hampshire", "J.L. Partners", 
  "Siena", "U. Georgia SPIA", "Pan Atlantic Research", "Franklin and Marshall College", 
  "Keating Research", "Change Research", "Research & Polling", "Elon U.", 
  "GQR", "Selzer", "Data Orbital", "CWS Research", "Research America", 
  "PPIC", "Washington Post/George Mason University", "Hendrix College", 
  "Elway", "HarrisX/Harris Poll", "SurveyMonkey", "Kaiser Family Foundation", 
  "Big Data Poll", "SoonerPoll.com", "Gonzales Research & Media Services", 
  "Wick", "Global Strategy Group", "EPIC/MRA", "Cherry Communications", 
  "Kaplan Strategies", "SurveyUSA/High Point University", "Fabrizio Ward", 
  "BK Strategies", "Fairleigh Dickinson", "Spry Strategies", "Roanoke College", 
  "University of Houston/Texas Southern University", "Strategies 360", 
  "YouGov Blue", "Navigator", "Peak Insights", "UMass Amherst/YouGov", 
  "Fabrizio", "Civiqs", "Public Opinion Strategies", "U. North Florida", 
  "Hoffman Research", "Targoz Market Research", "North Star Opinion Research", 
  "Landmark Communications", "Praecones Analytica", "DHM Research", 
  "MRG (Marketing Resource Group)", "Bendixen & Amandi International", 
  "1892 Polling", "University of Texas at Tyler", "P2 Insights", 
  "Innovative Research Group", "SSRS", "The Tyson Group", "KAConsulting LLC", 
  "Digital Research", "Meeting Street Insights", "John Zogby Strategies", 
  "Hart Research Associates", "RABA Research", "St. Pete Polls", 
  "GS Strategy Group", "Rasmussen", "Chism Strategies", "U. Houston", 
  "Dan Jones", "Target Insyght", "VCreek/AMG", "PRRI", "Tulchin Research", 
  "NORC", "Ohio Northern University Institute for Civics and Public Policy", 
  "Premise", "Zogby", "ABC/Washington Post", "Schoen Cooperman", 
  "OH Predictive Insights / MBQF", "Blueprint Polling", "U. Massachusetts - Lowell", 
  "Benenson", "Florida Atlantic University", "Fleming & Associates", 
  "Harris Poll", "NewsNation/Decision Desk HQ"
)

# Ensure all pollsters are in the known list
stopifnot(all(clean_president_polls$pollster %in% expected_pollsters))

