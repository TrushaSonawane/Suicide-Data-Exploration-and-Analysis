# Author: Trusha S, Yash S, Shivam P, Surya D, Mahalakshmi S
# Created: 2025-04-18
# Edited: 2025-04-21
# Course: ALY6040

# Clear environment and console
cat("\014") # Clears console
rm(list = ls()) # Clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # Clears packages
options(scipen = 100) # Disables scientific notation for entire R session

# Load necessary libraries
library(pacman)
p_load(tidyverse)

# Read the data
data <- read.csv("master.csv")

# Basic data structure
str(data)
summary(data)

# Calculate the median of the 'hdi' column, ignoring NA values
data <- data %>%
  select(-(hdi))

# Check the dropped column
summary(data)

# ---- Distribution Analysis ----

# 1. Suicide Rates by Age and Gender
ggplot(data, aes(x = age, y = suicides.100k, fill = sex)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Suicide Rates by Age Group and Gender", 
       x = "Age Group", 
       y = "Suicides per 100k Population") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 2. Time Trends by Generation
ggplot(data, aes(x = year, y = suicides_no, color = generation)) +
  geom_line(stat = "summary", fun = "sum") +
  labs(title = "Suicide Trends by Generation", 
       x = "Year", 
       y = "Number of Suicides") +
  theme_minimal() +
  facet_wrap(~generation, scales = "free_y")



# 3. Top Countries by Suicide Count
top_countries <- data %>%
  group_by(country) %>%
  summarise(total_suicides = sum(suicides_no)) %>%
  arrange(desc(total_suicides)) %>%
  head(10)

ggplot(top_countries, aes(x = reorder(country, -total_suicides), y = total_suicides)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Countries by Total Suicides", 
       x = "Country", 
       y = "Total Suicides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---------------------------------------------------------

# Check number of entries
cat("Total entries in the dataset:", nrow(data), "\n")

# Check for missing values
cat("Any missing values?", anyNA(data), "\n")

# Check for duplicate rows
cat("Number of duplicate rows:", sum(duplicated(data)), "\n")

# Basic outlier check using boxplot for numeric fields
boxplot(data$suicides_no, main = "Boxplot of Suicide Counts")

