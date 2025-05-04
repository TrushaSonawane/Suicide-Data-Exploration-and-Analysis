# Author: Trusha S
# Research Question: Can we predict whether a country's suicide trend is increasing or decreasing over time based on socio-economic and demographic indicators?
# Algorithm: Decision Tree & K-Means Clustering

# Clear environment and console
cat("\014")
rm(list = ls())
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE)
options(scipen = 100)

# Load required libraries
library(pacman)
p_load(tidyverse, rpart, rpart.plot, caret, dplyr, cluster, factoextra)

# Read the data
data <- read.csv("master.csv")

# Drop HDI column and clean missing values
data <- data %>% select(-hdi)
data <- na.omit(data)

# Create a country-year identifier
data <- data %>% mutate(country_year = paste(country, year, sep = "_"))

# Calculate suicide trend by country over time
data <- data %>% arrange(country, year)
data <- data %>% group_by(country) %>% mutate(rate_diff = suicides.100k - lag(suicides.100k))

# Create binary target: Increase vs Decrease/No change
data <- data %>% mutate(trend_direction = case_when(
  rate_diff > 0 ~ "Increase",
  TRUE ~ "NotIncrease"
))

# Remove rows with NA in new column (first year per country)
data <- data %>% filter(!is.na(trend_direction))

# Encode categorical variables
data$sex <- as.factor(data$sex)
data$age <- as.factor(data$age)
data$generation <- as.factor(data$generation)
data$trend_direction <- as.factor(data$trend_direction)

# Select features for modeling
model_data <- data %>% select(suicides.100k, gdppercapita, population, sex, age, generation, trend_direction)

# Train-test split
set.seed(123)
train_index <- createDataPartition(model_data$trend_direction, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Train decision tree model
tree_model <- rpart(trend_direction ~ ., data = train_data, method = "class", cp = 0.01)

# Plot the decision tree
rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE, main = "Decision Tree: Suicide Trend Direction")

# Predict and evaluate
predictions <- predict(tree_model, test_data, type = "class")
conf_matrix <- confusionMatrix(predictions, test_data$trend_direction)
print(conf_matrix)

# --- RANDOM FOREST SECTION --- #

# Load the package
p_load(randomForest)

# Train the Random Forest model
set.seed(123)
rf_model <- randomForest(trend_direction ~ ., data = train_data, importance = TRUE, ntree = 500)

# Predict on test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate model
rf_conf_matrix <- confusionMatrix(rf_predictions, test_data$trend_direction)
print(rf_conf_matrix)

# Plot variable importance
varImpPlot(rf_model, main = "Random Forest - Variable Importance")