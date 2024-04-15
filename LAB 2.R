# Loading the required libraries
library(tidyverse)
library(caret)
library(glmnet)

# Setting of the work directory
setwd("C:/Users/peddo/Downloads/")

# Reading the loaded datasets
students <- read.csv("students.csv")
assessments <- read.csv("assessments.csv")

# Merging the datasets students and assessments
merge_data <- merge(students, assessments, by = c("id_student", "code_module", "code_presentation"))

# Removing the rows with missing or null values
merge_data <- merge_data %>% drop_na()

# Convert date columns to Date type
date_columns <- c("date", "date_submitted")
merge_data[date_columns] <- lapply(merge_data[date_columns], as.Date)

# Convertion of the categorical columns to factors
categorical_columns <- c("gender", "region", "highest_education", "imd_band", "age_band", "final_result", "assessment_type")
merge_data[categorical_columns] <- lapply(merge_data[categorical_columns], as.factor)

# Classification Model (predicting final_result)
# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(merge_data$final_result, p = 0.7, list = FALSE)
train_data <- merge_data[trainIndex, ]
test_data <- merge_data[-trainIndex, ]

# Train a classification model (using logistic regression as an example)
clf_formula <- final_result ~ . - id_student - date - date_registration - date_unregistration
clf_model <- glm(classification_formula, data = train_data, family = binomial)

# Prediction on test data
predictions_clf <- predict(clf_model, newdata = test_data, type = "response")

# reg Model (predicting score)
# Train a regression model
reg_formula <- score ~ . - id_student - date - date_registration - date_unregistration
reg_model <- glm(reg_formula, data = train_data)

# Prediction on test data
predictions_reg <- predict(reg_model, newdata = test_data)

# Classification model summary
summary(clf_model)

#Tidy up the classification Summary
tidy(clf_model)

# regression model summary
summary(reg_model)

#Tidy up the regression Summary
tidy(reg_model)

#Glance the regression summary
glance(reg_model)

