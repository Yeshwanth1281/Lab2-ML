# Ensure the 'studentS.csv' file is in the current working directory. Check with getwd() and change with setwd() as needed.

# Loading the student data using base R read.csv function
students <- read.csv(r"(C:\Users\peddo\OneDrive\Desktop\Lab2\Students.csv)")

#Display the dataset.
students

# Data preprocessing: Transform 'final_result' into a binary variable and 'disability' into a factor
students$is_passed <- as.factor(ifelse(students$final_result == "Pass", 1, 0))
students$credits <- as.factor(students$studied_credits)


# Convert 'imd_band' to a numeric scale based on given categories
imd_scale <- c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
students$imd_numeric <- as.numeric(factor(students$imd_band, levels = imd_scale))

# Creating train and test sets manually
set.seed(20230712)  # Setting seed for reproducibility
sample_count <- floor(0.8 * nrow(students)) #Splitting the data into 80% as train data and 20% as test data.
training <- sample(seq_len(nrow(students)), size = sample_count)

train_data <- students[training, ]
test_data <- students[-training, ]

# Building a logistic regression model with glm (Generalized Linear Model) in base R
logit_model <- glm(is_passed ~ credits + imd_numeric, family = binomial(link = "logit"), data = train_data)

# Model summary display.
summary(logit_model)

#Tidying the model 
library(tidymodels)
tidy(logit_model)

#Plotting the model.
plot(logit_model)

# Prediction on test data
test_predictions <- predict(logit_model, test_data, type = "response")
predicted_outcome <- ifelse(test_predictions > 0.5, 1, 0)

# Accuracy calculation
true_outcomes <- as.numeric(test_data$is_passed) - 1  # Adjusting factor levels from 1 to 0 and 1 for comparison
model_accuracy <- mean(predicted_outcome == true_outcomes)
print(paste("Model Accuracy:", model_accuracy))

# This script uses only base R functions and does not rely on additional libraries like tidyverse or tidymodels.

