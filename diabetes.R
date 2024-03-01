# Let's install below packages, if not please install them first.
install.packages('tidyverse')
install.packages('knitr')
library(tidyverse)
library(knitr)
library(ggplot2)

library(readxl)

diabetes_dataset <- read.csv("AI Foundations/Assignemnt/diabetes_prediction_dataset.csv")
View(diabetes_dataset)
attach(diabetes_dataset)



#What is the correlation between diabetes (Dependent variable) and independent variables? Is this expected or surprising?
cor(diabetes_dataset$bmi,diabetes_dataset$diabetes)
# Fit a linear regression model
model <- lm(diabetes~., data = diabetes_dataset)

# Summary of the model
summary(model)


# Scatterplot matrix for continuous variables
#pairs(diabetes_dataset[c("age", "bmi", "HbA1c_level", "blood_glucose_level")])

# Bar plot for categorical variables
par(mfrow=c(2, 2))  # Set the layout for multiple plots

barplot(table(diabetes_dataset$gender), main = "Gender")  # Replace 'gender' with your gender variable
barplot(table(diabetes_dataset$hypertension), main = "Hypertension")  # Replace 'hypertension' with your hypertension variable
barplot(table(diabetes_dataset$smoking_history), main = "Smoking History")  # Replace 'smoking_history' with your smoking variable
barplot(table(diabetes_dataset$heart_disease), main = "Heart Disease")  # Replace 'heart_disease' with your heart disease variable


# Boxplot for continuous variables across categories
par(mfrow=c(2, 2))  # Set the layout for multiple plots

boxplot(age ~ hypertension, data = diabetes_dataset, main = "Age vs. Hypertension", xlab = "Hypertension", ylab = "Age")
boxplot(blood_glucose_level ~ gender, data = diabetes_dataset, main = "blood_glucose_level  vs. Gender", xlab = "Gender", ylab = "blood_glucose_level")
#boxplot(heart_disease ~ smoking_history, data = diabetes_dataset, main = "heart_disease vs. Smoking History", xlab = "Smoking History", ylab = "heart disease")
boxplot(blood_glucose_level ~ heart_disease, data = diabetes_dataset, main = "Blood Glucose vs. Heart Disease", xlab = "Heart Disease", ylab = "Blood Glucose Level")
boxplot(HbA1c_level ~ diabetes, data = diabetes_dataset, main = "HbA1c_level vs. diabetes", xlab = "HbA1c level", ylab = "diabetes")


# Scatterplot for age vs. bmi
plot(diabetes_dataset$age, diabetes_dataset$bmi, xlab = "Age", ylab = "BMI", main = "Scatterplot: Age vs. BMI")

# Scatterplot for age vs. HbA1c_level
plot(diabetes_dataset$age, diabetes_dataset$HbA1c_level, xlab = "Age", ylab = "HbA1c Level", main = "Scatterplot: Age vs. HbA1c Level")

# Scatterplot for age vs. blood_glucose_level
plot(diabetes_dataset$age, diabetes_dataset$blood_glucose_level, xlab = "Age", ylab = "Blood Glucose Level", main = "Scatterplot: Age vs. Blood Glucose Level")

# Scatterplot for bmi vs. HbA1c_level
plot(diabetes_dataset$bmi, diabetes_dataset$HbA1c_level, xlab = "BMI", ylab = "HbA1c Level", main = "Scatterplot: BMI vs. HbA1c Level")

install.packages("MASS")
library(MASS)

# Convert 'gender' to dummy variables (if it's a categorical variable)
diabetes_dataset <- cbind(diabetes_dataset, model.matrix(~ gender - 1, data = diabetes_dataset))

# Select specific numeric columns
selected_columns <- diabetes_dataset[c("age", "bmi", "HbA1c_level", "blood_glucose_level","hypertension", "heart_disease","diabetes", 'genderFemale', 'genderMale', "genderOther")]


# Compute correlation matrix for selected columns including 'gender_factor'
correlation_matrix <- cor(selected_columns)


# Print the correlation matrix
print(correlation_matrix)

