library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(corrplot)
library(MASS)

data <- read.csv('employee_survey.csv')

# ---------------------------------------------------
# Cleaning Data
# ---------------------------------------------------

## Checking for null values
sum(is.na(data))

colSums(is.na(data))

## Checking for duplicates
duplicates <- data[duplicated(data),]

print(duplicates)

num_duplicates <- sum(duplicated(data))
print (num_duplicates)

# ---------------------------------------------------
# Summary of the dataset
# ---------------------------------------------------

summary(data)
names(data)
summary(data[, c("Age", "Experience", "WLB", "PhysicalActivityHours", "Workload", "SleepHours",
                 "Stress", "CommuteDistance" ,"NumCompanies", "TeamSize", "NumReports","haveOT",
                 "TrainingHoursPerYear")])

# ---------------------------------------------------
# Ordinal logistic regression
# ---------------------------------------------------

# Fit ordinal logistic regression
model <- polr(as.factor(JobSatisfaction) ~ Age + JobLevel + 
                Dept + WLB + WorkEnv + 
                Workload + Stress + SleepHours + 
                NumCompanies + TeamSize + EduLevel + haveOT +
                TrainingHoursPerYear, data = data, Hess = TRUE)

# Summary of model
summary(model)

# p-values
ctable <- coef(summary(model))
p_vals <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p_vals)
ctable


# ---------------------------------------------------
# 1. Frequency Distribution of Job Satisfaction
# ---------------------------------------------------
cat("Frequency of Job Satisfaction scores:\n")
print(table(data$JobSatisfaction))
cat("Proportion of each Job Satisfaction score:\n")
print(prop.table(table(data$JobSatisfaction)))

job_satisfaction <- table(data$JobSatisfaction)
barplot(job_satisfaction,
        beside = TRUE,
        col = c("lightblue", "lightgreen", "gold", "orange", "tomato"),
        legend = rownames(gender_satisfaction),
        main = "Job Satisfaction Frequency",
        xlab = "Job Satisfaction",
        ylab = "Count")


# ---------------------------------------------------
# 9. Spearman Correlation Heatmap for Numeric Vars
# ---------------------------------------------------

numeric_vars <- data[sapply(data, is.numeric)]
cor_matrix <- cor(numeric_vars, method = "spearman")
corrplot(cor_matrix, method = "color", tl.cex = 0.8)
title("Spearman Correlation Matrix", line = 2)