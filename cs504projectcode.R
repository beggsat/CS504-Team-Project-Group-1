install.packages("dplyr")
install.packages("lubridate")
install.packages('tidyverse')

library(dplyr)
library(lubridate)
library(ggplot2)

employee_data <- read.csv("Extended_Employee_Performance_and_Productivity_Data.csv")
head(employee_data)
summary(employee_data)


data_num = employee_data[, sapply(employee_data, is.numeric)]
data_cor = cor(data_num)
data_cor[c("Employee_Satisfaction_Score", "Performance_Score"), ] 

ggplot(employee_data, aes(x = Performance_Score)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Performance Score",
       x = "Performance Score",
       y = "Count") +
  theme_minimal()

ggplot(employee_data, aes(x = Employee_Satisfaction_Score)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Employee Satisfaction Score",
       x = "Performance Score",
       y = "Count") +
  theme_minimal()


employee_data$Performance_Score_num <- as.numeric(as.character(employee_data$Performance_Score))

lmdata <- lm(Monthly_Salary ~ Performance_Score_num, data = employee_data)

plot(employee_data$Performance_Score_num, employee_data$Monthly_Salary,
     main = "Linear Regression: Monthly Salary vs. Performance Score",
     xlab = "Performance Score",
     ylab = "Monthly Salary",
     pch = 16)

abline(lmdata, col = "red", lwd = 2)


par(mfrow = c(2, 3))

# 1. Projects_Handled
lm_proj <- lm(Performance_Score ~ Projects_Handled, data = employee_data)
plot(employee_data$Projects_Handled, employee_data$Performance_Score,
     main = "Performance vs Projects Handled",
     xlab = "Projects Handled", ylab = "Performance Score", pch = 16)
abline(lm_proj, col = "red", lwd = 2)

# 2. Remote_Work_Frequency
lm_remote <- lm(Performance_Score ~ Remote_Work_Frequency, data = employee_data)
plot(employee_data$Remote_Work_Frequency, employee_data$Performance_Score,
     main = "Performance vs Remote Work Frequency",
     xlab = "Remote Work Frequency", ylab = "Performance Score", pch = 16)
abline(lm_remote, col = "red", lwd = 2)

# 3. Team_Size
lm_team <- lm(Performance_Score ~ Team_Size, data = employee_data)
plot(employee_data$Team_Size, employee_data$Performance_Score,
     main = "Performance vs Team Size",
     xlab = "Team Size", ylab = "Performance Score", pch = 16)
abline(lm_team, col = "red", lwd = 2)

# 4. Training_Hours
lm_train <- lm(Performance_Score ~ Training_Hours, data = employee_data)
plot(employee_data$Training_Hours, employee_data$Performance_Score,
     main = "Performance vs Training Hours",
     xlab = "Training Hours", ylab = "Performance Score", pch = 16)
abline(lm_train, col = "red", lwd = 2)

# 5. Overtime_Hours
lm_ot <- lm(Performance_Score ~ Overtime_Hours, data = employee_data)
plot(employee_data$Overtime_Hours, employee_data$Performance_Score,
     main = "Performance vs Overtime Hours",
     xlab = "Overtime Hours", ylab = "Performance Score", pch = 16)
abline(lm_ot, col = "red", lwd = 2)

# 6. Years_At_Company
lm_years <- lm(Performance_Score ~ Years_At_Company, data = employee_data)
plot(employee_data$Years_At_Company, employee_data$Performance_Score,
     main = "Performance vs Years at Company",
     xlab = "Years at Company", ylab = "Performance Score", pch = 16)
abline(lm_years, col = "red", lwd = 2)


# 1. Projects_Handled
lm_proj <- lm(Employee_Satisfaction_Score ~ Projects_Handled, data = employee_data)
plot(employee_data$Projects_Handled, employee_data$Employee_Satisfaction_Score,
     main = "Satisfaction vs Projects Handled",
     xlab = "Projects Handled", ylab = "Employee Satisfaction Score", pch = 16)
abline(lm_proj, col = "blue", lwd = 2)

# 2. Remote_Work_Frequency
lm_remote <- lm(Employee_Satisfaction_Score ~ Remote_Work_Frequency, data = employee_data)
plot(employee_data$Remote_Work_Frequency, employee_data$Employee_Satisfaction_Score,
     main = "Satisfaction vs Remote Work Frequency",
     xlab = "Remote Work Frequency", ylab = "Employee Satisfaction Score", pch = 16)
abline(lm_remote, col = "blue", lwd = 2)

# 3. Team_Size
lm_team <- lm(Employee_Satisfaction_Score ~ Team_Size, data = employee_data)
plot(employee_data$Team_Size, employee_data$Employee_Satisfaction_Score,
     main = "Satisfaction vs Team Size",
     xlab = "Team Size", ylab = "Employee Satisfaction Score", pch = 16)
abline(lm_team, col = "blue", lwd = 2)

# 4. Training_Hours
lm_train <- lm(Employee_Satisfaction_Score ~ Training_Hours, data = employee_data)
plot(employee_data$Training_Hours, employee_data$Employee_Satisfaction_Score,
     main = "Satisfaction vs Training Hours",
     xlab = "Training Hours", ylab = "Employee Satisfaction Score", pch = 16)
abline(lm_train, col = "blue", lwd = 2)

# 5. Overtime_Hours
lm_ot <- lm(Employee_Satisfaction_Score ~ Overtime_Hours, data = employee_data)
plot(employee_data$Overtime_Hours, employee_data$Employee_Satisfaction_Score,
     main = "Satisfaction vs Overtime Hours",
     xlab = "Overtime Hours", ylab = "Employee Satisfaction Score", pch = 16)
abline(lm_ot, col = "blue", lwd = 2)

# 6. Years_At_Company
lm_years <- lm(Employee_Satisfaction_Score ~ Years_At_Company, data = employee_data)
plot(employee_data$Years_At_Company, employee_data$Employee_Satisfaction_Score,
     main = "Satisfaction vs Years at Company",
     xlab = "Years at Company", ylab = "Employee Satisfaction Score", pch = 16)
abline(lm_years, col = "blue", lwd = 2)

library(MASS)
employee_data$Performance_Score <- factor(employee_data$Performance_Score, ordered = TRUE)

model <- polr(Performance_Score ~ Projects_Handled + Remote_Work_Frequency + Team_Size +
                Training_Hours + Overtime_Hours + Years_At_Company + Work_Hours_Per_Week +
                Years_At_Company + Work_Hours_Per_Week, 
              data = employee_data, 
              Hess = TRUE)
summary(model)
ctable <- coef(summary(model))
p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p_values)
print(ctable)


employee_data$Employee_Satisfaction_Score <- factor(employee_data$Employee_Satisfaction_Score, ordered = TRUE)

model <- polr(Employee_Satisfaction_Score ~ Projects_Handled + Remote_Work_Frequency + Team_Size +
                Training_Hours + Overtime_Hours + Years_At_Company + Work_Hours_Per_Week, 
              data = employee_data, 
              Hess = TRUE)
summary(model)
ctable <- coef(summary(model))
p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p_values)
print(ctable)