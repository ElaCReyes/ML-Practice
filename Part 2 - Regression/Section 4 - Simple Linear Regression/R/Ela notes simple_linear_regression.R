# Ela Notes: Simple Linear Regression


#### 0. Import Data #####
dataset_salary_data <- read.csv('Salary_Data.csv')

##### 0. Data Preprocessing #####
#Split data for training and test. 
# We have 30 observations so one could say 75% but in this case let's do two thirs
library(caTools) #To split
set.seed(020723)
#You need to specify the column in sample split to focus on the rows. Otherwise it would splity in columns.
split_salary_data <- sample.split(dataset_salary_data$Salary, SplitRatio = 2/3)
training_set_salary <- subset(dataset_salary_data, split_salary_data == TRUE)
test_set_salary <- subset(dataset_salary_data, split_salary_data == FALSE)

# We do not need feature scaling because the R lm takes care of it. 

# SIMPLE LINEAR REGRESSION ##### 
regressor <- lm(formula = Salary ~ YearsExperience, #Salary is proportional to Exp. Exp being independent
                data = training_set_salary)
summary(regressor)
#ELA: Strong linear correlation
# Call:
#   lm(formula = Salary ~ YearsExperience, data = training_set_salary)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6755.1 -4046.8  -194.2  3293.1  8823.1 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      25270.5     2577.3   9.805 1.21e-08 *** ELA: Highly stats significance
#   YearsExperience   9316.4      462.8  20.132 8.60e-14 *** ELA: Highly stats significance
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5202 on 18 degrees of freedom
# Multiple R-squared:  0.9575,	Adjusted R-squared:  0.9551 
# F-statistic: 405.3 on 1 and 18 DF,  p-value: 8.596e-14

#ELA: When you have a lot of variables used the Adjusted R-squared. 


###### 2. Predicting the Test set results ######
# We use the predict() function that needs two arguments, the regressor (results of the SLR) and the second is the new data (the training set)
# Uses time series prediction methods. 
# Given that we say a strong linear relation between Exp and Salary we are expecting to be a good fit.
y_pred <- predict(regressor, newdata = test_set_salary)
Viview(y_pred) # shows the predicted salaries for those in the test set
# For example. Employee 4 predicted salary was 43903.33 and the actual was 43525

###### 3. Predicted vs Actual #####
# library(ggplot2)

ggplot() +
  geom_point(aes(x = training_set_salary$YearsExperience, y = training_set_salary$Salary),
                 colour = 'red') + #Real salaries
             geom_line(aes(x = training_set_salary$YearsExperience, y = predict(regressor, newdata = training_set_salary)), 
                        colour = 'blue') + #My regression model
            ggtitle('Salary vs Experience (Training Set)') +
            xlab('Years of Expereince') +
            ylab('Salary')




