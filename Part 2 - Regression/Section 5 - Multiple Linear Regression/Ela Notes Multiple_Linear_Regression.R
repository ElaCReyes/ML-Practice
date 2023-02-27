####################################
### MULTIPLE LINEAR REGRESSION #####
####################################

# Task: Is there a correlation between any of the expenses (R&D, admin, marketing, state)
# and profit for these companies

# Load Data
# Importing the dataset
dataset_50_Startups = read.csv('50_Startups.csv')

View(dataset_50_Startups) # The amount of money spend in each category. 
# We want to predict the profit based on the independent variables. 
# Predict where to invest and which independent variable has the highest effect in profit. 

# Encode the categorical data - State variable
dataset_50_Startups$State <- factor(dataset_50_Startups$State, #variable
                           levels = c('New York', 'California', 'Florida'), #The names that we have
                           labels = c(1, 2, 3) #The character strings
)

# Splitting dataset
library(caTools)
set.seed(020923)
split_50_Startups <- sample.split(dataset_50_Startups$Profit, SplitRatio = 0.8)
training_set <-  subset(dataset_50_Startups, split_50_Startups == TRUE)
test_set <- subset(dataset_50_Startups, split_50_Startups == FALSE)

# We do not need to to feature scaling because of the model. 

##### Running the Multiple Regression in The Training Set #######

# Profit is a linear combination of the four independent variables
# The formula below is the long version  
# regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State)
# You can also use
regressor <- lm(formula = Profit ~ ., # The dot means all the independent variables 
                data = training_set) # Becasue we want to train first
summary(regressor)

# R creates the dummy variables and avoid the trap by removing one of the variables
# Interpretation: Since the R&D is the only that is significant we could simplify the 
# mode by only having R.D.Spend as indepdent variable. 

# Call:
#   lm(formula = Profit ~ ., data = training_set)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -32629  -5163    377   7404  18177 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      4.849e+04  8.716e+03   5.564 3.18e-06 ***
# R.D.Spend        8.129e-01  7.002e-02  11.609 2.24e-13 *** Ela: the profit is the only one that has the strong effect
# Administration  -2.099e-02  6.582e-02  -0.319    0.752    
# Marketing.Spend  2.494e-02  2.411e-02   1.035    0.308    
# State2           1.440e+02  4.192e+03   0.034    0.973    
# State3           1.910e+03  3.944e+03   0.484    0.631    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10290 on 34 degrees of freedom
# Multiple R-squared:  0.9414,	Adjusted R-squared:  0.9328 
# F-statistic: 109.3 on 5 and 34 DF,  p-value: < 2.2e-16

##### Predicting the Test Set Results ######
y_predict <- predict(regressor, newdata = test_set)
y_predict
# If I were to add only the the R.D.Spend I'll have the same performance because it was the only significant. 

###### Homework - Backward Elimination #######
# Predict the Optimal Model Using Backward Elimination
regressor_back_elim <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
                    data = dataset_50_Startups) #The whole dataset to have more information

summary(regressor_back_elim)

##### Step 1 ####
# Call:
#   lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + 
#        State, data = dataset_50_Startups)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -33504  -4736     90   6672  17338 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      5.008e+04  6.953e+03   7.204 5.76e-09 ***
# R.D.Spend        8.060e-01  4.641e-02  17.369  < 2e-16 ***
# Administration  -2.700e-02  5.223e-02  -0.517    0.608    
# Marketing.Spend  2.698e-02  1.714e-02   1.574    0.123    
# State2           4.189e+01  3.256e+03   0.013    0.990    Ela: Highest p-value 
# State3           2.407e+02  3.339e+03   0.072    0.943    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 9439 on 44 degrees of freedom
# Multiple R-squared:  0.9508,	Adjusted R-squared:  0.9452 
# F-statistic: 169.9 on 5 and 44 DF,  p-value: < 2.2e-16

regressor_back_elim_noState2 <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, # Removed State variable
                          data = dataset_50_Startups) #The whole dataset to have more information

summary(regressor_back_elim_noState2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      5.012e+04  6.572e+03   7.626 1.06e-09 ***
#   R.D.Spend        8.057e-01  4.515e-02  17.846  < 2e-16 ***
#   Administration  -2.682e-02  5.103e-02  -0.526    0.602    
# Marketing.Spend  2.723e-02  1.645e-02   1.655    0.105 
#The the worst is Administration
regressor_back_elim_noState2Admin <- lm(formula = Profit ~ R.D.Spend + Marketing.Spend, # Removed State variable
                                   data = dataset_50_Startups) #The whole dataset to have more information

summary(regressor_back_elim_noState2Admin) # I would keep it 
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     4.698e+04  2.690e+03  17.464   <2e-16 ***
#   R.D.Spend       7.966e-01  4.135e-02  19.266   <2e-16 ***
#   Marketing.Spend 2.991e-02  1.552e-02   1.927     0.06 . 
# There's marginal in Marketing Spend 

# Function that Hadelin gave in the MOOC for Backward Elimiation
dataset = read.csv('50_Startups.csv')
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)) {
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
     j = which(coef(summary(regressor))[c(2:numVars, "Pr(>|t|)")] == maxVar)
     x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
#I could not make it work :(
backwardElimination(dataset, SL)

