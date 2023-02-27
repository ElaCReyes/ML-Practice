# Data Preprocessing 

# Load Data
dataset = read.csv(file = 'Data.csv')

# Taking care of missing data 
# Imputing the mean
dataset$Age = ifelse(is.na(dataset$Age), 
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)), #if it's TRUE. If it's missing
                     dataset$Age # If FALSE
                     )

dataset$Salary = ifelse(is.na(dataset$Salary), 
                     ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)), #if it's TRUE. If it's missing
                     dataset$Salary # If FALSE
)
dataset0 <- dataset

# Encode Categorical Data
# We have two categorical variables: Country and Purchased. 
# Encode is making the variables as numbers. 
# So we use the factor function to change from character to factor. Transform the column.
dataset0$Country <- factor(dataset0$Country, #variable
                          levels = c('France', 'Spain', 'Germany'), #The names that we have
                          labels = c(1, 2, 3) #The character strings
                          )

dataset0$Purchased <- factor(dataset0$Purchased,
                             levels = c('Yes', 'No'),
                             labels = c(1, 0)
                             )

# Splitting the dataset into Training set and Test set
# You do this to make sure there's no overfitting and high correlations 
# Also to test how good is your mode. You want the performance to be similar 
# between the Training set and Test set
# You need package caTools
# install.packages('caTools')
library(caTools) # to split dataset
# set seed
set.seed(020323)
split <- sample.split(dataset0$Purchased, #The dependent variable
                      SplitRatio = 0.8 #The percentage of observations for training set. 
                      #In Python you sue the percentage for the test set. Which is the opposite. 
                      )
# If TRUE measn that goes to training set. If FALSE means goes to test set. 
split
training_set = subset(dataset0, split == TRUE)
test_set = subset(dataset0, split == FALSE)

# Feature Scaling 
# Given that age and salary are not in the same scale
# ML modules are based on the Euclidean distance 
# The square coordinate. In this case Age, Salary
# Given that it's squared we will have very big numbers so we don't want numbers to dominate. 
# That's why we use the feature scaling = standardisation OR normalisation

# I have to specify the columns to scale because the others are class factor which is not allowed
# Also, I do not need to scale the factor!
# Not all libraries ask you to do the sscaling. This is just to know. 
training_set[, 2:3] <- scale(training_set[,2:3]) 
test_set[, 2:3] <- scale(training_set[,2:3])

#### In case of missing data #####


#### In case of categorical data #####
