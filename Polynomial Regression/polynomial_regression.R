#Polynomial Regression
# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]
# Splitting the dataset into the Training set and Test set: In this case splitting
# is not required because we don't have much data to train our model
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

#Fitting the Linear Regression to our dataset
lin_reg = lm(formula = Salary ~ .,
             data = dataset)
#Fitting the Polynomial Regression to our dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ .,
              data = dataset)
#Visualising the Linear Regression results
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             color = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            color = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('Level') +
  ylab('Salary')
#Visalising the Polynomial Regression Results
x_grid = seq(min(dataset$Level),max(dataset$Level),0.1)
dataset2 = data.frame(Level = x_grid, Level2 = x_grid^2, Level3 = x_grid^3,
                      Level4 = x_grid^4)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             color = 'red') +
  geom_line(aes(x = x_grid, y = predict(poly_reg, newdata = dataset2)),
            color = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')
#Predicting a new result with Linear Regression
y_pred = predict(lin_reg,data.frame(Level=6.5))
#Predicting a new result with Polynomial Regression
y_pred = predict(poly_reg,data.frame(Level = 6.5,
                                     Level2 = 6.5^2,
                                     Level3 = 6.5^3,
                                     Level4 = 6.5^4))