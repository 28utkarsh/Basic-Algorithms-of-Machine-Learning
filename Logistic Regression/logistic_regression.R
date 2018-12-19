#Logistic Regression

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[,3:5]
# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[, 1:2] = scale(training_set[,1:2])
test_set[, 1:2] = scale(test_set[, 1:2])

#Fitting Logistic Regression to the Training Set
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

#Predicting the test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set)
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#Making the Confusion Matrix
cm = table(test_set[,3],y_pred)

#Visualising the Training set results
install.packages('ElemStatLearn')
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
X2 = seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob