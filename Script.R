# Load required libraries
library(rpart)
library(rpart.plot)

# Set the working directory
setwd("C:/Users/Kapad/OneDrive/Desktop/Rutgers/R Related/Project2")


data <- read.csv("onlinefoods.csv")
summary(data)

#Numerical variables - Age, Family.size, latitude, longitude, Pin.code
#Character variable - Gender, Martial.Status, Occupation, Monthly.Income, EEducational.Qualifications
#Output, Feedback, X

#combining all the variables

num_var <- cbind.data.frame(data)
summary(data)

# Summary statistics for numerical variables
summary(data[, c("Age", "Family.size", "latitude", "longitude", "Pin.code")])

# Train/test split (assuming 70% train and 30% test)
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

minBuck <-16
minS <- 15
cup <- 0.01
dep <- 4

# Train the rpart model
model <- rpart(Feedback ~ Age + Family.size + latitude + longitude + Gender + 
               Marital.Status + Occupation + Monthly.Income + Output +
               Educational.Qualifications, 
               data = train_data, method = "class", control = rpart.control(minsplit = minS, minbucket = minBuck, cp = cup, maxdepth = dep))

# Predict on test data
predictions <- predict(model, newdata = test_data, type = "class")

# Confusion matrix
conf_matrix <- table(predictions, test_data$Output)
print(conf_matrix)

# Load required libraries
library(e1071)  # for Naive Bayes classification

# Train Naive Bayes model
nb_model <- naiveBayes(Feedback ~ Output + Age + Family.size + latitude + longitude + Gender + Occupation + Monthly.Income + Educational.Qualifications, data = train_data)

# Predict on training data
nb_predictions <- predict(nb_model, newdata = test_data)

# Confusion matrix for Naive Bayes
nb_conf_matrix <- table(nb_predictions, test_data$Output)
print("Confusion Matrix for Naive Bayes:")
print(nb_conf_matrix)

# Calculate accuracy for Naive Bayes
nb_accuracy <- sum(diag(nb_conf_matrix)) / sum(nb_conf_matrix)
print(paste("Accuracy of Naive Bayes:", nb_accuracy))

# Compare with rpart accuracy
rpart_accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy of rpart:", rpart_accuracy))
