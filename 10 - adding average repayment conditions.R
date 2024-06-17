# doing the random forest on the people who used both revolving credit and
# a fixed repayment scheme gives a LOT of false negatives.
library(data.table)
library(rpart)
library(randomForest)
library(rpart.plot)
library(ggplot2)
library(caret)

dt <- fread("cleaned_data.csv")
dt <- dt[dt$Num_Revolving_Credit %in% c(1,2,3,4,5)]

#dcast(dt, Num_Revolving_Credit~., mean, value.var = "Defaulted")
#dcast(dt, Num_Revolving_Credit~., length, value.var = "Defaulted")
#dt$Num_No_Credit_Consumption

#d <- dcast(dt, Credit~., mean, value.var = "Defaulted")
#plot(d)
# did they ever go over their credit limit
# split into groups based on how many times they used revolving credit
# split into groups based on above or below 500k


#plot(dt$Account_Balance_Jan~dt$Defaulted)
# do they start with one type of repayment and then switch

# setting a seed
set.seed(234) # for reproducibility

# we are going to split the data into testing data and training data
train = sample(1:nrow(dt), as.integer(nrow(dt)/3))
training_dt=dt[train,]
testing_dt=dt[-train,]
# Load your dataset or create a sample dataset
# For this example, let's create a sample dataset

# Convert the data.table to a data.frame as required by randomForest
training_df <- as.data.frame(training_dt)

# Build a random forest model with 10 trees
random_forest_model <- randomForest(as.factor(Defaulted) ~ ., data = training_df, ntree = 100)

# Print the random forest model
print(random_forest_model)

# Plot the error rate as a function of the number of trees
plot(random_forest_model)


# Make predictions on the testing dataset
predictions <- predict(random_forest_model, testing_dt)

# Compare predictions to actual target values
actuals <- testing_dt$Defaulted

# Create a confusion matrix
confusion_matrix <- table(Predicted = predictions, Actual = actuals)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(predictions == actuals) / length(actuals)
print(paste("Accuracy:", accuracy))

# Calculate other metrics (optional)
conf_matrix <- table(actuals, predictions)

# Convert the confusion matrix to a data.frame for ggplot2
conf_matrix_df <- as.data.frame(conf_matrix)
colnames(conf_matrix_df) <- c("default", "prediction", "count")
conf_matrix_df

# Plot the confusion matrix using ggplot2
ggplot(conf_matrix_df, aes(x = prediction, y = default)) +
  geom_tile(aes(fill = count), color = "white") +
  scale_fill_steps(n.breaks = 6, low="red", high="blue") +
  geom_text(aes(label = count), vjust = 1) +
  labs(title = "Confusion Matrix", x = "Prediction", y = "Actual") +
  theme_minimal()
ggsave(filename="plot random forest categorical with some revolving credit.png")
# this test, even with low accuracy, is actually good.
# simply saying that no one will default makes you right 72% of the time,
# but this model is right 77% of the time.
accuracy - (1-mean(dt$Defaulted))








# doing just people WITHOUT any revolving credit
dt <- fread("cleaned_data.csv")
dt <- dt[dt$Num_Revolving_Credit ==0]

#dcast(dt, Num_Revolving_Credit~., mean, value.var = "Defaulted")
#dcast(dt, Num_Revolving_Credit~., length, value.var = "Defaulted")
#dt$Num_No_Credit_Consumption

#d <- dcast(dt, Credit~., mean, value.var = "Defaulted")
#plot(d)
# did they ever go over their credit limit
# split into groups based on how many times they used revolving credit
# split into groups based on above or below 500k


#plot(dt$Account_Balance_Jan~dt$Defaulted)
# do they start with one type of repayment and then switch

# setting a seed
set.seed(234) # for reproducibility

# we are going to split the data into testing data and training data
train = sample(1:nrow(dt), as.integer(nrow(dt)/3))
training_dt=dt[train,]
testing_dt=dt[-train,]
# Load your dataset or create a sample dataset
# For this example, let's create a sample dataset

# Convert the data.table to a data.frame as required by randomForest
training_df <- as.data.frame(training_dt)

# Build a random forest model with 10 trees
random_forest_model <- randomForest(as.factor(Defaulted) ~ ., data = training_df, ntree = 100)

# Print the random forest model
print(random_forest_model)

# Plot the error rate as a function of the number of trees
plot(random_forest_model)


# Make predictions on the testing dataset
predictions <- predict(random_forest_model, testing_dt)

# Compare predictions to actual target values
actuals <- testing_dt$Defaulted

# Create a confusion matrix
confusion_matrix <- table(Predicted = predictions, Actual = actuals)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(predictions == actuals) / length(actuals)
print(paste("Accuracy:", accuracy))

# Calculate other metrics (optional)
conf_matrix <- table(actuals, predictions)

# Convert the confusion matrix to a data.frame for ggplot2
conf_matrix_df <- as.data.frame(conf_matrix)
colnames(conf_matrix_df) <- c("default", "prediction", "count")
conf_matrix_df

# Plot the confusion matrix using ggplot2
ggplot(conf_matrix_df, aes(x = prediction, y = default)) +
  geom_tile(aes(fill = count), color = "white") +
  scale_fill_steps(n.breaks = 6, low="red", high="blue") +
  geom_text(aes(label = count), vjust = 1) +
  labs(title = "Confusion Matrix", x = "Prediction", y = "Actual") +
  theme_minimal()
ggsave(filename="plot random forest categorical with no revolving credit.png")
# this test, even with low accuracy, is actually good.
# simply saying that no one will default makes you right 73% of the time,
# but this model is right 79% of the time.
accuracy - (1-mean(dt$Defaulted))






# doing people who ONLY had revolving credit
dt <- fread("cleaned_data.csv")
dt <- dt[dt$Num_Revolving_Credit == 6]

#dcast(dt, Num_Revolving_Credit~., mean, value.var = "Defaulted")
#dcast(dt, Num_Revolving_Credit~., length, value.var = "Defaulted")
#dt$Num_No_Credit_Consumption

#d <- dcast(dt, Credit~., mean, value.var = "Defaulted")
#plot(d)
# did they ever go over their credit limit
# split into groups based on how many times they used revolving credit
# split into groups based on above or below 500k


#plot(dt$Account_Balance_Jan~dt$Defaulted)
# do they start with one type of repayment and then switch

# setting a seed
set.seed(234) # for reproducibility

# we are going to split the data into testing data and training data
train = sample(1:nrow(dt), as.integer(nrow(dt)/3))
training_dt=dt[train,]
testing_dt=dt[-train,]
# Load your dataset or create a sample dataset
# For this example, let's create a sample dataset

# Convert the data.table to a data.frame as required by randomForest
training_df <- as.data.frame(training_dt)

# Build a random forest model with 10 trees
random_forest_model <- randomForest(as.factor(Defaulted) ~ ., data = training_df, ntree = 100)

# Print the random forest model
print(random_forest_model)

# Plot the error rate as a function of the number of trees
plot(random_forest_model)


# Make predictions on the testing dataset
predictions <- predict(random_forest_model, testing_dt)

# Compare predictions to actual target values
actuals <- testing_dt$Defaulted

# Create a confusion matrix
confusion_matrix <- table(Predicted = predictions, Actual = actuals)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(predictions == actuals) / length(actuals)
print(paste("Accuracy:", accuracy))

# Calculate other metrics (optional)
conf_matrix <- table(actuals, predictions)

# Convert the confusion matrix to a data.frame for ggplot2
conf_matrix_df <- as.data.frame(conf_matrix)
colnames(conf_matrix_df) <- c("default", "prediction", "count")
conf_matrix_df

# Plot the confusion matrix using ggplot2
ggplot(conf_matrix_df, aes(x = prediction, y = default)) +
  geom_tile(aes(fill = count), color = "white") +
  scale_fill_steps(n.breaks = 6, low="red", high="blue") +
  geom_text(aes(label = count), vjust = 1) +
  labs(title = "Confusion Matrix", x = "Prediction", y = "Actual") +
  theme_minimal()
ggsave(filename="plot random forest categorical with only revolving credit.png")


# but it seems that the accuracy of this test is not any better just saying
# no one will default at all.
# it's right 90% of the time but the chance of defaulting is 90%.
accuracy - (1-mean(dt$Defaulted))
