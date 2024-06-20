# doing the random forest on the people who used both revolving credit and
# a fixed repayment scheme gives a LOT of false negatives.
library(data.table)
library(rpart)
library(randomForest)
library(rpart.plot)
library(ggplot2)
library(caret)

dt <- fread("cleaned_data.csv")
dt <- dt[dt$Credit > 499999]

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
ggsave(filename="plots/11b - confusion matrix - random forest categorical - credit level 500k and above.png")
# this test, even with low accuracy, is actually good.
# simply saying that no one will default makes you right 89% of the time,
# but this model is right 88% of the time.
results$error <- xor(as.numeric(results$Defaulted), as.numeric(results$Prediction))
accuracy
1-mean(dt$Defaulted)
accuracy - (1-mean(dt$Defaulted))
# so this model sucks






dt <- fread("cleaned_data.csv")
dt <- dt[dt$Credit < 500000]

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
ggsave(filename="plots/11d - confusion matrix - random forest categorical - credit level below 500k.png")
# this test, even with low accuracy, is actually good.
# simply saying that no one will default makes you right 78% of the time,
# but this model is right 82% of the time.
results$error <- xor(as.numeric(results$Defaulted), as.numeric(results$Prediction))
accuracy
1-mean(dt$Defaulted)
accuracy - (1-mean(dt$Defaulted))








