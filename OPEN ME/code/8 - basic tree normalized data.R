library(data.table)
library(rpart)
library(rpart.plot)
library(ggplot2)

dt <- fread("clean_data_normalized.csv")

dt

# setting a seed
set.seed(234) # for reproducibility

# we are going to split the data into testing data and training data
train = sample(1:nrow(dt), as.integer(nrow(dt)/3))
train
training_dt=dt[train,]
testing_dt=dt[-train,]


################################################################################
#                             TREE 1
################################################################################
# let's get a model and see what it comes up with
fit.tree = rpart(Defaulted ~ ., data=training_dt, method = "class", cp=0.008)
rpart.plot(fit.tree)
# wow. that sucks lol
# it simply looks at one variable lol
# let's see the performance
pred.tree = predict(fit.tree, testing_dt, type = "class")
testing_dt$Prediction <- pred.tree

results <- testing_dt[,c("Defaulted","Prediction")]
results$error <- as.numeric(results$Prediction) - as.numeric(results$Defaulted)

# Calculate the confusion matrix
conf_matrix <- table(results$Defaulted, results$Prediction)

# Convert the confusion matrix to a data.frame for ggplot2
conf_matrix_df <- as.data.frame(conf_matrix)
colnames(conf_matrix_df) <- c("Defaulted", "prediction", "count")
conf_matrix_df

# Plot the confusion matrix using ggplot2
ggplot(conf_matrix_df, aes(x = prediction, y = Defaulted)) +
  geom_tile(aes(fill = count), color = "white") +
  scale_fill_steps(n.breaks = 6, low="red", high="blue") +
  geom_text(aes(label = count), vjust = 1) +
  labs(title = "Confusion Matrix", x = "Prediction", y = "Actual") +
  theme_minimal()
ggsave(filename="plots/8b - plot tree 1 normalized data.png")
# ok, so it's really likely to give a false negative. it has more false negatives
# than it has true positives.





################################################################################
#                                TREE 2
################################################################################
# let's try with a different cp
fit.tree2 = rpart(Defaulted ~ ., data=training_dt, method = "class", cp=0.001)
rpart.plot(fit.tree2)
# now that is a lot of branches

pred.tree2 = predict(fit.tree2, testing_dt, type = "class")
testing_dt$Prediction <- pred.tree2
results <- testing_dt[,c("Defaulted","Prediction")]

# Calculate the confusion matrix
conf_matrix <- table(results$Defaulted, results$Prediction)

# Convert the confusion matrix to a data.frame for ggplot2
conf_matrix_df <- as.data.frame(conf_matrix)
colnames(conf_matrix_df) <- c("Defaulted", "prediction", "count")

# Plot the confusion matrix using ggplot2
ggplot(conf_matrix_df, aes(x = prediction, y = Defaulted)) +
  geom_tile(aes(fill = count), color = "white") +
  scale_fill_steps(n.breaks = 6, low="red", high="blue") +
  geom_text(aes(label = count), vjust = 1) +
  labs(title = "Confusion Matrix", x = "Prediction", y = "Actual") +
  theme_minimal()
ggsave(filename="plot tree 2normalized data.png")
# ok so this one is even worse

################################################################################
#                                 TREE 3
################################################################################
# let's try another cp
fit.tree3 = rpart(Defaulted ~ ., data=training_dt, method = "class", cp=0.004)
rpart.plot(fit.tree3)
# now that is a lot of branches

pred.tree3 = predict(fit.tree3, testing_dt, type = "class")
testing_dt$Prediction <- pred.tree3
results <- testing_dt[,c("Defaulted","Prediction")]

# Calculate the confusion matrix
conf_matrix <- table(results$Defaulted, results$Prediction)

# Convert the confusion matrix to a data.frame for ggplot2
conf_matrix_df <- as.data.frame(conf_matrix)
colnames(conf_matrix_df) <- c("Defaulted", "prediction", "count")

# Plot the confusion matrix using ggplot2
ggplot(conf_matrix_df, aes(x = prediction, y = Defaulted)) +
  geom_tile(aes(fill = count), color = "white") +
  scale_fill_steps(n.breaks = 6, low="red", high="blue") +
  geom_text(aes(label = count), vjust = 1) +
  labs(title = "Confusion Matrix", x = "Prediction", y = "Actual") +
  theme_minimal()
ggsave(filename="plot tree 3normalized data.png")
# really similar to TREE 2




################################################################################
#                          PRUNING THE TREES
################################################################################
# Explicitly request the lowest cp value and make a pruned tree
fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"]
bestcp <-fit.tree$cptable[which.min(fit.tree$cptable[,"xerror"]),"CP"]
pruned.tree <- prune(fit.tree, cp = bestcp)
rpart.plot(pruned.tree)
# Explicitly request the lowest cp value and make a pruned tree
fit.tree2$cptable[which.min(fit.tree2$cptable[,"xerror"]),"CP"]
bestcp <-fit.tree2$cptable[which.min(fit.tree2$cptable[,"xerror"]),"CP"]
pruned.tree2 <- prune(fit.tree2, cp = bestcp)
rpart.plot(pruned.tree2)
# Explicitly request the lowest cp value and make a pruned tree
fit.tree3$cptable[which.min(fit.tree3$cptable[,"xerror"]),"CP"]
bestcp <-fit.tree3$cptable[which.min(fit.tree3$cptable[,"xerror"]),"CP"]
pruned.tree3 <- prune(fit.tree3, cp = bestcp)
rpart.plot(pruned.tree3)


################################################################################
#                   PREDICTIONS WITH PRUNED TREES
################################################################################
# let's see what the predictions are like with the pruned trees
pred.tree2 = predict(pruned.tree2, testing_dt, type = "class")
testing_dt$Prediction <- pred.tree2
results <- testing_dt[,c("Defaulted","Prediction")]

# Calculate the confusion matrix
conf_matrix <- table(results$Defaulted, results$Prediction)

# Convert the confusion matrix to a data.frame for ggplot2
conf_matrix_df <- as.data.frame(conf_matrix)
colnames(conf_matrix_df) <- c("Defaulted", "prediction", "count")

# Plot the confusion matrix using ggplot2
ggplot(conf_matrix_df, aes(x = prediction, y = Defaulted)) +
  geom_tile(aes(fill = count), color = "white") +
  scale_fill_steps(n.breaks = 6, low="red", high="blue") +
  geom_text(aes(label = count), vjust = 1) +
  labs(title = "Confusion Matrix", x = "Prediction", y = "Actual") +
  theme_minimal()
ggsave(filename="plot tree 2 prunednormalized data.png")
