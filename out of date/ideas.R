# scale the variables to their standard deviations
# scale some of them to undo the zipfian distribution
# scale some of them to undo the boltzman distribution
# remove all outliers

# Load required packages
library(data.table)
library(sads)
library(ggplot2)

# Example data creation for demonstration
set.seed(123)
dt <- data.table(value = sads::rzipf(n=100000, N=1000, s=1))  # Replace with your actual data
# Plot the original distribution
hist(dt$value, breaks = 200, main = "Original Zipfian Distribution", xlab = "Value")
hist((dt$value)**0.000000000001, main = "Original Zipfian Distribution", xlab = "Value")

ggplot(dt)+
  geom_density(aes(x=value), fill="red", alpha=0.2)+
  geom_density(aes(x=value**0.5), fill="blue", alpha=0.2)+
  geom_density(aes(x=value**0.25), fill="green", alpha=0.2)+
  geom_density(aes(x=log(value)), fill="yellow", alpha=0.2)+
  geom_density(aes(x=log10(value)), fill="purple", alpha=0.2)+
  xlim(c(0,50))+
  theme_bw()




# Example data creation for demonstration
set.seed(123)
dt <- fread("cleaned_data.csv")
dt$value <- dt$Credit
# Plot the original distribution
ggplot(dt)+
  geom_density(aes(x=value/10000), fill="red", alpha=0.2)+
  geom_density(aes(x=(value**0.5)/10), fill="blue", alpha=0.2)+
  geom_density(aes(x=value**0.25), fill="green", alpha=0.2)+
  geom_density(aes(x=log(value)), fill="yellow", alpha=0.2)+
  theme_bw()





# see which transformation brings us closest to the normal distribution
# Load required packages
# install.packages("data.table")
# install.packages("MASS")
library(data.table)
library(MASS)

# Example data creation for demonstration
set.seed(123)

# Apply Box-Cox transformation to find the optimal lambda
boxcox_result <- boxcox(dt$value ~ 1, lambda = seq(-5, 5, by = 0.1))

# Extract the optimal lambda
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]

# Function to apply the Box-Cox transformation
boxcox_transform <- function(x, lambda) {
  if (lambda == 0) {
    return(log(x))
  } else {
    return((x^lambda - 1) / lambda)
  }
}

# Apply the optimal lambda to transform the data
dt[, transformed_value := boxcox_transform(value, optimal_lambda)]
ggplot(dt)+
  geom_density(aes(x=value/10000), fill="red", alpha=0.2)+
  geom_density(aes(x=(value**0.5)/10), fill="blue", alpha=0.2)+
  geom_density(aes(x=(value**0.25)*5), fill="green", alpha=0.2)+
  geom_density(aes(x=log(value)*10), fill="yellow", alpha=0.2)+
  geom_density(aes(x=transformed_value), fill="purple", alpha=0.2)+
  theme_bw()


# let's transform all the numeric data
# first get rid of the placeholder variables for graph tresting
dt$value <- NULL
dt$transformed_value <- NULL

# Age
boxcox_result <- boxcox(dt$Age ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Age_t <- boxcox_transform(dt$Age, optimal_lambda)
# Credit
boxcox_result <- boxcox(dt$Credit ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Credit_t <- boxcox_transform(dt$Credit, optimal_lambda)
# Account_Balance_Jun
dt$Account_Balance_Jun_t <- dt$Account_Balance_Jun - min(dt$Account_Balance_Jun)+1
boxcox_result <- boxcox(dt$Account_Balance_Jun_t ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Account_Balance_Jun_t <- boxcox_transform(dt$Account_Balance_Jun_t, optimal_lambda)
# Account_Balance_May
dt$Account_Balance_May_t <- dt$Account_Balance_May - min(dt$Account_Balance_May)+1
boxcox_result <- boxcox(dt$Account_Balance_May_t ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Account_Balance_May_t <- boxcox_transform(dt$Account_Balance_May_t, optimal_lambda)
# Account_Balance_Apr
dt$Account_Balance_Apr_t <- dt$Account_Balance_Apr - min(dt$Account_Balance_Apr)+1
boxcox_result <- boxcox(dt$Account_Balance_Apr_t ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Account_Balance_Apr_t <- boxcox_transform(dt$Account_Balance_Apr_t, optimal_lambda)
# Account_Balance_Mar
dt$Account_Balance_Mar_t <- dt$Account_Balance_Mar - min(dt$Account_Balance_Mar)+1
boxcox_result <- boxcox(dt$Account_Balance_Mar_t ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Account_Balance_Mar_t <- boxcox_transform(dt$Account_Balance_Mar_t, optimal_lambda)
# Account_Balance_Feb
dt$Account_Balance_Feb_t <- dt$Account_Balance_Feb - min(dt$Account_Balance_Feb)+1
boxcox_result <- boxcox(dt$Account_Balance_Feb_t ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Account_Balance_Feb_t <- boxcox_transform(dt$Account_Balance_Feb_t, optimal_lambda)
# Account_Balance_Jan
dt$Account_Balance_Jan_t <- dt$Account_Balance_Jan - min(dt$Account_Balance_Jan)+1
boxcox_result <- boxcox(dt$Account_Balance_Jan_t ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Account_Balance_Jan_t <- boxcox_transform(dt$Account_Balance_Jan_t, optimal_lambda)
# Amount_Prev_Repaid_Jun
dt$Amount_Prev_Repaid_Jun_t <- dt$Amount_Prev_Repaid_Jun - min(dt$Amount_Prev_Repaid_Jun)+1
boxcox_result <- boxcox(dt$Amount_Prev_Repaid_Jun_t ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Amount_Prev_Repaid_Jun_t <- boxcox_transform(dt$Amount_Prev_Repaid_Jun_t, optimal_lambda)
# Amount_Prev_Repaid_May
dt$Amount_Prev_Repaid_May_t <- dt$Amount_Prev_Repaid_May - min(dt$Amount_Prev_Repaid_May)+1
boxcox_result <- boxcox(dt$Amount_Prev_Repaid_May_t ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Amount_Prev_Repaid_May_t <- boxcox_transform(dt$Amount_Prev_Repaid_May_t, optimal_lambda)
# Amount_Prev_Repaid_Apr
dt$Amount_Prev_Repaid_Apr_t <- dt$Amount_Prev_Repaid_Apr - min(dt$Amount_Prev_Repaid_Apr)+1
boxcox_result <- boxcox(dt$Amount_Prev_Repaid_Apr_t ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Amount_Prev_Repaid_Apr_t <- boxcox_transform(dt$Amount_Prev_Repaid_Apr_t, optimal_lambda)
# Amount_Prev_Repaid_Mar
dt$Amount_Prev_Repaid_Mar_t <- dt$Amount_Prev_Repaid_Mar - min(dt$Amount_Prev_Repaid_Mar)+1
boxcox_result <- boxcox(dt$Amount_Prev_Repaid_Mar_t ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Amount_Prev_Repaid_Mar_t <- boxcox_transform(dt$Amount_Prev_Repaid_Mar_t, optimal_lambda)
# Amount_Prev_Repaid_Feb
dt$Amount_Prev_Repaid_Feb_t <- dt$Amount_Prev_Repaid_Feb - min(dt$Amount_Prev_Repaid_Feb)+1
boxcox_result <- boxcox(dt$Amount_Prev_Repaid_Feb_t ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Amount_Prev_Repaid_Feb_t <- boxcox_transform(dt$Amount_Prev_Repaid_Feb_t, optimal_lambda)
# Amount_Prev_Repaid_Jan
dt$Amount_Prev_Repaid_Jan_t <- dt$Amount_Prev_Repaid_Jan - min(dt$Amount_Prev_Repaid_Jan)+1
boxcox_result <- boxcox(dt$Amount_Prev_Repaid_Jan_t ~ 1, lambda = seq(-5, 5, by = 0.1))
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
dt$Amount_Prev_Repaid_Jan_t <- boxcox_transform(dt$Amount_Prev_Repaid_Jan_t, optimal_lambda)

dt$Age <- NULL
dt$Account_Balance_Jun <- NULL
dt$Account_Balance_May <- NULL
dt$Account_Balance_Apr <- NULL
dt$Account_Balance_Mar <- NULL
dt$Account_Balance_Feb <- NULL
dt$Account_Balance_Jan <- NULL
dt$Amount_Prev_Repaid_Jun <- NULL
dt$Amount_Prev_Repaid_Apr <- NULL
dt$Amount_Prev_Repaid_Feb <- NULL
dt$Amount_Prev_Repaid_Jan <- NULL
dt$Amount_Prev_Repaid_Mar <- NULL
dt$Amount_Prev_Repaid_May <- NULL
dt$Credit <- NULL

fwrite(dt, "clean_data_normalized.csv")

