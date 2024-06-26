library(data.table)
library(ggplot2)
library(fastDummies)

dt <- fread("cleaned_data.csv")
# setting a seed
set.seed(234) # for reproducibility

# we are going to split the data into testing data and training data
train = sample(1:nrow(dt), as.integer(nrow(dt)/3))
train
training_dt=dt[train,]
testing_dt=dt[-train,]
colnames(training_dt)

model <- glm(Defaulted ~.,family=binomial(link='logit'),data=training_dt)

fitted.results <- predict(model,newdata=testing_dt,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing_dt$Defaulted)
print(paste('Accuracy of Logistic Model',1-misClasificError))
print(paste('Proportion Not Defaulted',1-mean(testing_dt$Defaulted)))
summary(model)





d <- dt
d$Defaulted <- NULL
d$Credit <- NULL
d$Age <- NULL
d$Num_No_Credit_Consumption <- NULL
d$Num_Repaid_Duly <- NULL
d$Num_Revolving_Credit <- NULL
d$Amount_Prev_Repaid_Jan <- NULL
d$Amount_Prev_Repaid_Feb <- NULL
d$Amount_Prev_Repaid_Mar <- NULL
d$Amount_Prev_Repaid_Apr <- NULL
d$Amount_Prev_Repaid_May <- NULL
d$Amount_Prev_Repaid_Jun <- NULL
d$Account_Balance_Jan <- NULL
d$Account_Balance_Feb <- NULL
d$Account_Balance_Mar <- NULL
d$Account_Balance_Apr <- NULL
d$Account_Balance_May <- NULL
d$Account_Balance_Jun <- NULL
dummy_cols <- colnames(d)
t <- dummy_cols(dt, select_columns = colnames(d),remove_first_dummy = T)


# we are going to split the data into testing data and training data
train = sample(1:nrow(t), as.integer(nrow(t)/3))
train
training_t=t[train,]
testing_t=t[-train,]
colnames(training_t)

model <- glm(Defaulted ~.,family=binomial(link='logit'),data=training_t)
summary(model)

fitted.results <- predict(model,newdata=testing_t,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing_t$Defaulted)
print(paste('Accuracy of Logistic Model',1-misClasificError))
print(paste('Proportion Not Defaulted',1-mean(testing_t$Defaulted)))
