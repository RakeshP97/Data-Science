#TASK 6: Stopping creteria based on mutiple runs stopping at 100 epochs and Eta(n) value is 0.02
#Load Auto Data
Auto <- read.table("Auto.data", header = T, na.strings = '?')
#Omit the Null records
Auto <- na.omit(Auto)
attach(Auto)
library(dummies)

#Split Data into 2 parts
train <- (dim(Auto)[1] / 2)
set.seed(2809)
dummy_origin <- dummy(Auto$origin, sep = ".")
sub_Auto <-
  data.frame(horsepower, weight, year, dummy_origin[, 2], dummy_origin[, 3])

new_train.X <- sub_Auto[1:train,]
test.X <- sub_Auto[-c(1:train),]

#Split Label Data set
y <- rep(0, dim(Auto)[1])
y[mpg >= 23] <- 1
ma1 <- rep(0, 10)
n <- 0.0355
#Standarise Test data
for (i in 1:(dim(new_train.X)[2] - 2))
{
  m = mean(new_train.X[, i])
  s = sd(new_train.X[, i])
  for (j in 1:dim(new_train.X)[1])
  {
    new_train.X[j, i] = ((new_train.X[j, i] - m) / s)
    
  }
}
#Test Data
#Standarise the column values
for (i in 1:(dim(test.X)[2] - 2))
{
  m = mean(test.X[, i])
  s = sd(test.X[, i])
  for (j in 1:dim(test.X)[1])
  {
    test.X[j, i] = ((test.X[j, i] - m) / s)
    
  }
}

for (z in 1:100)
{
  #Lorgstic regerssion
  beta_values <- runif(6,-0.7, 0.7)
  previous_mse <- 0
  before_previous <- 0
  #Epochs rotation logic
  for (j in 1:100)
  {
    d <- rep(0, dim(new_train.X)[1])
    
    for (i in 1:dim(new_train.X)[1])
    {
      p = beta_values[1] + beta_values[2] * new_train.X[i, 1] + beta_values[3] * new_train.X[i, 2] +
        beta_values[4] * new_train.X[i, 3] + beta_values[5] * new_train.X[i, 4] + beta_values[6] * new_train.X[i, 5]
      P = (1 / (1 + exp(-p)))
      if (P > 0.5)
      {
        Pred <- 1
      }
      else
      {
        Pred <- 0
        
      }
      #“Backward pass”: Compute the coefficient
      d[i] = 2 * (y[i] - Pred) * (exp(-p) / ((1 + exp(-p)) ^ 2))
    }
    
    # Update the weights
    beta_values[1] = beta_values[1] + (n * (sum(d) / dim(new_train.X)[1]))
    for (k in 1:5)
    {
      dx <- 0
      #Calculate the beta values
      for (i in 1:dim(new_train.X)[1])
      {
        dx <- dx + (d[i] * new_train.X[i, k])
      }
      beta_values[k + 1] = beta_values[k + 1] + ((n / (dim(new_train.X)[1])) * (dx))
    }
    #The training set MSE is for each epoch
    MSE <- 0
    y_pred <- rep(0, train)
    for (i in 1:train)
    {
      p = beta_values[1] + beta_values[2] * new_train.X[i, 1] + beta_values[3] * new_train.X[i, 2] +
        beta_values[4] * new_train.X[i, 3] + beta_values[5] * new_train.X[i, 4] + beta_values[6] * new_train.X[i, 5]
      P = (1 / (1 + exp(-p)))
      if (P > 0.5)
      {
        Pred <- 1
      }
      else
      {
        Pred <- 0
        
      }
      y_pred[i] <- Pred
      MSE <- MSE + ((y[i] - Pred) ** 2)
    }
    MSE <- (MSE / train)
    #Continous three MSE values matches break the epochs loop
    if (before_previous == previous_mse && previous_mse == MSE)
    {
      #print(c(before_previous,previous_mse,MSE))
      break;
      
    }
    before_previous = previous_mse
    previous_mse = MSE
  }
  
  
  #Apply calculated weights on Test Data to predict acurracy
  pr <- rep(0, train)
  TEST_MSE <- 0
  for (i in 1:train) {
    p = beta_values[1] + beta_values[2] * test.X[i, 1] + beta_values[3] * test.X[i, 2] +
      beta_values[4] * test.X[i, 3] + beta_values[5] * test.X[i, 4] + beta_values[6] * test.X[i, 5]
    
    P = (1 / (1 + exp(-p)))
    
    if (P > 0.5)
    {
      Pred <- 1
      
    }
    else
    {
      Pred <- 0
      
    }
    pr[i] = Pred
    #Test MSE calculation
    
    TEST_MSE <- TEST_MSE + ((y[train + i] - Pred) ** 2)
  }
  ma1[z] <- (TEST_MSE / train)
  print(c(n, MSE, ma1[z]))
  #Check for the difference between Train MSE and Test MSE if it is less than or equal to 5 then stop the looping
  #if (abs(MSE - ma[k]) <= 0.05)
  #{
   # break
  #}
}

boxplot(ma1,
        xlab = "MSE",
        ylab = "Number Epochs",
        main = "MSE Calculations")
