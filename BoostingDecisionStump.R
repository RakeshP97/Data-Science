
#Finding RSS value for the RM column in Train Data
RSS <- function(colName, s, Xvalues, Yvalues)
{
   meanLess <- meanGreater <- lessNumber<- greaterNumber <-0
  
  meanLess <- mean(Yvalues[Xvalues[[colName]]<s])
  meanGreater <-  mean(Yvalues[Xvalues[[colName]] >= s])
  sumLessRSS <- sumGreaterRSS <- sumRSS<- 0
  for(j in 1: nrow(Xvalues))
  {
    if(Xvalues[[colName]][j]<s)
    {
      sumLessRSS = sumLessRSS + (Yvalues[j]- meanLess)^2
    }
    else if (Xvalues[[colName]][j]>=s)
    {
      sumGreaterRSS = sumGreaterRSS + (Yvalues[j]- meanGreater)^2
    }
  }
  sumRSS <- sumLessRSS + sumGreaterRSS
  return(c(sumRSS, meanLess, meanGreater))
}

#Finding minimum RSS value for the RM and LSTAT and retun the minimum values 
DecisionStump <- function(Xvalues, Yvalues)
{
  lstatS = Xvalues$lstat
  rmS = Xvalues$rm
  minLstatRSS <- minRmRSS <- 10000000
  slstat <- srm <- 0
  for (j in 1:nrow(Xvalues))
  {
    lstatRSS <- RSS('lstat', lstatS[j], Xvalues, Yvalues)
    lsRSS <- as.numeric(lstatRSS[1])
    if (minLstatRSS > lsRSS)
    {
      minLstatRSS <- lsRSS
      slstat <- lstatS[j];
      meanLess =  as.numeric(lstatRSS[2])
      meanGreater = as.numeric(lstatRSS[3])
      
    }
    rmRSS <- RSS('rm', rmS[j], Xvalues, Yvalues)
    rRSS <- as.numeric(rmRSS[1])
    if (minRmRSS > rRSS)
    {
      minRmRSS <- rRSS
      srm <- rmS[j]
      meanLess =  as.numeric(rmRSS[2])
      meanGreater = as.numeric(rmRSS[3])
    }
  }
  if (minRmRSS < minLstatRSS)
  {
    return(c('rm', srm, meanLess, meanGreater))
  }
  else
  {
    return(c('lstat', slstat, meanLess, meanGreater))
  }
}

DS <- function()
{
  
  #load the boston data
  library(MASS)
  data(Boston)
  set.seed(928)
  
  #Divide the data set into two equal parts
  train <- sample(1:nrow(Boston), nrow(Boston) / 2)
  trainData <- Boston[train, ]
  testData <- Boston[-train, ]
  trainYvales <- trainData$medv
  testYvalues <- testData$medv
  
  D = DecisionStump(trainData, trainYvales)
  #Test RSS and MSE at minimum S value of the train data
  sumTestLessRSS <- sumTestGreaterRSS <- sumTestRSS<- 0
  s <- as.numeric(D[2])
  meanLess =  as.numeric(D[3])
  meanGreater = as.numeric(D[4])
  print(c(D[1],s, meanLess, meanGreater))
  colName <- D[1] 
  for(i in 1:nrow(testData))
  {
    if(testData[[colName]][i] < s)
    {
      sumTestLessRSS <- sumTestLessRSS + (testYvalues[i]- meanLess)^2
    }
    if(testData[[colName]][i] >= s)
    {
      sumTestGreaterRSS <- sumTestGreaterRSS + (testYvalues[i]- meanGreater) ^2
    }
  }
  sumTestRSS <- sumTestLessRSS +sumTestGreaterRSS;
  print(c("Test RSS value is: ", sumTestRSS))
  print(c("Test MSE value is: ", (sumTestRSS / nrow(testData))))
}
DS()