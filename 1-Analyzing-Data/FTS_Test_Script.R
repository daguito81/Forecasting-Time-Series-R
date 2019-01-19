library(fBasics)
library(forecast)


data <- read.csv("Sessions2&3sim.csv", header = T, sep=';', dec = ',')

#For Simulated Data
#Uncomment this if the main_script will be used
series1 <- data[,1][1:200]
series2 <- data[,2][1:200]
series3 <- data[,3][1:200]
series4 <- data[,4][1:200]
series5 <- data[,5][1:200]
series6 <- data[,6]
series <- list(series1, series2, series3, series4, series5, series6)
names(series) <- c('series1', 'series2', 'series3', 'series4', 'series5', 'series6')

#For Real Data
#Uncomment this if a single series is going to be used 
# series1 <- data[,1]



par(mar=c(1,1,1,1))


#### Stationary ####
# Use this function if you want to check and transform the data (stationary)
checkStationary <- function(y, log_trf=FALSE) {
  stationary <<- F
  degrees <<- 0
  print("Original Graphical Representation")
  par(mfrow=c(3,1))
  ts.plot(y)
  acf(y)
  pacf(y)
  d <- ndiffs(y, alpha=0.05, test=c("adf"))
  if(d == 0){
    print("Series is Stationary")
    stationary <<- T
    return(y)
  } else if (d > 0){
    stationary <<-F
    degrees <<- d
    print("Series is not Stationary, Transforming")
    if(log_trf == F){
      z<-diff(y,d)
    } else {
      print("Log tranformation is ON")
      z <- diff(log(y),d)
    }
    if (ndiffs(z, alpha=0.05, test=c("adf")) == 0){
      print(paste("Transformed ", d, " level" ))
      print("Transformed Data Graphical Representation")
      par(mfrow=c(3,1))
      ts.plot(z)
      acf(z)
      pacf(z)
      return(z)
    } else {
      print("Transformation didn't work")
    }
  }
}

#### Check Normality ####
# This function checks if the distribution is normal
checkNormality <- function(y){
  print("Graphical Representation:")
  par(mfrow=c(1,1))
  hist(y,prob=T,ylim=c(0,0.6),xlim=c(mean(y)-3*sd(y),mean(y)+3*sd(y)),col="red")
  lines(density(y),lwd=2)
  mu<-mean(y)
  sigma<-sd(y)
  x<-seq(mu-3*sigma,mu+3*sigma,length=100)
  yy<-dnorm(x,mu,sigma)
  lines(x,yy,lwd=2,col="blue")
  print("Stat Values:")
  print(paste("Mean: ", mean(y)))
  print(paste("SD: ", sd(y)))
  print(paste("Skewness: ", skewness(y)))
  a <- kurtosis(y, method = c("moment"))[1]
  print(paste("Kurtosis: ", kurtosis(y, method = c("moment"))[1]))
  
  shtest <- shapiro.test(y)
  print(paste("Shapiro Test (p-value): ", shtest$p.value))
  if (shtest$p.value > 0.05){
    series_normal <<- TRUE
    print("Distribution is Normal")
  } else {
    print("Distribution is NOT normal")
    series_normal <<- FALSE
  }
  
}

#### Check WN/SWN/GWN ####
#USe this function to check for noise
checkWhiteNoise <- function(y){
  box <- Box.test(y, lag = 20, type="Ljung")
  print(paste("Box Test (p-value): ", box$p.value))
  #Hyp Testing for Mean = 0
  z <- (mean(y) - 0) / ( sd(y) / sqrt(length(y)))
  alpha <- 0.05
  z.half.alpha <- qnorm(1-alpha/2)
  print(paste("Z-Statistic: ", z))
  
  
  if (box$p.value > 0.05 && z > -z.half.alpha && z < z.half.alpha){
    WN <<- T
    print("The Series is White Noise")
  } else {
    WN <<- F
    print("The Series is NOT White Noise")
  }
  #Testing for Gaussian White Noise
  if(series_normal == T & WN == T ){
    GWN <<- T
    print("The Series is Gaussian White Noise")
  } else {
    GWN <<- F
    print("The Series is NOT Gaussian White Noise")
  }
  #Testing for Strict White Noise
  if (WN == F){
    SWN <<- F
    print("Series is Not Strict White Noise")
  } else if (WN == T && GWN == T){
    SWN <<- T
    print("Series is Strict White Noise")
  } else if (WN == T && GWN == F){
    a <- Box.test(y^2,lag=20, type="Ljung")
    print(paste("Box-y^2 Test (p-value): ", a$p.value))
    if (a$p.value > 0.05){
      SWN <<- T
      print("Series is Strict White Noise")
    }
    else {
      SWN <<- F
      print("Series is NOT Strict White Noise")
    
    }
  }
}




#### Check Models / Transformations ####
# USe this function to check if a Linear or non-linear model is needed
# or if a transformation was needed to make the series stationary
checkModelsTransf <- function(y){
  #Linear / Non-Linear Models
  if(WN == T && SWN == T){
    LM <<- F
    nLM <<- F
    print("Linear Model is not needed")
    print("Non-Linear Model is not needed")
  } else if (WN == F && SWN == F){
    LM <<- T
    nLM <<- NA
    print("Linear Model is needed")
    print("Don't Know if non-Linear Model is needed")
  } else if (WN == T && SWN == F){
    LM <<- F
    nLM <<- T
    print("Linear Model is not needed")
    print("Non-Linear Model is needed")
  }
  
  # Transformations
  if (stationary == T){
    print("No Transformation Needed")
    trf <<- F
  } else {
    print(paste("Transformation Needed: ", degrees, " degrees"))
    trf <<- T
  }
}

#### Check Everything ####
# Main function that runs all previous functions on the data
# log_trf is the parameter if you want to apply log transformation
checkFTS <- function(y, log_trf = FALSE){
  y <- checkStationary(y, log_trf = log_trf)
  checkNormality(y)
  checkWhiteNoise(y)
  checkModelsTransf(y)
  result <- c(stationary, series_normal, WN, GWN, SWN, LM, nLM, trf)
  return(result)
}


#### Run scripts for many series ####
# This is run if you have many series and want to create a results table
# the top series block needs to be uncommented
main_script <- function(series){
  results <- data.frame(matrix(ncol = 8, nrow = 0))
  j <- 1
  for (i in series){
    print(paste("Series ", j))
    res <- checkFTS(i)
    print("-------------------------------------------------------")
    print("-------------------------------------------------------")
    results <- rbind(results, res)
    j <- j + 1
  }
  colnames(results) <- c('stationary', 'series_normal', 'WN', 'GWN', 'SWN', 'LM', 'nLM', 'trf')
  return(results)
}

#Uncomment this for several series
final_results <- main_script(series)

#Uncomment this for 1 series
#checkFTS(series6, log_trf=FALSE)