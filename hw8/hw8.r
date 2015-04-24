xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

  ### 

genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  Sample<-unlist(tapply(y,x,sample, replace=T))
  names(Sample)<-c()
  return(Sample)
}


genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
errors<-sample(err,50,replace=F)
z<-fit+errors
return(z)
}
  # test genBootR(predict(lm(y~x,data=myData),newdata=myData),myData[,2]-predict(lm(y~x,data=myData),newdata=myData),rep)
fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line or a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if (degree==1){
    Model<-lm(y ~ x)  
  } else if (degree==2){
    Model<-lm(y ~ x + I(x^2))
  }
  else { stop ("Error:Invalid Degrees")}
  coeff<-Model$coef
  return(coeff)
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  
if (is.null(fit)) {
  a<-genBootY (data[,1], data[,2])
  b<-fitModel (data[,1], a, degree=degree)
} else {
  a<-genBootR (fit[,1], fit[,2])
  b<-fitModel (data[,1], a, degree=degree)
}
return (b)
}


repBoot = function(data, B = 1000){
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic
  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list  
Line<-data.frame(lm(data$y~data$x)$fitted.values,lm(data$y~data$x)$residuals)
Quadratic<-data.frame(lm(data$y~data$x+I((data$x)^2))$fitted.values,lm(data$y~data$x+I((data$x)^2))$residuals)
l1 <- replicate(B,oneBoot(data,fit=NULL,degree=1))
l2 <- replicate(B,oneBoot(data,fit=NULL,degree=2))
l3 <- replicate(B,oneBoot(data,fit=Line,degree=1))
l4 <- replicate(B,oneBoot(data,fit=Quadratic,degree=2)) 
coeff<- list(l1,l2,l3,l4)
return(coeff)
} 

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data
  plot(x, y)
  
  if (ncol(coeff) == 2) {
    mapply(function(a,b) abline(a,b,col='red'),coeff[,1],coeff[,2])
  } else {
    mapply(function(a,b,c) curve(a+b*x+c*x^2,add=T,col='red'),coeff[,1],coeff[,2],coeff[,3])
  } 
  
  if (length(trueCoeff == 2)) {
    abline(trueCoeff[1], trueCoeff[2], col='green')
  } else {
    curve(trueCoeff[1] + trueCoeff[2]*x + trueCoeff[3]*x^2, add=T, col='red')
  }
  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out

}

### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}
