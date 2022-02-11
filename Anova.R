t.test.Equality_Of_Means.Anova <- function(data.1, data.2, type, MSE, df, significance = 0.05, welch = FALSE){
  
  n <- length(data.1[,1])
  m <- length(data.2[,1])
  alpha <- significance
  x.bar <- mean(data.1[,1])
  y.bar <- mean(data.2[,1])
  
  s.p <- sqrt(MSE)
  test.statistic <- (x.bar - y.bar)/(s.p*sqrt((1/n)+(1/m)))
  
  if(type == 't'){
    significance.greater <- -qt(alpha/2, df)
    significance.lower <- qt(alpha/2, df)
    t.value.sig <- list("t-value at lower significance threshold" = significance.lower, "t-value at upper significance threshold" = significance.greater)
    deci <- (test.statistic >= significance.greater) | (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 2*pt(-abs(test.statistic), df) 
    t.test.type <- "Alternate: Both-Sided [ true mean of data.1 unequals true mean of data.2 ]"
  }
  
  return(list("Two Sample t-test for Equality of Means" = t.test.type, "Welch"=welch, "Level of Significance"=alpha, "Null Hypothesis(H0):"= "True mean of data.1 - True mean of data.2 = 0", "Sample Size of data.1"=n, "Sample Size of data.2"=m,"Sample Mean of data.1"=x.bar, "Sample Mean of data.2"=y.bar, "Pooled Sample Standard Deviation"=s.p, "t-value at the significance threshold" = t.value.sig, "t-value"=test.statistic, "Degrees of Freedom"=df, "p-value"=p.value.test.statistic, "Decision"= decision))
  
}
