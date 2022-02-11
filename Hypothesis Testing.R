
t.test.data <- function(data, mu.0, type = 't', significance = 0.05){
  
  n <- length(data[,1])
  alpha <- significance
  x.bar <- mean(data[,1]) 
  s <- sd(data[,1])
  test.statistic <- sqrt(n)*(x.bar - mu.0)/s # This Test Statistic is a T r.v. with t distribution with n-1 degs of freedom
  
  if(type == 't'){
    significance.greater <- -qt(alpha/2, n-1)
    significance.lower <- qt(alpha/2, n-1)
    t.value.sig <- list("t-value at lower significance threshold" = significance.lower, "t-value at upper significance threshold" = significance.greater)
    deci <- (test.statistic >= significance.greater) | (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 2*pt(-abs(test.statistic), n-1) 
    t.test.type <- "Alternate: Both-Sided"
  }
  if(type == 'l'){
    significance.lower <- qt(alpha, n-1)
    t.value.sig <- significance.lower
    deci <- (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- pt(test.statistic, n-1) 
    t.test.type <- "Alternate: Less Than Type"
  }
  if(type == 'g'){
    significance.greater <- -qt(alpha, n-1)
    t.value.sig <- significance.greater
    deci <- (test.statistic >= significance.greater)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 1 - pt(test.statistic, n-1) 
    t.test.type <- "Alternate: Greater Than Type"
  }
  
  return(list("One Sample t-test" = t.test.type, "Level of Significance"=alpha, "Null Hypothesis(H0)"=mu.0, "Sample Mean"=x.bar, "Sample Standard Deviation"=s, "t-value at the significance threshold" = t.value.sig, "t-value"=test.statistic, "p-value"=p.value.test.statistic, "Decision"= decision))
}

Hypothesis.Testing <- function(data, mu.0, std=-1, type, significance = 0.05){
  
  n <- length(data[,1])
  
  if(std == -1 & n <= 30){
    return (t.test.data(data, mu.0, type, significance))
  }
    
    alpha <- significance
    x.bar <- mean(data[,1]) 
    if(std == -1){
      s <- sd(data[,1])
      std.known <- "Unknown" 
    }
    else{
      s <- std
      std.known <- "Known"
    }
    test.statistic <- sqrt(n)*(x.bar - mu.0)/s
    
    if(type == 't'){
      significance.greater <- -qnorm(alpha/2)
      significance.lower <- qnorm(alpha/2)
      z.value.sig <- list("z-value at lower significance threshold" = significance.lower, "z-value at upper significance threshold" = significance.greater)
      deci <- (test.statistic >= significance.greater) | (test.statistic <= significance.lower)
      if(deci == TRUE)
        decision <- "Reject Null Hypothesis"
      else
        decision <- "Fail to Reject Null Hypothesis"
      p.value.test.statistic <- 2*pnorm(-abs(test.statistic)) 
      test.type <- "Alternate: Both-Sided"
    }
    if(type == 'l'){
      significance.lower <- qnorm(alpha)
      z.value.sig <- significance.lower
      deci <- (test.statistic <= significance.lower)
      if(deci == TRUE)
        decision <- "Reject Null Hypothesis"
      else
        decision <- "Fail to Reject Null Hypothesis"
      p.value.test.statistic <- pnorm(test.statistic) 
      test.type <- "Alternate: Less Than Type"
    }
    if(type == 'g'){
      significance.greater <- -qnorm(alpha)
      z.value.sig <- significance.greater
      deci <- (test.statistic >= significance.greater)
      if(deci == TRUE)
        decision <- "Reject Null Hypothesis"
      else
        decision <- "Fail to Reject Null Hypothesis"
      p.value.test.statistic <- 1 - pnorm(test.statistic) 
      test.type <- "Alternate: Greater Than Type"
    }
    
    return(list("One Sample Z-test" = test.type, "Level of Significance"=alpha, "Null Hypothesis(H0)"=mu.0, "Sample Size"=n, "Sample Mean"=x.bar, "True Standard Deviation" = std.known, "Sample Standard Deviation"=s, "z-value at threshold" = z.value.sig, "z-value"=test.statistic, "p-value"=p.value.test.statistic, "Decision"= decision))
}

t.test.data.withoutDataset <- function(sample_mean, sample_std, sample_size, mu.0, type = 't', significance = 0.05){
  
  n <- sample_size
  alpha <- significance
  x.bar <- sample_mean 
  s <- sample_std
  test.statistic <- sqrt(n)*(x.bar - mu.0)/s # This Test Statistic is a T r.v. with t distribution with n-1 degs of freedom
  
  if(type == 't'){
    significance.greater <- -qt(alpha/2, n-1)
    significance.lower <- qt(alpha/2, n-1)
    t.value.sig <- list("t-value at lower significance threshold" = significance.lower, "t-value at upper significance threshold" = significance.greater)
    deci <- (test.statistic >= significance.greater) | (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 2*pt(-abs(test.statistic), n-1) 
    t.test.type <- "Alternate: Both-Sided"
  }
  if(type == 'l'){
    significance.lower <- qt(alpha, n-1)
    t.value.sig <- significance.lower
    deci <- (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- pt(test.statistic, n-1) 
    t.test.type <- "Alternate: Less Than Type"
  }
  if(type == 'g'){
    significance.greater <- -qt(alpha, n-1)
    t.value.sig <- significance.greater
    deci <- (test.statistic >= significance.greater)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 1 - pt(test.statistic, n-1) 
    t.test.type <- "Alternate: Greater Than Type"
  }
  
  return(list("One Sample t-test" = t.test.type, "Level of Significance"=alpha, "Null Hypothesis(H0)"=mu.0, "Sample Mean"=x.bar, "Sample Standard Deviation"=s, "t-value at the significance threshold" = t.value.sig, "t-value"=test.statistic, "p-value"=p.value.test.statistic, "Decision"= decision))
}

Hypothesis.Testing.withoutDataset <- function(sample_mean, sample_std, sample_size, mu.0, std=-1, type, significance = 0.05){
  
  n <- sample_size
  
  if(std == -1 & n <= 30){
    return (t.test.data.withoutDataset(sample_mean, sample_std, sample_size, mu.0, type, significance))
  }
  
  alpha <- significance
  x.bar <- sample_mean
  if(std == -1){
    s <- sample_std
    std.known <- "Unknown" 
  }
  else{
    s <- std
    std.known <- "Known"
  }
  test.statistic <- sqrt(n)*(x.bar - mu.0)/s
  
  if(type == 't'){
    significance.greater <- -qnorm(alpha/2)
    significance.lower <- qnorm(alpha/2)
    z.value.sig <- list("z-value at lower significance threshold" = significance.lower, "z-value at upper significance threshold" = significance.greater)
    deci <- (test.statistic >= significance.greater) | (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 2*pnorm(-abs(test.statistic)) 
    test.type <- "Alternate: Both-Sided"
  }
  if(type == 'l'){
    significance.lower <- qnorm(alpha)
    z.value.sig <- significance.lower
    deci <- (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- pnorm(test.statistic) 
    test.type <- "Alternate: Less Than Type"
  }
  if(type == 'g'){
    significance.greater <- -qnorm(alpha)
    z.value.sig <- significance.greater
    deci <- (test.statistic >= significance.greater)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 1 - pnorm(test.statistic) 
    test.type <- "Alternate: Greater Than Type"
  }
  
  return(list("One Sample Z-test" = test.type, "Level of Significance"=alpha, "Null Hypothesis(H0)"=mu.0, "Sample Size"=n, "Sample Mean"=x.bar, "True Standard Deviation" = std.known, "Sample Standard Deviation"=s, "z-value at threshold" = z.value.sig, "z-value"=test.statistic, "p-value"=p.value.test.statistic, "Decision"= decision))
}

# --------------------------------------------------------------------------------------------------------------------------------

t.test.Equality_Of_Means <- function(data.1, data.2, type, significance = 0.05, welch = FALSE){
  
  n <- length(data.1[,1])
  m <- length(data.2[,1])
  alpha <- significance
  x.bar <- mean(data.1[,1])
  y.bar <- mean(data.2[,1])
  s.x <- sd(data.1[,1])
  s.y <- sd(data.2[,1])
  if (welch == TRUE){
    r <-  ((s.x^2 /n)+(s.y^2 /m))^2 / (((s.x^2 /n)^2 /(n-1)) + ((s.y^2 /m)^2 / (m-1)))
    r <- round(r)
    test.statistic <- (x.bar - y.bar) / sqrt((s.x^2)/n + (s.y^2)/m)
    s.p <- "Not Required"
  }
  else{
    r <- n+m-2
    s.p <- sqrt(((n-1)*(s.x^2) + (m-1)*(s.y^2)) / (r) ) 
    test.statistic <- (x.bar - y.bar)/(s.p*sqrt((1/n)+(1/m)))
  }
  
  if(type == 't'){
    significance.greater <- -qt(alpha/2, r)
    significance.lower <- qt(alpha/2, r)
    t.value.sig <- list("t-value at lower significance threshold" = significance.lower, "t-value at upper significance threshold" = significance.greater)
    deci <- (test.statistic >= significance.greater) | (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 2*pt(-abs(test.statistic), r) 
    t.test.type <- "Alternate: Both-Sided [ true mean of data.1 unequals true mean of data.2 ]"
  }
  
  if(type == 'l'){ # Alternate:  mu.x - mu.y < 0
    significance.lower <- qt(alpha, r)
    t.value.sig <- significance.lower
    deci <- (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- pt(test.statistic, r) 
    t.test.type <- "Alternate: Less Than Type [ true mean of data.1 < true mean of data.2 ]"
  }
  
  if(type == 'g'){
    significance.greater <- -qt(alpha, r)
    t.value.sig <- significance.greater
    deci <- (test.statistic >= significance.greater)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 1 - pt(test.statistic, r) 
    t.test.type <- "Alternate: Greater Than Type [ true mean of data.1 > true mean of data.2 ]"
  }
  
  return(list("Two Sample t-test for Equality of Means" = t.test.type, "Welch"=welch, "Level of Significance"=alpha, "Null Hypothesis(H0):"= "True mean of data.1 - True mean of data.2 = 0", "Sample Size of data.1"=n, "Sample Size of data.2"=m,"Sample Mean of data.1"=x.bar, "Sample Mean of data.2"=y.bar, "Sample Standard Deviation of data.1"=s.x, "Sample Standard Deviation of data.2"=s.y, "Pooled Sample Standard Deviation"=s.p, "t-value at the significance threshold" = t.value.sig, "t-value"=test.statistic, "Degrees of Freedom"=r, "p-value"=p.value.test.statistic, "Decision"= decision))
  
}

Hypothesis.Testing.Equality_of_Means <- function(data.1, data.2, type, significance = 0.05, std.1 = -1, std.2 = -1){
  
  
  n <- length(data.1[,1])
  m <- length(data.2[,1])
  
  if((std.1 == -1 & std.2 == -1) & (n <= 30 & m <= 30)){
    return (t.test.Equality_Of_Means(data.1, data.2, type, significance))
  }
  
  alpha <- significance
  x.bar <- mean(data.1[,1])
  y.bar <- mean(data.2[,1])
  
  if(std.1 == -1 & std.2 == -1){
    s.x <- sd(data.1[,1])
    s.y <- sd(data.2[,1])
    std.known <- "Unknown" 
  }
  else{
    s.x <- std.1
    s.y <- std.2
    std.known <- "Known"
  }
  test.statistic <- (x.bar - y.bar) / sqrt((s.x^2)/n + (s.y^2)/m)
  
  if(type == 't'){
    significance.greater <- -qnorm(alpha/2)
    significance.lower <- qnorm(alpha/2)
    z.value.sig <- list("z-value at lower significance threshold" = significance.lower, "z-value at upper significance threshold" = significance.greater)
    deci <- (test.statistic >= significance.greater) | (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 2*pnorm(-abs(test.statistic)) 
    test.type <- "Alternate: Both-Sided [ true mean of data.1 unequals true mean of data.2 ]"
  }
  if(type == 'l'){
    significance.lower <- qnorm(alpha)
    z.value.sig <- significance.lower
    deci <- (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- pnorm(test.statistic) 
    test.type <- "Alternate: Less Than Type [ true mean of data.1 < true mean of data.2 ]"
  }
  if(type == 'g'){
    significance.greater <- -qnorm(alpha)
    z.value.sig <- significance.greater
    deci <- (test.statistic >= significance.greater)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 1 - pnorm(test.statistic) 
    test.type <- "Alternate: Greater Than Type [ true mean of data.1 > true mean of data.2 ]"
  }
  
  return(list("Two Sample z-test for Equality of Means" = test.type, "Level of Significance"=alpha, "Null Hypothesis(H0):"= "True mean of data.1 - True mean of data.2 = 0", "Sample Size of data.1"=n, "Sample Size of data.2"=m, "Sample Mean of data.1"=x.bar, "Sample Mean of data.2"=y.bar,  "True Standard Deviations" = std.known, "Sample Standard Deviation of data.1"=s.x, "Sample Standard Deviation of data.2"=s.y, "z-value at threshold" = z.value.sig, "z-value"=test.statistic, "p-value"=p.value.test.statistic, "Decision"= decision))
}

t.test.Equality_Of_Means.withoutDataset <- function(sample_mean_data.1, sample_mean_data.2, sample_std.1, sample_std.2, sample_size_data.1, sample_size_data.2, type, significance = 0.05, welch=FALSE){
  
  n <- sample_size_data.1
  m <- sample_size_data.2
  alpha <- significance
  x.bar <- sample_mean_data.1
  y.bar <- sample_mean_data.2
  s.x <- sample_std.1
  s.y <- sample_std.2
  if (welch == TRUE){
    r <-  ((s.x^2 /n)+(s.y^2 /m))^2 / (((s.x^2 /n)^2 /(n-1)) + ((s.y^2 /m)^2 / (m-1)))
    r <- round(r)
    test.statistic <- (x.bar - y.bar) / sqrt((s.x^2)/n + (s.y^2)/m)
    s.p <- "Not Required"
  }
  else{
    r <- n+m-2
    s.p <- sqrt(((n-1)*(s.x^2) + (m-1)*(s.y^2)) / (r) ) 
    test.statistic <- (x.bar - y.bar)/(s.p*sqrt((1/n)+(1/m)))
  }
  
  if(type == 't'){
    significance.greater <- -qt(alpha/2, r)
    significance.lower <- qt(alpha/2, r)
    t.value.sig <- list("t-value at lower significance threshold" = significance.lower, "t-value at upper significance threshold" = significance.greater)
    deci <- (test.statistic >= significance.greater) | (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 2*pt(-abs(test.statistic), r) 
    t.test.type <- "Alternate: Both-Sided [ true mean of data.1 unequals true mean of data.2 ]"
  }
  
  if(type == 'l'){ # Alternate:  mu.x - mu.y < 0
    significance.lower <- qt(alpha, r)
    t.value.sig <- significance.lower
    deci <- (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- pt(test.statistic, r) 
    t.test.type <- "Alternate: Less Than Type [ true mean of data.1 < true mean of data.2 ]"
  }
  
  if(type == 'g'){
    significance.greater <- -qt(alpha, r)
    t.value.sig <- significance.greater
    deci <- (test.statistic >= significance.greater)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 1 - pt(test.statistic, r) 
    t.test.type <- "Alternate: Greater Than Type [ true mean of data.1 > true mean of data.2 ]"
  }
  
  return(list("Two Sample t-test for Equality of Means" = t.test.type, "Welch"=welch, "Level of Significance"=alpha, "Null Hypothesis(H0):"= "True mean of data.1 - True mean of data.2 = 0", "Sample Size of data.1"=n, "Sample Size of data.2"=m,"Sample Mean of data.1"=x.bar, "Sample Mean of data.2"=y.bar, "Sample Standard Deviation of data.1"=s.x, "Sample Standard Deviation of data.2"=s.y, "Pooled Sample Standard Deviation"=s.p, "t-value at the significance threshold" = t.value.sig, "t-value"=test.statistic, "Degrees of Freedom"=r, "p-value"=p.value.test.statistic, "Decision"= decision))
  
}

Hypothesis.Testing.Equality_of_Means.withoutDataset <- function(sample_mean_data.1, sample_mean_data.2, sample_std.1, sample_std.2, sample_size_data.1, sample_size_data.2, type, significance = 0.05, std.1 = -1, std.2 = -1){
  
  
  n <- sample_size_data.1
  m <- sample_size_data.2
  
  if((std.1 == -1 & std.2 == -1) & (n <= 30 | m <= 30)){
    return (t.test.Equality_Of_Means.withoutDataset(sample_mean_data.1, sample_mean_data.2, sample_std.1, sample_std.2, sample_size_data.1, sample_size_data.2, type, significance ))
  }
  
  alpha <- significance
  x.bar <- sample_mean_data.1
  y.bar <- sample_mean_data.2
  
  if(std.1 == -1 & std.2 == -1){
    s.x <- sample_std.1
    s.y <- sample_std.2
    std.known <- "Unknown" 
  }
  else{
    s.x <- std.1
    s.y <- std.2
    std.known <- "Known"
  }
  test.statistic <- (x.bar - y.bar) / sqrt((s.x^2)/n + (s.y^2)/m)
  
  if(type == 't'){
    significance.greater <- -qnorm(alpha/2)
    significance.lower <- qnorm(alpha/2)
    z.value.sig <- list("z-value at lower significance threshold" = significance.lower, "z-value at upper significance threshold" = significance.greater)
    deci <- (test.statistic >= significance.greater) | (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 2*pnorm(-abs(test.statistic)) 
    test.type <- "Alternate: Both-Sided [ true mean of data.1 unequals true mean of data.2 ]"
  }
  if(type == 'l'){
    significance.lower <- qnorm(alpha)
    z.value.sig <- significance.lower
    deci <- (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- pnorm(test.statistic) 
    test.type <- "Alternate: Less Than Type [ true mean of data.1 < true mean of data.2 ]"
  }
  if(type == 'g'){
    significance.greater <- -qnorm(alpha)
    z.value.sig <- significance.greater
    deci <- (test.statistic >= significance.greater)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 1 - pnorm(test.statistic) 
    test.type <- "Alternate: Greater Than Type [ true mean of data.1 > true mean of data.2 ]"
  }
  
  return(list("Two Sample z-test for Equality of Means" = test.type, "Level of Significance"=alpha, "Null Hypothesis(H0):"= "True mean of data.1 - True mean of data.2 = 0", "Sample Size of data.1"=n, "Sample Size of data.2"=m, "Sample Mean of data.1"=x.bar, "Sample Mean of data.2"=y.bar,  "True Standard Deviations" = std.known, "Sample Standard Deviation of data.1"=s.x, "Sample Standard Deviation of data.2"=s.y, "z-value at threshold" = z.value.sig, "z-value"=test.statistic, "p-value"=p.value.test.statistic, "Decision"= decision))
}

# --------------------------------------------------------------------------------------------------------------------------------

Hypothesis.Testing.proportion.withoutDataset <- function(sample_success_count, sample_size, p.0, type, significance = 0.05){
  
  alpha <- significance
  n <- sample_size
  y <- sample_success_count
  test.statistic <- ((y/n) - p.0) / sqrt(p.0*(1-p.0)/n)
  
  if(type == 't'){
    significance.greater <- -qnorm(alpha/2)
    significance.lower <- qnorm(alpha/2)
    z.value.sig <- list("z-value at lower significance threshold" = significance.lower, "z-value at upper significance threshold" = significance.greater)
    deci <- (test.statistic >= significance.greater) | (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 2*pnorm(-abs(test.statistic)) 
    test.type <- "Alternate: Both-Sided"
  }
  if(type == 'l'){
    significance.lower <- qnorm(alpha)
    z.value.sig <- significance.lower
    deci <- (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- pnorm(test.statistic) 
    test.type <- "Alternate: Less Than Type"
  }
  if(type == 'g'){
    significance.greater <- -qnorm(alpha)
    z.value.sig <- significance.greater
    deci <- (test.statistic >= significance.greater)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 1 - pnorm(test.statistic) 
    test.type <- "Alternate: Greater Than Type"
  }
  
  return(list("One Sample Z-test For Proportion" = test.type, "Level of Significance"=alpha, "Null Hypothesis(H0)"=p.0, "Sample Size"=n, "Sample Success Count"=sample_success_count, "Sample Success Proportion"=(y/n), "z-value at threshold" = z.value.sig, "z-value"=test.statistic, "p-value"=p.value.test.statistic, "Decision"= decision))
}

Hypothesis.Testing.proportion.Equality_of_Proportions.withoutDataset <- function(sample_size_data.1, sample_size_data.2, sample_success_count.1, sample_success_count.2, type, significance = 0.05, use.p.estimates = FALSE){
  
  n <- sample_size_data.1
  m <- sample_size_data.2
  
  alpha <- significance
  y1 <- sample_success_count.1
  y2 <- sample_success_count.2
  
  if(use.p.estimates == TRUE){
    p1 <- y1/n
    p2 <- y2/m
    test.statistic <- ((y1/n) - (y2/m)) / sqrt((p1*(1-p1)/n) + (p2*(1-p2)/m))
    pooled.prop <- "Not Required"
  }
  else{
    p <- (y1+y2)/(n+m)
    test.statistic <- ((y1/n) - (y2/m)) / sqrt(p*(1-p)*((1/m)+(1/n)))
    pooled.prop <- p
  }
  
  
  if(type == 't'){
    significance.greater <- -qnorm(alpha/2)
    significance.lower <- qnorm(alpha/2)
    z.value.sig <- list("z-value at lower significance threshold" = significance.lower, "z-value at upper significance threshold" = significance.greater)
    deci <- (test.statistic >= significance.greater) | (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 2*pnorm(-abs(test.statistic)) 
    test.type <- "Alternate: Both-Sided [ success probability of data.1 =/= success probability of data.2 ]"
  }
  if(type == 'l'){
    significance.lower <- qnorm(alpha)
    z.value.sig <- significance.lower
    deci <- (test.statistic <= significance.lower)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- pnorm(test.statistic) 
    test.type <- "Alternate: Less Than Type [ success probability of data.1 < success probability of data.2 ]"
  }
  if(type == 'g'){
    significance.greater <- -qnorm(alpha)
    z.value.sig <- significance.greater
    deci <- (test.statistic >= significance.greater)
    if(deci == TRUE)
      decision <- "Reject Null Hypothesis"
    else
      decision <- "Fail to Reject Null Hypothesis"
    p.value.test.statistic <- 1 - pnorm(test.statistic) 
    test.type <- "Alternate: Greater Than Type [ success probability of data.1 > success probabaility of data.2 ]"
  }
  
  return(list("Two Sample z-test for Equality of Proportions" = test.type, "Using Sample Proportion Estimates"=use.p.estimates, "Level of Significance"=alpha, "Null Hypothesis(H0):"= "Success Probability of data.1 - Success Probability of data.2 = 0", "Sample Size of data.1"=n, "Sample Size of data.2"=m, "Sample Success Count of data.1"=sample_success_count.1, "Sample Sucess Count of data.2"=sample_success_count.2, "Sample Sucess Proportion of data.1"=(y1/n), "Sample Success Proportion of data.2"=(y2/m), "Pooled Success Proportion"=pooled.prop, "z-value at threshold" = z.value.sig, "z-value"=test.statistic, "p-value"=p.value.test.statistic, "Decision"= decision))
  
}

# --------------------------------------------------------------------------------------------------------------------------------

Hypothesis.Testing.chi_sq.goodness_of_fit <- function(category, count_per_category, probability_vect, significance = 0.05, estimates_used = 0){
  
  require(tidyverse)
  
  (data <- tibble(
    categories = category,
    count = count_per_category
  ))
  (n <- sum(data$count))
  (k <- nrow(data))
  (alpha <- significance)
  (p <- probability_vect)
  (data <- data %>%
      mutate(ex = n*p, summands = (count - ex)^2/ex))
  (chi.sq.test.threshold <- qchisq(alpha, k-1-estimates_used, lower.tail = FALSE))
  (chi_sq.test.statistic <- sum(data$summands))
  decision <- chi_sq.test.statistic > chi.sq.test.threshold # Fail to reject Null Hypothesis
  (p.value.ch_sq.test.statistic <- pchisq(chi_sq.test.statistic, k-1-estimates_used, lower.tail = FALSE))
  
  return(list("Data"=data, "number of observations"=n, "number of categories"=k, "significance level"=alpha, "chi sq threshold"=chi.sq.test.threshold, "chi sq test statistic"=chi_sq.test.statistic, "Reject Null Hypothesis"=decision, "p value"=p.value.ch_sq.test.statistic))
}

Hypothesis.Testing.chi_sq.goodness_of_fit.independence <- function(contingency_table, significance = 0.05){
  
  require(tidyverse)
  
  (ds <- as_tibble(contingency_table))
  (Total.Rows <- ds %>%
      transmute(Row.Totals = rowSums(.))
  )
  (Total.Columns <- ds %>%
      summarise_all(sum))
  
  (prob.rows <- Total.Rows %>%
      transmute(Prob.Rows = Row.Totals / sum(ds)))
  
  (prob.columns <- Total.Columns %>%
      mutate_all(~.x/sum(ds)))
  
  (prob.estimate.table <- as.matrix(prob.rows) %*% as.matrix(prob.columns))
  
  (estimated.numbers <- prob.estimate.table * sum(ds))
  
  (data <- as.matrix(contingency_table))
  (summand.matrix <- (data - estimated.numbers)^2 / estimated.numbers)
  (most.contrib <- which(summand.matrix==max(summand.matrix), arr.ind = TRUE))
  
  (k <- nrow(ds)*ncol(ds))
  (alpha <- significance)
  (df <- (k-1) - (ncol(ds) - 1 + nrow(ds) - 1))
  (chi_sq.test.statistic <- sum(summand.matrix))
  (chi.sq.test.threshold <- qchisq(alpha, df, lower.tail = FALSE))
  decision <- chi_sq.test.statistic > chi.sq.test.threshold
  (p.value.ch_sq.test.statistic <- pchisq(chi_sq.test.statistic, df, lower.tail = FALSE))
  
  return(list("Data"=ds, "Total"=sum(ds), "Row Totals"=Total.Rows, "Estimated Row Probabilities"=prob.rows, "Column Totals"=Total.Columns, "Estimated Column Probabilities"=prob.columns, "Expected Table"=estimated.numbers, "significance level"=alpha, "chi sq threshold"=chi.sq.test.threshold, "chi sq test statistic"=chi_sq.test.statistic, "Max. Contributing Element"=most.contrib, "Reject Null Hypothesis"=decision, "p value"=p.value.ch_sq.test.statistic))
}

# --------------------------------------------------------------------------------------------------------------------------------

Hypothesis.Testing.one_factor_anova <- function(data, significance = 0.05){
  require(tidyverse)
  
  colnames(data) <- c("Treat", "Observ")
  (ds <- as_tibble(data))
  
  
  ds <- ds %>%
    filter(!(is.na(Observ))) %>%
    group_by(Treat)
  (n <- nrow(ds))
  
  (X.. <- mean(ds$Observ))
  (Xi. <- ds %>%
      summarise(mean = mean(Observ), count = n()))
  print(Xi.)
  (SST <- sum(Xi.$count * (Xi.$mean - X..)^2))
  
  (m <- nrow(Xi.))
  
  (SSE <- ds %>%
      mutate_at(.vars = vars(Observ), ~(.-mean(., na.rm = TRUE))^2) %>%
      rename("SSE.t" = Observ))
  SSE <- sum(SSE$SSE.t)
  
  (alpha <- significance)
  (test.statistic <- (SST / (m-1)) / (SSE / (n-m)))
  (significance.threshold <- qf(alpha, m-1, n-m, lower.tail = FALSE))
  if (test.statistic > significance.threshold)
    decision <- "Reject Null Hypothesis"
  else
    decision <- "Fail to Reject Null Hypothesis"
  (p.value.test.statistic <- pf(test.statistic, m-1, n-m, lower.tail = FALSE))
  
  ANOVA <- tibble(
    Source = c("Treatment", "Error", "Total"),
    SS = c(SST, SSE, SSE+SST),
    df = c(m-1, n-m, n-1),
    MS = c(SST/(m-1), SSE/(n-m), (SSE+SST)/(n-1)),
    F.statistic = c(test.statistic, NA, NA),
    p_value = c(p.value.test.statistic, NA, NA)
  )
  
  return(list("data"=ds, "No. of Treatment Groups"=m, "Total No. of Observations"=n, "group-wise means"=Xi., "total mean"=X.., "SST"=SST, "SSE"=SSE, "significance"=alpha, "Threshold"=significance.threshold, "F-Statistic"=test.statistic, "ANOVA"=ANOVA, "Decision"=decision))
}

Hypothesis.Testing.two_factor_anova <- function(data, interaction = FALSE){
  my_data <- tibble(data[,1], data[,2], data[,3])
  colnames(my_data) <- c("V1", "V2", "V3")
  
  grand.mean <- mean(my_data$V3)
  (row.means <- my_data %>%
      group_by(V1) %>%
      summarise(row_mean = mean(V3)))
  (col.means <- my_data %>%
      group_by(V2) %>%
      summarise(col_mean = mean(V3)))
  if(interaction == FALSE){
    ds <- my_data %>%
      spread(key = V2, value = V3)

    (my_data <- my_data %>%
        mutate(row_means = row.means$row_mean[V1], col_means = col.means$col_mean[V2], SS.row.summands = (row_means - grand.mean)^2, SS.col.summands = (col_means - grand.mean)^2, SS.error.summands = (V3 - row_means - col_means + grand.mean)^2))
    (SS <- my_data %>%
        summarise(SS.row = sum(SS.row.summands), SS.col = sum(SS.col.summands), SS.err = sum(SS.error.summands)))
    
    (anova.table <- tibble(source = c("SS.row", "SS.col", "SS.err", "SS.tot"), SS = c(SS$SS.row, SS$SS.col, SS$SS.err, SS$SS.row+SS$SS.col+SS$SS.err), df = c(nrow(row.means)-1, nrow(col.means)-1, (nrow(row.means)-1)*(nrow(col.means)-1), nrow(row.means)*nrow(col.means)-1)))
    (anova.table <- anova.table %>%
        mutate(MS = SS/df, F.stat = c(MS[1]/MS[3], MS[2]/MS[3], NA, NA), p = c(pf(F.stat[1], df[1], df[3], lower.tail = FALSE), pf(F.stat[2], df[2], df[3], lower.tail = FALSE), NA, NA)))
  }
  else{
    ds <- my_data
    
    (cell.means <- my_data %>%
       group_by(V1, V2) %>%
       summarize(cell_mean = mean(V3)))
    
    (my_data <- my_data %>%
        mutate(row_means = row.means$row_mean[V1], col_means = col.means$col_mean[V2]) %>%
        full_join(cell.means, by = c("V1", "V2")) %>%
        mutate(SS.row.summands = (row_means - grand.mean)^2, SS.col.summands = (col_means - grand.mean)^2, SS.int.summands = (cell_mean - row_means - col_means + grand.mean)^2, SS.error.summands = (V3 - cell_mean)^2))
    (SS <- my_data %>%
        summarise(SS.row = sum(SS.row.summands), SS.col = sum(SS.col.summands), SS.int = sum(SS.int.summands), SS.err = sum(SS.error.summands)))
    (anova.table <- tibble(source = c("SS.row", "SS.col", "SS.int", "SS.err", "SS.tot"), SS = c(SS$SS.row, SS$SS.col, SS$SS.int, SS$SS.err, SS$SS.row+SS$SS.col+SS$SS.err), df = c(nrow(row.means)-1, nrow(col.means)-1, (nrow(row.means)-1)*(nrow(col.means)-1), nrow(row.means)*nrow(col.means)*(nrow(my_data)/nrow(cell.means) - 1), nrow(my_data)-1)))
    (anova.table <- anova.table %>%
        mutate(MS = SS/df, F.stat = c(MS[1]/MS[4], MS[2]/MS[4], MS[3]/MS[4], NA, NA), p = c(pf(F.stat[1], df[1], df[4], lower.tail = FALSE), pf(F.stat[2], df[2], df[4], lower.tail = FALSE), pf(F.stat[3], df[3], df[4], lower.tail = FALSE), NA, NA)))
  }
  return(list("data"=ds , "No. of Row Groups"=nrow(row.means), "No. of Col Groups"=nrow(col.means), "Total No. of Observations"=nrow(my_data), "row-wise means"=row.means, "col-wise means"=col.means, "grand mean"=grand.mean, "ANOVA"=anova.table))
}
