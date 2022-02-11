Confidence.Interval.t <- function(data, type = 't', significance = 0.05){
  
  n <- length(data[,1])
  alpha <- significance
  x.bar <- mean(data[,1]) 
  s <- sd(data[,1])
  
  if(type == 't'){
    significance.greater <- -qt(alpha/2, n-1)
    significance.lower <- qt(alpha/2, n-1)
    conf.upper <- x.bar + (significance.greater*s/sqrt(n))
    conf.lower <- x.bar + (significance.lower*s/sqrt(n))
    conf.interval <- c(conf.lower, conf.upper)
  }
  if(type == 'l'){
    significance.lower <- qt(alpha, n-1)
    conf.lower <- x.bar + (significance.lower*s/sqrt(n))
    conf.interval <- c(conf.lower, Inf)
  }
  if(type == 'g'){ # Generates Upper Bound for True Mean
    significance.greater <- -qt(alpha, n-1)
    conf.upper <- x.bar + (significance.greater*s/sqrt(n))
    conf.interval <- c(-Inf, conf.upper)
  }
  
  return(list("Confidence Interval" = "Using t-distribution", "Level of Significance"=alpha, "Sample Size"=n, "Sample Mean"=x.bar, "Sample Standard Deviation"=s, "Confidence Interval" = conf.interval, "Confidence Level"=(1-alpha)*100))
}

Confidence.Interval <- function(data, std=-1, type = "t", significance = 0.05){
  
  n <- length(data[,1])
  
  if(std == -1 & n < 30)
    return(Confidence.Interval.t(data, type, significance))
  
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
  
  if(type == 't'){
    significance.greater <- -qnorm(alpha/2)
    significance.lower <- qnorm(alpha/2)
    conf.upper <- x.bar + (significance.greater*s/sqrt(n))
    conf.lower <- x.bar + (significance.lower*s/sqrt(n))
    conf.interval <- c(conf.lower, conf.upper)
  }
  if(type == 'l'){
    significance.lower <- qnorm(alpha)
    conf.lower <- x.bar + (significance.lower*s/sqrt(n))
    conf.interval <- c(conf.lower, Inf)
  }
  if(type == 'g'){ # Generates Upper Bound for True Mean
    significance.greater <- -qnorm(alpha)
    conf.upper <- x.bar + (significance.greater*s/sqrt(n))
    conf.interval <- c(-Inf, conf.upper)
  }
  
  return(list("Confidence Interval" = "Using Z-distribution", "Level of Significance"=alpha, "Sample Size"=n, "Sample Mean"=x.bar, "True Standard Deviation"=std.known, "Sample Standard Deviation"=s, "Confidence Interval" = conf.interval, "Confidence Level"=(1-alpha)*100))
  
}

Confidence.Interval.t.withoutDataset <- function(sample_mean, sample_std, sample_size, type = 't', significance = 0.05){
  
  n <- sample_size
  alpha <- significance
  x.bar <- sample_mean 
  s <- sample_std
  
  if(type == 't'){
    significance.greater <- -qt(alpha/2, n-1)
    significance.lower <- qt(alpha/2, n-1)
    conf.upper <- x.bar + (significance.greater*s/sqrt(n))
    conf.lower <- x.bar + (significance.lower*s/sqrt(n))
    conf.interval <- c(conf.lower, conf.upper)
  }
  if(type == 'l'){
    significance.lower <- qt(alpha, n-1)
    conf.lower <- x.bar + (significance.lower*s/sqrt(n))
    conf.interval <- c(conf.lower, Inf)
  }
  if(type == 'g'){ # Generates Upper Bound for True Mean
    significance.greater <- -qt(alpha, n-1)
    conf.upper <- x.bar + (significance.greater*s/sqrt(n))
    conf.interval <- c(-Inf, conf.upper)
  }
  
  return(list("Confidence Interval" = "Using t-distribution", "Level of Significance"=alpha, "Sample Size"=n, "Sample Mean"=x.bar, "Sample Standard Deviation"=s, "Confidence Interval" = conf.interval, "Confidence Level"=(1-alpha)*100))
}

Confidence.Interval.withoutDataset <- function(sample_mean, sample_std = -1, sample_size, std=-1, type = "t", significance = 0.05){
  
  n <- sample_size
  
  if(std == -1 & n < 30)
    return(Confidence.Interval.t.withoutDataset(sample_mean, sample_std, sample_size, type, significance))
  
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
  
  if(type == 't'){
    significance.greater <- -qnorm(alpha/2)
    significance.lower <- qnorm(alpha/2)
    conf.upper <- x.bar + (significance.greater*s/sqrt(n))
    conf.lower <- x.bar + (significance.lower*s/sqrt(n))
    conf.interval <- c(conf.lower, conf.upper)
  }
  if(type == 'l'){
    significance.lower <- qnorm(alpha)
    conf.lower <- x.bar + (significance.lower*s/sqrt(n))
    conf.interval <- c(conf.lower, Inf)
  }
  if(type == 'g'){ # Generates Upper Bound for True Mean
    significance.greater <- -qnorm(alpha)
    conf.upper <- x.bar + (significance.greater*s/sqrt(n))
    conf.interval <- c(-Inf, conf.upper)
  }
  
  return(list("Confidence Interval" = "Using Z-distribution", "Level of Significance"=alpha, "Sample Size"=n, "Sample Mean"=x.bar, "True Standard Deviation"=std.known, "Sample Standard Deviation"=s, "Confidence Interval" = conf.interval, "Confidence Level"=(1-alpha)*100))
  
}

# -------------------------------------------------------------------------------------------------------------------------------------

Confidence.Interval.t.Difference_Of_Means <- function(data.1, data.2, type = 't', significance = 0.05, welch = FALSE){
  
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
    s.w <- sqrt((s.x^2)/n + (s.y^2)/m)
    s.p <- "Not Required"
  }
  else{
    r <- n+m-2
    s.p <- sqrt(((n-1)*(s.x^2) + (m-1)*(s.y^2)) / (r) ) 
    s.w <- (s.p*sqrt((1/n)+(1/m)))
  }
  
  w.bar <- x.bar - y.bar
  
  if(type == 't'){
    significance.greater <- -qt(alpha/2, r)
    significance.lower <- qt(alpha/2, r)
    
    conf.lower <- w.bar + significance.lower*s.w
    conf.upper <- w.bar + significance.greater*s.w
    conf.interval <- c(conf.lower, conf.upper)
  }
  
  if(type == 'l'){ 
    significance.lower <- qt(alpha, r)
    
    conf.lower <- w.bar + significance.lower*s.w
    conf.interval <- c(conf.lower, Inf)
    
  }
  
  if(type == 'g'){
    significance.greater <- -qt(alpha, r)
    
    conf.upper <- w.bar + significance.greater*s.w
    conf.interval <- c(-Inf, conf.upper)
  }
  
  return(list("Confidence Interval for Difference of Means" = "Using t-distribution", "Welch"=welch, "Degrees of Freedom"=r, "Level of Significance"=alpha, "Sample Size of data.1"=n, "Sample Size of data.2"=m,"Sample Mean of data.1"=x.bar, "Sample Mean of data.2"=y.bar, "Sample Standard Deviation of data.1"=s.x, "Sample Standard Deviation of data.2"=s.y, "Pooled Sample Standard Deviation"=s.p, "Difference between 2 sample means"=w.bar, "Confidence Interval" = conf.interval, "Confidence Level"=(1-alpha)*100))
  
}

Confidence.Interval.Difference_of_Means <- function(data.1, data.2, type = 't', significance = 0.05, std.1 = -1, std.2 = -1){
  
  
  n <- length(data.1[,1])
  m <- length(data.2[,1])
  
  if((std.1 == -1 & std.2 == -1) & (n < 30 & m < 30)){
    return (Confidence.Interval.t.Difference_Of_Means(data.1, data.2, type, significance))
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
    std.known <- "Known [The Sample Std mentioned below are the True Std deviations]"
  }
  
  s.w <- sqrt((s.x^2/n)+(s.y^2/m))
  w.bar <- x.bar - y.bar
  
  if(type == 't'){
    significance.greater <- -qnorm(alpha/2)
    significance.lower <- qnorm(alpha/2)
    
    conf.upper <- w.bar + significance.greater*s.w
    conf.lower <- w.bar + significance.lower*s.w
    conf.interval <- c(conf.lower, conf.upper)
  }
  if(type == 'l'){
    significance.lower <- qnorm(alpha)
    
    conf.lower <- w.bar + s.w*significance.lower
    conf.interval <- c(conf.lower, Inf)
  }
  if(type == 'g'){
    significance.greater <- -qnorm(alpha)
    
    conf.upper <- w.bar + s.w*significance.greater
    conf.interval <- c(-Inf, conf.upper)
  }
  
  return(list("Confidence Interval For Difference of Means" = "Using Z-distribution", "Level of Significance"=alpha, "Sample Size of data.1"=n, "Sample Size of data.2"=m, "Sample Mean of data.1"=x.bar, "Sample Mean of data.2"=y.bar, "True Standard Deviations"=std.known, "Sample Standard Deviation of data.1"=s.x, "Samle Standard Deviation of data.2"=s.y, "Difference between 2 sample means"=w.bar, "Confidence Interval" = conf.interval, "Confidence Level"=(1-alpha)*100))
}

Confidence.Interval.t.Difference_Of_Means.withoutDataset <- function(sample_mean.1, sample_mean.2, sample_std.1, sample_std.2, sample_size.1, sample_size.2, type = 't', significance = 0.05, welch = FALSE){
  
  n <- sample_size.1
  m <- sample_size.2
  alpha <- significance
  x.bar <- sample_mean.1
  y.bar <- sample_mean.2
  s.x <- sample_std.1
  s.y <- sample_std.2
  if (welch == TRUE){
    r <-  ((s.x^2 /n)+(s.y^2 /m))^2 / (((s.x^2 /n)^2 /(n-1)) + ((s.y^2 /m)^2 / (m-1)))
    r <- round(r)
    s.w <- sqrt((s.x^2)/n + (s.y^2)/m)
    s.p <- "Not Required"
  }
  else{
    r <- n+m-2
    s.p <- sqrt(((n-1)*(s.x^2) + (m-1)*(s.y^2)) / (r) ) 
    s.w <- (s.p*sqrt((1/n)+(1/m)))
  }
  
  w.bar <- x.bar - y.bar
  
  if(type == 't'){
    significance.greater <- -qt(alpha/2, r)
    significance.lower <- qt(alpha/2, r)
    
    conf.lower <- w.bar + significance.lower*s.w
    conf.upper <- w.bar + significance.greater*s.w
    conf.interval <- c(conf.lower, conf.upper)
  }
  
  if(type == 'l'){ 
    significance.lower <- qt(alpha, r)
    
    conf.lower <- w.bar + significance.lower*s.w
    conf.interval <- c(conf.lower, Inf)
    
  }
  
  if(type == 'g'){
    significance.greater <- -qt(alpha, r)
    
    conf.upper <- w.bar + significance.greaterr*s.w
    conf.interval <- c(-Inf, conf.upper)
  }
  
  return(list("Confidence Interval for Difference of Means" = "Using t-distribution", "Welch"=welch, "Degrees of Freedom"=r, "Level of Significance"=alpha, "Sample Size of data.1"=n, "Sample Size of data.2"=m,"Sample Mean of data.1"=x.bar, "Sample Mean of data.2"=y.bar, "Sample Standard Deviation of data.1"=s.x, "Sample Standard Deviation of data.2"=s.y, "Pooled Sample Standard Deviation"=s.p, "Difference between 2 sample means"=w.bar, "Confidence Interval" = conf.interval, "Confidence Level"=(1-alpha)*100))
  
}

Confidence.Interval.Difference_of_Means.withoutDataset <- function(sample_mean.1, sample_mean.2, sample_std.1, sample_std.2, sample_size.1, sample_size.2, std.1 = -1, std.2 = -1, type = "t", significance = 0.05){
  
  n <- sample_size.1
  m <- sample_size.2
  
  if((std.1 == -1 & std.2 == -1) & (n < 30 & m < 30)){
    return (Confidence.Interval.t.Difference_Of_Means.withoutDataset(sample_mean.1, sample_mean.2, sample_std.1, sample_std.2, sample_size.1, sample_size.2, type, significance))
  }
  
  alpha <- significance
  x.bar <- sample_mean.1
  y.bar <- sample_mean.2
  
  if(std.1 == -1 & std.2 == -1){
    s.x <- sample_std.1
    s.y <- sample_std.2
    std.known <- "Unknown" 
  }
  else{
    s.x <- std.1
    s.y <- std.2
    std.known <- "Known [The Sample Std mentioned below are the True Std deviations]"
  }
  
  s.w <- sqrt((s.x^2/n)+(s.y^2/m))
  w.bar <- x.bar - y.bar
  
  if(type == 't'){
    significance.greater <- -qnorm(alpha/2)
    significance.lower <- qnorm(alpha/2)
    
    conf.upper <- w.bar + significance.greater*s.w
    conf.lower <- w.bar + significance.lower*s.w
    conf.interval <- c(conf.lower, conf.upper)
  }
  if(type == 'l'){
    significance.lower <- qnorm(alpha)
    
    conf.lower <- w.bar + s.w*significance.lower
    conf.interval <- c(conf.lower, Inf)
  }
  if(type == 'g'){
    significance.greater <- -qnorm(alpha)
    
    conf.upper <- w.bar + s.w*significance.greater
    conf.interval <- c(-Inf, conf.upper)
  }
  
  return(list("Confidence Interval For Difference of Means" = "Using Z-distribution", "Level of Significance"=alpha, "Sample Size of data.1"=n, "Sample Size of data.2"=m, "Sample Mean of data.1"=x.bar, "Sample Mean of data.2"=y.bar, "True Standard Deviations"=std.known, "Sample Standard Deviation of data.1"=s.x, "Samle Standard Deviation of data.2"=s.y, "Difference between 2 sample means"=w.bar, "Confidence Interval" = conf.interval, "Confidence Level"=(1-alpha)*100))
}

# -------------------------------------------------------------------------------------------------------------------------------------

Confidence.Interval.Proportion.withoutDataset <- function(sample_success_count, sample_size, type ='t', significance = 0.05, success.low.or.size.low = FALSE, Approx = TRUE){
  
  alpha <- significance
  n <- sample_size
  y <- sample_success_count
  if(success.low.or.size.low == TRUE){
    p.hat <- (y+2)/(n+4)
    w <- sqrt(p.hat*(1-p.hat)/(n+4))
  }
  else{
    p.hat <- y/n
    w <- sqrt(p.hat*(1-p.hat)/n)
  }
  
  if(type == 't'){
    significance.greater <- -qnorm(alpha/2)
    significance.lower <- qnorm(alpha/2)
    if(Approx == FALSE){
      conf.upper <- (p.hat + ((significance.greater^2)/(2*n)) + significance.greater*sqrt(w^2 + ((significance.greater^2)/(4*n^2)))) / (1+((significance.greater^2)/n))
      conf.lower <- (p.hat + ((significance.lower^2)/(2*n)) + significance.lower*sqrt(w^2 + ((significance.lower^2)/(4*n^2)))) / (1+((significance.lower^2)/n))
    }
    else{
      conf.upper <- p.hat + significance.greater*w
      conf.lower <- p.hat + significance.lower*w
    }
    conf.interval <- c(conf.lower, conf.upper)
  }
  if(type == 'l'){
    significance.lower <- qnorm(alpha)
    if(Approx == FALSE)
      conf.lower <- (p.hat + ((significance.lower^2)/(2*n)) + significance.lower*sqrt(w^2 + ((significance.lower^2)/(4*n^2)))) / (1+((significance.lower^2)/n))
    else
      conf.lower <- p.hat + significance.lower*w

    conf.interval <- c(conf.lower, 1)
    
  }
  if(type == 'g'){
    significance.greater <- -qnorm(alpha)
    if(Approx == FALSE)
      conf.upper <- (p.hat + ((significance.greater^2)/(2*n)) + significance.greater*sqrt(w^2 + ((significance.greater^2)/(4*n^2)))) / (1+((significance.greater^2)/n))
    else
      conf.upper <- p.hat + significance.greater*w

    conf.interval <- c(0, conf.upper)
  }
  
  return(list("Confidence Interval for Sample Proportion" = "Using Z-distribution", "Using Approximation" = Approx, "Level of Significance"=alpha, "Sample Size"=n, "Sample Success Count"=sample_success_count, "Sample Success Proportion"=(y/n), "Sample Size or Success or Failure Count Low"=success.low.or.size.low, "Confidence Interval"=conf.interval, "Confidence"=100*(1-alpha)))
  
  
}

Confidence.Interval.Difference_of_Proportions.withoutDataset <- function(sample_size_data.1, sample_size_data.2, sample_success_count.1, sample_success_count.2, type = 't', significance = 0.05){
  
  n <- sample_size_data.1
  m <- sample_size_data.2
  
  alpha <- significance
  y1 <- sample_success_count.1
  y2 <- sample_success_count.2
  
  
  p1 <- y1/n
  p2 <- y2/m
  w.bar <- ((y1/n) - (y2/m))
  w <- sqrt((p1*(1-p1)/n) + (p2*(1-p2)/m))
    
  
  
  
  if(type == 't'){
    significance.greater <- -qnorm(alpha/2)
    significance.lower <- qnorm(alpha/2)
    conf.upper <- w.bar + significance.greater*w
    conf.lower <- w.bar + significance.lower*w
    conf.interval <- c(conf.lower, conf.upper)
  }
  if(type == 'l'){
    significance.lower <- qnorm(alpha)
    conf.lower <- w.bar + significance.lower*w
    conf.interval <- c(conf.lower, 1)
    
  }
  if(type == 'g'){
    significance.greater <- -qnorm(alpha)
    conf.upper <- w.bar + significance.greater*w
    conf.interval <- c(-1, conf.upper)    
  }
  
  return(list("Confidence Interval for Difference of 2 proportions" = "Using Z-distribution", "Level of Significance"=alpha, "Sample Size of data.1"=n, "Sample Size of data.2"=m, "Sample Success Count of data.1"=sample_success_count.1, "Sample Sucess Count of data.2"=sample_success_count.2, "Sample Sucess Proportion of data.1"=(y1/n), "Sample Success Proportion of data.2"=(y2/m), "Difference in the Sample Proportions[data.1 - data.2]"=(y1/n)-(y2/m), "Confidence Interval"=conf.interval, "Confidence"=100*(1-alpha)))
  
}

# -------------------------------------------------------------------------------------------------------------------------------------

Confidence.Interval.Simple.Linear.Model <- function(data, x, significance = 0.05, type = 't'){
  
  model <- Simple.Linear.Regression(data)
  alpha.hat <- model$alpha_MLE
  beta.hat <- model$beta_MLE
  sigma_2.hat <- model$sigma_2.hat
  Sxx <- sum((data[,1] - mean(data[,1]))^2)
  
  n <- length(data[,1])
  alpha <- significance
  x.bar <- mean(data[,1]) 
  s <- sd(data[,1])
  
  if(type == 't'){
    significance.greater <- -qt(alpha/2, n-2)
    significance.lower <- qt(alpha/2, n-2)
    conf.upper <- alpha.hat + beta.hat*(x-x.bar) + significance.greater * sqrt((1/n) + ((x-x.bar)^2/Sxx)) * sqrt(n*sigma_2.hat/(n-2)) 
    conf.lower <- alpha.hat + beta.hat*(x-x.bar) - significance.greater * sqrt((1/n) + ((x-x.bar)^2/Sxx)) * sqrt(n*sigma_2.hat/(n-2)) 
    conf.interval <- c(conf.lower, conf.upper)
  }
  if(type == 'l'){
    significance.lower <- qt(alpha, n-2)
    conf.lower <- alpha.hat + beta.hat*(x-x.bar) - significance.greater * sqrt((1/n) + ((x-x.bar)^2/Sxx)) * sqrt(n*sigma_2.hat/(n-2))
    conf.interval <- c(conf.lower, Inf)
  }
  if(type == 'g'){ # Generates Upper Bound for True Mean
    significance.greater <- -qt(alpha, n-2)
    conf.upper <- alpha.hat + beta.hat*(x-x.bar) + significance.greater * sqrt((1/n) + ((x-x.bar)^2/Sxx)) * sqrt(n*sigma_2.hat/(n-2)) 
    conf.interval <- c(-Inf, conf.upper)
  }
  
  return(list("Confidence Interval for mu(x) Simple Linear Model Mean" = "Using t-distribution", "Level of Significance"=alpha, "No. of Data Points"=n, "Confidence Interval" = conf.interval, "Confidence Level"=(1-alpha)*100))
}

Prediction.Interval.Simple.Linear.Model <- function(data, x, significance = 0.05, type = 't'){
  
  model <- Simple.Linear.Regression(data)
  alpha.hat <- model$alpha_MLE
  beta.hat <- model$beta_MLE
  sigma_2.hat <- model$sigma_2.hat
  Sxx <- sum((data[,1] - mean(data[,1]))^2)
  
  n <- length(data[,1])
  alpha <- significance
  x.bar <- mean(data[,1]) 
  s <- sd(data[,1])
  
  if(type == 't'){
    significance.greater <- -qt(alpha/2, n-2)
    significance.lower <- qt(alpha/2, n-2)
    conf.upper <- alpha.hat + beta.hat*(x-x.bar) + significance.greater * sqrt(1 + (1/n) + ((x-x.bar)^2/Sxx)) * sqrt(n*sigma_2.hat/(n-2)) 
    conf.lower <- alpha.hat + beta.hat*(x-x.bar) - significance.greater * sqrt(1 + (1/n) + ((x-x.bar)^2/Sxx)) * sqrt(n*sigma_2.hat/(n-2)) 
    conf.interval <- c(conf.lower, conf.upper)
  }
  if(type == 'l'){
    significance.lower <- qt(alpha, n-2)
    conf.lower <- alpha.hat + beta.hat*(x-x.bar) - significance.greater * sqrt(1 + (1/n) + ((x-x.bar)^2/Sxx)) * sqrt(n*sigma_2.hat/(n-2))
    conf.interval <- c(conf.lower, Inf)
  }
  if(type == 'g'){ # Generates Upper Bound for True Mean
    significance.greater <- -qt(alpha, n-2)
    conf.upper <- alpha.hat + beta.hat*(x-x.bar) + significance.greater * sqrt(1 + (1/n) + ((x-x.bar)^2/Sxx)) * sqrt(n*sigma_2.hat/(n-2)) 
    conf.interval <- c(-Inf, conf.upper)
  }
  
  return(list("Confidence Interval for mu(x) Simple Linear Model Mean" = "Using t-distribution", "Level of Significance"=alpha, "No. of Data Points"=n, "Confidence Interval" = conf.interval, "Confidence Level"=(1-alpha)*100))
}
