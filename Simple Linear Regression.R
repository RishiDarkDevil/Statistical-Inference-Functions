# Input: data= Dataframe that contains first column as the independent variable X and second column as the dependent
# variable Y.
# Output: Fits Linear Regression Line and plots the figure 

Simple.Linear.Regression <- function(data){
  par(mfrow = c(2,1))
  plot(data, main = "Data & Fitted Line")
  Y <- data[,2]
  x <- data[,1]
  alpha.hat <- mean(Y)
  beta.hat <- sum(Y*(x-mean(x))) / sum((x-mean(x))^2)
  x.bar <- mean(x)
  # (a)
  alpha.hat
  beta.hat
  # (b)
  fitted.line <- function(alpha.hat, beta.hat, x) alpha.hat + beta.hat*(x - x.bar)
  x.test <- seq(min(x), max(x), by = (max(x)-min(x))/length(x))
  y.fit <- fitted.line(alpha.hat, beta.hat, x.test)
  plot.data <- cbind(x.test, y.fit)
  lines(plot.data, type = 'l')
  # (c)
  sigma_2.hat <- sum((Y - fitted.line(alpha.hat, beta.hat, x))^2) / length(x)
  sigma_2.hat
  residual <- Y - fitted.line(alpha.hat, beta.hat, x)
  plot(x, residual, main = "Residual Plot")
  lines(x, numeric(length(x)))
  par(mfrow = c(1,1))
  return(list("Simple Linear Regression"="Model: Y_i = alpha + beta.(x_i - x.bar) + e_i", "Number of Data Points"=length(x), "alpha_MLE"=alpha.hat, "beta_MLE"=beta.hat, "sigma_2.hat"=sigma_2.hat))
}
