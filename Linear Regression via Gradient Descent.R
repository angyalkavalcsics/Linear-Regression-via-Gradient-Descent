library(datasets)
mtcars <- scale(mtcars)
# add intercept column
mtcars.x <- cbind(1, mtcars[,-1])
mtcars.y <- mtcars[,1]

# Function computes the partial derivative with
# respect to betak 
partial.beta_k <- function(curr, Y, X){
  n <- length(Y)
  error <- (X%*%curr - Y)
  partial <- (1/n) * (t(X) %*% error)
  return(partial)
}

# recall that eps is our tolerance and 
# alpha is our step size
GD <- function(Y, X, eps = 1e-10, alpha = 0.01){
  y <- Y
  dat <- X
  beta_k <- c(rep(0, ncol(dat)))
  beta_kplus1 <- beta_k + 1
  # number of iterations
  ct <- 0
  while (mean(abs(beta_k - beta_kplus1)) > eps) {
    # update count
    ct <- ct + 1
    # Find partial derivatives
    DBk <- partial.betak(curr = beta_kplus1, Y = y, X = dat)
    # Update parameter vector
    beta_k <- beta_kplus1
    beta_kplus1 <- beta_k - alpha*DBk
  }
  return(list("coefficients" = beta_kplus1, "iter" = ct))
} 
mtcars_by_GD <- GD(Y = mtcars.y, X = mtcars.x, eps = 1e-10, alpha = 0.01)
mtcars_by_GD$coefficients
mtcars_by_GD$iter
# Check:
df <- as.data.frame(mtcars)
mod <- lm(mpg ~ ., data = df)
mod$coefficients

