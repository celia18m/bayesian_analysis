# Metropolis hasting
x <- scale(df[1:100,-c(5,6,7)])
n <- dim(x)[1]
l <- dim(x)[2]
t <- 1
y <- df$user_ordnum_1month[1:100]

start.glm <- glm(y ~ x, family = poisson(link = log))

logpost.all <- function(lambda, alpha, theta){
  sum((y + alpha - 1) * log(lambda) - (t + 1/theta) * lambda - log(gamma(alpha)) - alpha * log(theta))
}

alpha.jumping <- function(alpha){
  runif(1, max(0, alpha - 0.5), alpha + 0.5)
}

beta.jumping <- function(beta){
  # k <- length(beta)
  k <- 5
  newbeta <- rep(0, k)
  newbeta[1] <- runif(1, beta[1] - 0.3, beta[1] + 0.3)
  for (i in 2:k){
    newbeta[i] <- runif(1, beta[i] - 0.1, beta[i] + 0.1)
  }
  newbeta
}

sim <- 1000000
lambda <- matrix(0, nrow = sim, ncol = n)
alpha <- rep(0, sim)
theta <- matrix(0, nrow = sim, ncol = n)
beta <- matrix(0, nrow = sim, ncol = (l + 1))

accept <- rep(0, sim)

# setting starting point
beta[1, ] <- start.glm$coefficients
alpha[1] <- 3
theta[1,] <- exp(beta[1, ] %*% t(cbind(1,x)))

for (i in 2:sim){
  alpha.star <- alpha.jumping(alpha[i-1])
  beta.star <- beta.jumping(beta[i-1, ])
  theta.star <- exp(beta.star %*% t(cbind(1,x)))
  lambda.star <- sapply(1/theta.star, function(x) rgamma(1, alpha.star, x))
  r <- exp(logpost.all(lambda.star, alpha.star, theta.star) - logpost.all(lambda[i-1], alpha[i-1], theta[i-1, ]))
  prob <- min(r, 1)
  if (prob > runif(1))
  {
    alpha[i] <- alpha.star
    beta[i, ] <- beta.star
    theta[i, ] <- theta.star
    lambda[i, ] <- lambda.star
    accept[i] <- 1
  }
  else
  {
    alpha[i] <- alpha[i-1]
    beta[i, ] <- beta[i-1, ]
    theta[i, ] <- theta[i-1, ]
    lambda[i, ] <- lambda[i-1, ]
  }
}

ind <- which(accept==1)
ind <- ind[900:1900]

par(mfrow=c(3,2))
for (i in 1:6){
  plot(alpha[ind], beta[ind, i], pch = ".", xlab = "alpha", ylab = paste("beta", i))
}

par(mfrow=c(1,1))
hist(alpha[ind], main = "Histogram of alpha", xlab = "alpha")

par(mfrow=c(2,3))
for (i in 1:6){
  hist(beta[ind, i], main = paste("Histogram of beta", i), xlab = paste("beta", i))
}
