### Gibbs Sampler

# Formula to evaluate posterior
post.alpha <- function(lambda, theta, alpha){
  r <- lambda / theta
  sum(alpha * log(r) - r - log(gamma(alpha) * lambda))
}

x <- dat[, 1:13]
n <- dim(x)[1]
l <- dim(x)[2]
x.sum <- apply(x, 2, sum)
y <- dat$y1
t <- 1

# Gibbs Sampler
sim <- 1000
lambda <- matrix(0, nrow = sim, ncol = n)
phi <- matrix(0, nrow = sim, ncol = l)
theta <- matrix(0, nrow = sim, ncol = n)
alpha <- rep(0, sim)

phi[1,] <- rep(1, l)
theta[1, ] <- apply(phi[1, ] ^ x, 1, prod)
alpha[1] <- 1


for (i in 2:sim){
  lambda[i, ] <- mapply(function(a, b) rgamma(1, a + alpha[i-1], t + b), y, 1 / theta[i-1, ])

  for (j in 1:l){
    if (j == 1){
      D.j <- apply(phi[i-1, -1] ^ x[, -1], 1, prod)
    } else if (j == l){
      D.j <- apply(phi[i, -l] ^ x[, -l], 1, prod)
    } else {
      D <- cbind(phi[i, 1:(j-1)] ^ x[, 1:(j-1)], phi[i-1, (j+1):l] ^ x[, (j+1):l])
      D.j <- apply(D, 1, prod)
    }
    phi[i, j] <- rigamma(1, alpha[i-1] * x.sum[j] - 1, sum(lambda[i, which(x[, j] == 1)] / D.j[which(x[, j] == 1)]))
  }
  
  theta[i, ] <- apply(phi[i, ] ^ x, 1, prod)
  
  # alpha[i] <- prod(sapply(lambda[i, ]/theta[i, ], function(x) rpois(1, x)))
  alpha.grid <- ppoints(100)*0.2 + (alpha[i-1] - 0.1)
  post.e <- sapply(alpha.grid, function(x) post.alpha(lambda[i,], theta[i, ], x))
  post <- exp(post.e - max(post.e))
  prob.a <- post/sum(post)
  alpha[i] <- sample(alpha.grid, 1, prob = prob.a)
}

par(mfrow=c(3,2))
for (i in 7:12){
  plot(alpha[500:1000], log(phi[500:1000,i]), xlab = "alpha", ylab = paste("beta",names(x)[i]))
}

par(mfrow=c(2,7))
hist(alpha[500:1000], xlab = "alpha", main = "Histogram of alpha")
for (i in 1:13){
  hist(log(phi[500:1000,i]), xlab = paste("beta",names(x)[i]), main = paste("Histogram of beta", names(x)[i]))
}

summary(alpha[500:1000])
summary(log(phi[500:1000,]))



names(gs) <- c("alpha", "beta1", "startpoint")
gs$alpha <- c(alpha.1, alpha.4)
gs$beta1 <- c(beta.1, beta.4)

ggplot(gs, aes(x = alpha, y = beta1, colour = startpoint)) + geom_line()
