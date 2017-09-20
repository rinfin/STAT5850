#by kevin.lee
set.seed(920)

# sample mean is unbiased estimator of population mean

M <- 100 # number of simulation
N <- 30 # number of samples for each run

samp_mean <- rep(0,M) # a vector that will save the sample mean of each run

# sample from Normal dsitribution
for (m in 1:M) {
  samp <- rnorm(N, mean = 0, sd = 1) # sample generated from N(0,1)
  samp_mean[m] <- mean(samp)
}

samp_mean
hist(samp_mean, main = "Histogram of sample mean", xlab = "sample mean")
mean(samp_mean) # check if close to 0

# sample from Uniform distribution
for (m in 1:M) {
  samp <- runif(N, 0, 1) # sample generated from U(0,1)
  samp_mean[m] <- mean(samp)
}

samp_mean
hist(samp_mean, main = "Histogram of sample mean", xlab = "sample mean")
mean(samp_mean) # check if close to 1/2


# least squares coefficient estimates are unbiased estimator of population coefficients
# population regression line, Y = 1 + 2*X. Hence, population intercept is 1 and population slope is 2.
# error's iid N(0, 1)
M <- 100 # number of simulation
N <- 30 # number of samples for each run

betahat1 <- rep(0,M) # a vector that will save the betahat1 of each run
betahat0 <- rep(0,M) # a vector that will save the betahat1 of each run

for (m in 1:M) {
  X <- runif(N, min = 0, max = 1) # X generated from U(0,1)
  error <- rnorm(N, mean = 0, sd = 1) # error generated from N(0,1)
  Y <- 1 + 2*X + error

  fit <- lm(Y~X)
  betahat1[m] <- fit$coefficients[2]
  betahat0[m] <- fit$coefficients[1]
}

# check last simultion result
plot(X, Y)
abline(fit)

names(fit)
fit$coefficients

betahat1
hist(betahat1, main = expression(paste("Histogram of ", hat(beta)[1])), xlab = expression(hat(beta)[1]))
mean(betahat1) # check if close to 2

betahat0
hist(betahat0, main = expression(paste("Histogram of ", hat(beta)[0])), xlab = expression(hat(beta)[0]))
mean(betahat0) # check if close to 0


###########

# See how the number of samples affects the result of single estimate of sample mean

sim_mean <- function(M, N, Xmean, Xvar) {
  samp_mean <- rep(0,M)
  for (m in 1:M) {
    samp <- rnorm(N, Xmean, sqrt(Xvar)) # sample generated from N(mean,sd)
    samp_mean[m] <- mean(samp)
  }
  return(samp_mean)
}

# number of samples for each run 30
sim1 <- sim_mean(100, 30, 0, 1)
sim1
hist(sim1, main = "N=30, N(0,1)", xlab = "sample mean")
mean(sim1)
var(sim1)

# number of samples for each run 300
sim2 <- sim_mean(100, 300, 0, 1)
sim2
hist(sim2, main = "N=300, N(0,1)", xlab = "sample mean")
mean(sim2)
var(sim2)

# ideally when the error variace was small
sim3 <- sim_mean(100, 30, 0, 0.1)
hist(sim3, main = "N=30, N(0,0.1)", xlab = "sample mean")
mean(sim3)
var(sim3)

# Plot three histograms at the same time
plot_range <- range(c(sim1, sim2, sim3)) # to take same range for three plots

par(mfrow=c(1,3))
hist(sim1, main = "N=30, N(0,1)", xlim = plot_range, xlab = "sample mean")
hist(sim2, main = "N=300, N(0,1)", xlim = plot_range, xlab = "sample mean")
hist(sim3, main = "N=30, N(0,0.1)", xlim = plot_range, xlab = "sample mean")


# See how the number of samples affects the result of single estimate of least squares coefficient estimaete

sim_betahat1 <- function(M, N, Xmin, Xmax, err_mean, err_var) {
  betahat1 <- rep(0,M) # a vector that will save the betahat1 of each run
  for (m in 1:M) {
    X <- runif(N, min = Xmin, max = Xmax) # X generated from U(Xmin,Xmax)
    error <- rnorm(N, mean = err_mean, sd = sqrt(err_var)) # error generated from N(err_mean, err_sd)
    Y <- 1 + 2*X + error
    fit <- lm(Y~X)
    betahat1[m] <- fit$coefficients[2]
  }
  return(betahat1)
}

sim_betahat0 <- function(M, N, Xmin, Xmax, err_mean, err_var) {
  betahat0 <- rep(0,M) # a vector that will save the betahat1 of each run
  for (m in 1:M) {
    X <- runif(N, min = Xmin, max = Xmax) # X generated from U(Xmin,Xmax)
    error <- rnorm(N, mean = err_mean, sd = sqrt(err_var)) # error generated from N(err_mean, err_sd)
    Y <- 1 + 2*X + error
    fit <- lm(Y~X)
    betahat0[m] <- fit$coefficients[1]
  }
  return(betahat0)
}


# number of samples for each run 30
sim1 <- sim_betahat1(100, 30, 0, 1, 0, 1)
hist(sim1, main = "N=30, X~U(0,1), err~N(0,1)", xlab=expression(hat(beta)[1]))
mean(sim1)
var(sim1)

# number of samples for each run 300
sim2 <- sim_betahat1(100, 300, 0, 1, 0, 1)
hist(sim2, main = "N=300, X~U(0,1), err~N(0,1)", xlab=expression(hat(beta)[1]))
mean(sim2)
var(sim2)

# ideally when the error variace was small
sim3 <- sim_betahat1(100, 30, 0, 1, 0, 0.1)
hist(sim3, main = "N=30, X~U(0,1), err~N(0,0.1)", xlab=expression(hat(beta)[1]))
mean(sim3)
var(sim3)

# x_{i}'s are less spread out
sim4 <- sim_betahat1(100, 30, 0.2, 0.8, 0, 1)
hist(sim4, main = "N=30, X~U(0.2,0.8), err~N(0,1)", xlab=expression(hat(beta)[1]))
mean(sim4)
var(sim4)

# Plot four histograms at the same time
plot_range <- range(c(sim1, sim2, sim3, sim4)) # to take same range for three plots

par(mfrow=c(2,2))
hist(sim1, main = "N=30, X~U(0,1), err~N(0,1)", xlim = plot_range, xlab = expression(hat(beta)[1]))
hist(sim2, main = "N=300, X~U(0,1), err~N(0,1)", xlim = plot_range, xlab = expression(hat(beta)[1]))
hist(sim3, main = "N=30, X~U(0,1), err~N(0,0.1)", xlim = plot_range, xlab = expression(hat(beta)[1]))
hist(sim4, main = "N=30, X~U(0.2,0.8), err~N(0,1)", xlim = plot_range, xlab = expression(hat(beta)[1]))


# number of samples for each run 30
sim1 <- sim_betahat0(100, 30, 0, 1, 0, 1)
hist(sim1, main = "N=30, X~U(0,1), err~N(0,1)", xlab=expression(hat(beta)[0]))
mean(sim1)
var(sim1)

# number of samples for each run 300
sim2 <- sim_betahat0(100, 300, 0, 1, 0, 1)
hist(sim2, main = "N=300, X~U(0,1), err~N(0,1)", xlab=expression(hat(beta)[0]))
mean(sim2)
var(sim2)

# ideally when the error variace was small
sim3 <- sim_betahat0(100, 30, 0, 1, 0, 0.1)
hist(sim3, main = "N=30, X~U(0,1), err~N(0,0.1)", xlab=expression(hat(beta)[0]))
mean(sim3)
var(sim3)

# x_{i}'s are less spread out
sim4 <- sim_betahat0(100, 30, 0.2, 0.8, 0, 1)
hist(sim4, main = "N=30, X~U(0.2,0.8), err~N(0,1)", xlab=expression(hat(beta)[0]))
mean(sim4)
var(sim4)

# Plot four histograms at the same time
plot_range <- range(c(sim1, sim2, sim3, sim4)) # to take same range for three plots

par(mfrow=c(2,2))
hist(sim1, main = "N=30, X~U(0,1), err~N(0,1)", xlim = plot_range, xlab = expression(hat(beta)[0]))
hist(sim2, main = "N=300, X~U(0,1), err~N(0,1)", xlim = plot_range, xlab = expression(hat(beta)[0]))
hist(sim3, main = "N=30, X~U(0,1), err~N(0,0.1)", xlim = plot_range, xlab = expression(hat(beta)[0]))
hist(sim4, main = "N=30, X~U(0.2,0.8), err~N(0,1)", xlim = plot_range, xlab = expression(hat(beta)[0]))



