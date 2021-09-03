
pacman::p_load(here, rstan, cmdstanr, posterior)

data <- list(
  k = c(16, 18, 22, 25, 27),
  m = 5,
  nmax = 500
)


## Define initial values
myinits <- list(
  list(theta=.1, thetaprior=.1),  # chain 1 starting value
  list(theta=.9, thetaprior=.9))  # chain 2 starting value

## Specify where the model is
file <- file.path(here("6_Survey", "6_Survey.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))

samples <- mod$sample(
  data = data,
  seed = 123,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 1000,
  iter_sampling = 2000,
  refresh = 500,
  max_treedepth = 20,
  adapt_delta = 0.99,
)


# Now the values for the monitored parameters are in the "samples" object, 
# ready for inspection.

# The commands below are useful for a quick overview:
samples$summary()  # summary, same as print(samples)
samples$summary("theta", "mean", "sd") # more specific summary
samples$summary("n", "mean", "sd") # more specific summary

# Extract posterior samples and include sampling of the prior:
draws_df <- as_draws_df(samples$draws()) 

## First calculate MLE:
cc <- -Inf
ind <- 0

for (i in 1:nrow(draws_df)) {
  logL <- 0
  for(j in 1:data$m) {   
    logL <- logL+lgamma(draws_df$n[i]+1)-lgamma(data$k[j]+1)-lgamma(draws_df$n[i]-data$k[j]+1)
    logL <- logL+data$k[j]*log(draws_df$theta[i])+(draws_df$n[i]-data$k[j])*log(1-draws_df$theta[i])
  }
  if (logL>cc) {
    ind <- i
    cc <- logL
  }
}
# end MLE

######################Plots#####################################################
layout(matrix(c(2,0,1,3),2,2,byrow=T), width=c(2/3, 1/3), heights=c(1/3, 2/3))
xhist <- hist(draws_df$n, plot=F)
yhist <- hist(draws_df$theta, plot=F)
top <- max(c(xhist$counts, yhist$counts))
xrange <- c(0, data$nmax)
yrange <- c(0, 1)

par(mar=c(5, 5, 1, 1))
plot(draws_df$n, draws_df$theta, xlim=xrange, ylim=yrange,ylab="", xlab="")
axis(1)
mtext("Number of Surveys", side=1,line=2.25, cex=1.2)
axis(2, cex=1.2)
las=0
mtext("Rate of Return", side=2 ,line=2.25, cex=1.2)
las=1
points(mean(draws_df$n), mean(draws_df$theta), col="red", lwd=3, pch=4) #expectation
points(draws_df$n[ind], draws_df$theta[ind], col="green", lwd=3, pch=10) #Maximum Likelihood

par(mar=c(0, 4, 1, 1))
barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0,col="lightblue")

par(mar=c(4, 0, 1, 3))
barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE,
        col="lightblue")
