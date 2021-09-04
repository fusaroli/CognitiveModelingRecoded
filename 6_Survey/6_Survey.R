## Load libraries
pacman::p_load(here, 
               cmdstanr, 
               posterior)

# Define data
data <- list(
  k = c(16, 18, 22, 25, 27), # returned surveys per each helper
  m = 5, # n of helpeer
  nmax = 500 # man n of surveys
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


# The commands below are useful for a quick overview:
samples$summary()  # summary, same as print(samples)
samples$summary("theta", "mean", "sd") 
samples$summary("n", "mean", "sd") 

# Extract posterior samples 
draws_df <- as_draws_df(samples$draws()) 

## Diagnositics
ggplot(draws_df, aes(.iteration, theta, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, n, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()

## First calculate MLE:
cc <- -Inf
ind <- 0

for (i in 1:nrow(draws_df)) {
  logL <- 0
  for(j in 1:data$m) {   
    logL <- logL + lgamma(draws_df$n[i]+1) - lgamma(data$k[j]+1) - lgamma(draws_df$n[i]-data$k[j]+1)
    logL <- logL + data$k[j] * log(draws_df$theta[i]) + (draws_df$n[i] - data$k[j]) * log(1-draws_df$theta[i])
  }
  if (logL>cc) {
    ind <- i
    cc <- logL
  }
}
# end MLE


p <- ggplot(draws_df, aes(n, theta)) +
  geom_point(alpha=0.1) +
  geom_point(aes(x = mean(n), y = mean(theta)), color="red", size=10, shape=4) +
  geom_point(aes(x = draws_df$n[ind], y = draws_df$theta[ind]), color="lightblue", size=5) +
  xlab("Number of Surveys") +
  ylab("Rate of Return") +
  geom_rug(size=0.1) +
  theme_classic()

p1 <- ggExtra::ggMarginal(p, type="histogram", fill = "lightblue")

