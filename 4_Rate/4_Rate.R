
## Load the relevant libraries

pacman::p_load(tidyverse,
               here,
               cmdstanr,
               bayesplot,
               posterior,
               rstan)

## Create the data
data <- list(
  n = 10,
  k = 5
)

## Define initial values
myinits <- list(
  list(theta=.1, thetaprior=.1),  # chain 1 starting value
  list(theta=.9, thetaprior=.9))  # chain 2 starting value

## Specify where the model is
file <- file.path(here("4_Rate", "4_Rate.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))

# The following command calls Stan with specific options.
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
samples$summary("theta", "mean", "sd") # more specific summary

# Extract posterior samples and include sampling of the prior:
draws_df <- as_draws_df(samples$draws())

# Now let's plot the density for theta (prior and posterior)
ggplot(draws_df) +
  geom_density(aes(theta), fill="blue", alpha=0.3) +
  geom_density(aes(thetaprior), fill="red", alpha=0.3) +
  xlab("Rate") +
  ylab("Posterior Density") +
  theme_classic()

# Now let's plot predictive checks for theta (prior and posterior)
ggplot(draws_df) +
  geom_histogram(aes(priorpredk), fill="red", linetype="dashed", alpha=0.1, binwidth=0.5) +
  geom_histogram(aes(postpredk), fill="blue", alpha=0.3, binwidth=0.5) +
  geom_point(aes(x=data$k, y=10), shape=4, size=5, color="black") +
  scale_x_continuous(breaks=seq(0,10,1)) +
  xlab("Success Count") +
  ylab("Posterior Density") +
  theme_classic()

## Diagnostics
ggplot(draws_df, aes(.iteration, theta, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()

# Extract diagnostics
samples$cmdstan_diagnose() # summary
diagnostics_df <- as_draws_df(samples$sampler_diagnostics())
print(diagnostics_df)


## GENERATE THE CODE IN BRMS TO COMPARE
d <- tibble(k=5, n=10)
library(brms)
m <- brms::brm(
  k|trials(n)~1,
  d,
  family = binomial(link = "identity"),
  prior = prior(beta(1,1), class=Intercept),
  sample_prior = T,
  seed = 123,
  chains = 2,
  cores = 2,
  iter = 2000,
  backend = "cmdstanr",
  threads = threading(2),
  control = list(
    max_treedepth = 20,
    adapt_delta = 0.99)
)
stancode(m)

