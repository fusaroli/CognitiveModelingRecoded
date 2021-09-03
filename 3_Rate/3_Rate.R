
## Load the relevant libraries

pacman::p_load(tidyverse,
               here,
               cmdstanr,
               bayesplot,
               posterior,
               rstan,
               bayesplot)

## Create the data
data <- list(
  n1 = 10,
  n2 = 10,
  k1 = 5,
  k2 = 7
)

## Define initial values
myinits <- list(
  list(theta=.1),  # chain 1 starting value
  list(theta=.9))  # chain 2 starting value

## Specify where the model is and compile it enabling threading
file <- file.path(here("3_Rate", "3_Rate.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))

# Fit the models

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
draws_df <- as_draws_df(samples$draws()) %>%
  mutate(
    theta_prior = rbeta(n = 4000, 1, 1)
  )

# Now let's plot a denity plot for theta. 
ggplot(draws_df) +
  geom_density(aes(theta), color="blue", alpha=0.3) +
  geom_density(aes(theta_prior), color="red", alpha=0.3) +
  xlab("Rate") +
  ylab("Posterior Density") +
  theme_classic()

## Diagnostics: traceplot
ggplot(draws_df, aes(.iteration, theta, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()

# Extract diagnostics
samples$cmdstan_diagnose() # summary
diagnostics_df <- as_draws_df(samples$sampler_diagnostics())
print(diagnostics_df)


## Generate the code for the same model in brms for comparison
d <- tibble(k=c(5,7), n=10)
library(brms)
m <- brms::brm(
  k|trials(n)~ 1,
  d,
  family = binomial(link = "identity"),
  prior = prior(beta(1,1), class = Intercept),
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

