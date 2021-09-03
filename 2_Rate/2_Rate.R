
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

## Specify where the model is
file <- file.path(here("2_Rate", "2_Rate.stan"))
# Compile the model including threading
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))

# Fit the model.
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
samples$summary("delta", "mean", "sd") # more specific summary

# Extract posterior samples and include sampling of the priors:
# Note: in future models we will sample the prior directly within the model
draws_df <- as_draws_df(samples$draws()) %>%
  mutate(
    theta1_prior = rbeta(n = 4000, 1, 1),
    theta2_prior = rbeta(n = 4000, 1, 1),
    delta_prior = theta1_prior - theta2_prior
  )

# Now let's plot a density plot for delta comparing prior (red) and posterior (blue) estimates
ggplot(draws_df) +
  geom_density(aes(delta), color="blue", alpha=0.3) +
  geom_density(aes(delta_prior), color="red", alpha=0.3) +
  xlab("Difference Rate") +
  ylab("Posterior Density") +
  theme_classic()

## Diagnostics: traceplot
ggplot(draws_df, aes(.iteration, delta, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()

# Extract diagnostics
samples$cmdstan_diagnose() # summary

# More detailed diagnostics
diagnostics_df <- as_draws_df(samples$sampler_diagnostics())
print(diagnostics_df)

# mean of delta:
mean(draws_df$delta)
# median of delta:
median(draws_df$delta)
# mode of delta, estimated from the "density" smoother:
density(draws_df$delta)$x[which(density(draws_df$delta)$y==max(density(draws_df$delta)$y))]
# 95% credible interval for delta:
quantile(draws_df$delta, c(.025,.975))



## Generate code in brms for comparison 

d <- tibble(k=c(5,7), n=10, id=c("ID1","ID2"))
library(brms)
m <- brms::brm(
  k|trials(n)~1 + id,
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

