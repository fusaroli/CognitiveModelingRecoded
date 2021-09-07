pacman::p_load(
  tidyverse,
  here,
  cmdstanr,
  posterior,
  brms,
  bayesplot
)

# Choose a dataset:
dataset <- 1

# The datasets:
if (dataset == 1) { 
  x <- matrix(c( .8, 102, 
                 1.0,  98, 
                 .5, 100,
                 .9, 105, 
                 .7, 103, 
                 .4, 110,
                 1.2,  99, 
                 1.4,  87,
                 .6, 113,
                 1.1,  89,
                 1.3,  93), nrow=11, ncol=2, byrow=T) 
}
if (dataset == 2) {
  x <- matrix(c( .8, 102, 
                 1.0,  98,
                 .5, 100,
                 .9, 105,
                 .7, 103, 
                 .4, 110,
                 1.2,  99,
                 1.4,  87,
                 .6, 113, 
                 1.1,  89,
                 1.3,  93,
                 .8, 102, 
                 1.0,  98, 
                 .5, 100, 
                 .9, 105, 
                 .7, 103, 
                 .4, 110, 
                 1.2,  99, 
                 1.4,  87, 
                 .6, 113, 
                 1.1,  89, 
                 1.3,  93), nrow=22,ncol=2,byrow=T) 
}

n <- nrow(x) # number of people/units measured

data <- list(x=x, n=n) # to be passed on to Stan

df <- tibble(x1=x[,1], x2 = x[,2])

## Specify where the model is
file <- file.path(here("10_PearsonCorrelation", "10_PearsonCorrelation.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))

samples <- mod$sample(
  data = data,
  seed = 123,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 2000,
  iter_sampling = 2000,
  refresh = 500,
  max_treedepth = 20,
  adapt_delta = 0.99,
)

samples$summary()  # summary, same as print(samples)

# Extract posterior samples 
draws_df <- as_draws_df(samples$draws()) 

# Now let's plot a histogram for mu and sigma. 
ggplot(draws_df) +
  geom_density(aes(rprior), fill="red", alpha=0.3) +
  geom_density(aes(`r`), fill="blue", alpha=0.3) +
  xlab("Pearson coefficient") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df) +
  geom_density(aes(`muprior`), fill="red", alpha=0.3) +
  geom_density(aes(`mu[1]`), fill="blue", alpha=0.3) +
  geom_density(aes(`mu[2]`), fill="green", alpha=0.3) +
  xlab("Mean") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df) +
  geom_density(aes(`sigmaprior`), fill="red", alpha=0.3) +
  geom_density(aes(`sigma[1]`), fill="blue", alpha=0.3) +
  geom_density(aes(`sigma[2]`), fill="green", alpha=0.3) +
  xlab("Sigma") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df, aes(`mu[1]`, `sigma[1]`)) +
  geom_point(alpha=0.1) +
  xlab("Mean") +
  ylab("Standard Deviation") +
  geom_rug(size = 0.1) +
  theme_classic()

ggplot(draws_df, aes(`mu[2]`, `sigma[2]`)) +
  geom_point(alpha=0.1) +
  xlab("Mean") +
  ylab("Standard Deviation") +
  geom_rug(size = 0.1) +
  theme_classic()

## Diagnostics
samples$cmdstan_diagnose() 
stanfit <- rstan::read_stan_csv(samples$output_files())
posterior_cp <- as.array(stanfit)

mcmc_trace(posterior_cp)
mcmc_rank_overlay(posterior_cp)


## Now in brms
m <- brm(
  bf(mvbind(x1,x2) ~ 1) + set_rescor(rescor=TRUE),
  df,
  family = gaussian,
  prior = c(
    prior(normal(0, 10), class = Intercept, resp = x1),
    prior(normal(0, 10), class = sigma, resp = x1),
    prior(normal(0, 10), class = Intercept, resp = x2),
    prior(normal(0, 10), class = sigma, resp = x2),
    prior(lkj(1), class = rescor)),
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
  