## BLABLA

pacman::p_load(
  tidyverse,
  here,
  cmdstanr,
  posterior,
  brms
)

## Define the data

x <- matrix(c(90, 95, 100, 105, 110, 115, 150, 155, 160), 
            nrow=3, ncol=3, byrow=T) 
x

n <- nrow(x) # number of people
m <- ncol(x) # number of repeated measurements

data <- list(x=x, n=n, m=m) # to be passed on to Stan

df <- tibble(IQ=as.vector(x), ID = c(1,1,1,2,2,2,3,3,3))

## Specify where the model is
file <- file.path(here("9_RepeatedIQ", "9_RepeatedIQ.stan"))
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
  geom_density(aes(muprior), color="red", alpha=0.3) +
  geom_density(aes(`mu[1]`), color="blue", alpha=0.3) +
  geom_density(aes(`mu[2]`), color="green", alpha=0.3) +
  geom_density(aes(`mu[3]`), color="yellow", alpha=0.3) +
  xlab("Mean") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df) +
  geom_density(aes(`sigmaprior`), color="red", alpha=0.3) +
  geom_density(aes(`sigma`), color="blue", alpha=0.3) +
  xlab("Sigma") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df, aes(`mu[1]`, `sigma`)) +
  geom_point(alpha=0.1) +
  xlab("Mean") +
  ylab("Standard Deviation") +
  geom_rug(size = 0.1) +
  theme_classic()

ggplot(draws_df, aes(`mu[2]`, `sigma`)) +
  geom_point(alpha=0.1) +
  xlab("Mean") +
  ylab("Standard Deviation") +
  geom_rug(size = 0.1) +
  theme_classic()
ggplot(draws_df, aes(`mu[3]`, `sigma`)) +
  geom_point(alpha=0.1) +
  xlab("Mean") +
  ylab("Standard Deviation") +
  geom_rug(size = 0.1) +
  theme_classic()


ggplot(draws_df) +
  geom_density(aes(`priorpredk`), fill="red", alpha=0.3) +
  geom_density(aes(`postpredk[1]`), fill="blue", alpha=0.3) +
  geom_density(aes(`postpredk[2]`), fill="green", alpha=0.3) +
  geom_density(aes(`postpredk[3]`), fill="yellow", alpha=0.3) +
  xlab("Predictive checks") +
  ylab("Posterior Density") +
  theme_classic()

## Now BRMS

m <- brm(
  bf(IQ ~ 0 + ID, sigma ~ 1),
  df,
  family = gaussian,
  prior = c(
    prior(normal(100, 15), class = b),
    prior(normal(0, 15), class = Intercept, dpar = sigma)),
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
