# CHOOSE a data set:
# Influenza 
y <- c(14, 4, 5, 210)
# Hearing Loss 
# y <- c(20, 7, 103, 417)
# Rare Disease
# y <- c(0, 0, 13, 157)

n = length(y)

data <- list(y = y, n = n) 

df <- tibble(y=y)

## Specify where the model is
file <- file.path(here("12_kappa", "12_Kappa.stan"))
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
  geom_density(aes(kappaprior), fill="red", alpha=0.3) +
  geom_density(aes(`kappa`), fill="blue", alpha=0.3) +
  xlab("Kappa") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df) +
  geom_density(aes(alphaprior), fill="red", alpha=0.3) +
  geom_density(aes(`alpha`), fill="blue", alpha=0.3) +
  xlab("Alpha") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df) +
  geom_density(aes(betaprior), fill="red", alpha=0.3) +
  geom_density(aes(`beta`), fill="blue", alpha=0.3) +
  xlab("Beta") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df) +
  geom_density(aes(gammaprior), fill="red", alpha=0.3) +
  geom_density(aes(`gamma`), fill="blue", alpha=0.3) +
  xlab("Gamma") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df, aes(`alpha`, `beta`)) +
  geom_point(alpha=0.1) +
  xlab("Alpha") +
  ylab("Beta") +
  geom_rug(size = 0.1) +
  theme_classic()

ggplot(draws_df, aes(`alpha`, `beta`)) +
  geom_point(alpha=0.1) +
  xlab("Gamma") +
  ylab("Beta") +
  geom_rug(size = 0.1) +
  theme_classic()

ggplot(draws_df, aes(`alpha`, `beta`)) +
  geom_point(alpha=0.1) +
  xlab("Alpha") +
  ylab("Gamma") +
  geom_rug(size = 0.1) +
  theme_classic()
