k <- c(21, 17, 21, 18, 22, 31, 31, 34, 34, 35, 35, 36, 39, 36, 35)
p <- length(k)  # number of people
n <- 40  # number of questions

data <- list(p=p, k=k, n=n) # to be passed on to Stan

## Specify where the model is
file <- file.path(here("17_Exams", "17_Exams.stan"))
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
  geom_density(aes(phiprior), fill="red", alpha=0.3) +
  geom_density(aes(`phi[1]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[2]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[3]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[4]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[5]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[6]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[7]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[8]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[9]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[10]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[11]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[12]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[13]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[14]`), fill="blue", alpha=0.3) +
  geom_density(aes(`phi[15]`), fill="blue", alpha=0.3) +
  geom_vline(xintercept=0.25) +
  xlab("Phi") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df) +
  geom_density(aes(muprior), fill="red", alpha=0.3) +
  geom_density(aes(`mu`), fill="blue", alpha=0.3) +
  geom_vline(xintercept=0.25) +
  xlab("Mu") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df) +
  geom_density(aes(sigmaprior), fill="red", alpha=0.3) +
  geom_density(aes(`sigma`), fill="blue", alpha=0.3) +
  geom_vline(xintercept=0.25) +
  xlab("Sigma") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df) +
  #geom_density(aes(sigmaprior), fill="red", alpha=0.3) +
  geom_density(aes(`predphi`), fill="blue", alpha=0.3) +
  geom_vline(xintercept=0.25) +
  xlab("PredPhi") +
  ylab("Posterior Density") +
  theme_classic()

# The data
df <- tibble(k = k, n = n, id=c(1:p))

mix <- mixture(binomial, binomial)
f0 <- bf(
  k|(trials(n)) ~ 1 + (1|id)
)

prior <- c(
  prior(normal(0, 0.001), class=Intercept, dpar = mu1),
  prior(normal(0, 1), class=Intercept, dpar = mu2)
)



# Fit it!

m <- brm(
  f0,
  df,
  family = mix,
  prior = prior,
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
