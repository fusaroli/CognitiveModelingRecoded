nfails <- 949  
n <- 50  # Number of questionsin each test 
z_observed <- 30  # Score on the successful trial

data <- list(nfails = nfails, n = n, z_observed = z_observed) # to be passed on to Stan

file <- file.path(here("14_CensoredData", "14_CensoredData.stan"))
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
    ##init = myinits,
    max_treedepth = 20,
    adapt_delta = 0.99,
  )

samples$summary()  # summary, same as print(samples)

# Extract posterior samples 
draws_df <- as_draws_df(samples$draws()) 

# Now let's plot a histogram for mu and sigma. 
ggplot(draws_df) +
  geom_density(aes(thetaprior), fill="red", alpha=0.3) +
  geom_density(aes(`theta`), fill="blue", alpha=0.3) +
  geom_vline(xintercept=0.25) +
  xlab("theta") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df) +
  geom_density(aes(priorpred), fill="red", alpha=0.3) +
  geom_density(aes(`postpred`), fill="blue", alpha=0.3) +
  xlab("Predictive Checks") +
  ylab("Posterior Density") +
  theme_classic()

## BRMS
#m <- brm(
#  
#)
