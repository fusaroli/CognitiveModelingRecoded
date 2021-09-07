x <- 10  # number of captures
k <- 4  # number of recaptures from n
n <- 5  # size of second sample
tmax <- 50  # maximum population size

data <- list(x=x, k=k, n=n, tmax=tmax) # to be passed on to Stan

file <- file.path(here("15_Planes", "15_Planes.stan"))
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
  fixed_param = T
)

samples$summary()  # summary, same as print(samples)

# Extract posterior samples 
draws_df <- as_draws_df(samples$draws()) 

# Now let's plot a histogram for mu and sigma. 
ggplot(draws_df) +
  #geom_density(aes(thetaprior), fill="red", alpha=0.3) +
  geom_density(aes(`t`), fill="blue", alpha=0.3) +
  geom_vline(xintercept=0.25) +
  xlab("theta") +
  ylab("Posterior Density") +
  theme_classic()
