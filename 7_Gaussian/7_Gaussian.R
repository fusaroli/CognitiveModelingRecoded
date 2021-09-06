## Load libraries
pacman::p_load(here, 
               cmdstanr, 
               posterior,
               brms)

# Define data
x <- c(1.1, 1.9, 2.3, 1.8)
n <- length(x)
data <- list(x = x, n = n)
df <- tibble(x = x, n = n) # for plotting

## Define initial values
myinits <- list(
  list(theta=.1, thetaprior=.1),  # chain 1 starting value
  list(theta=.9, thetaprior=.9))  # chain 2 starting value

## Specify where the model is
file <- file.path(here("7_Gaussian", "7_Gaussian.stan"))
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
samples$summary("mu", "mean", "sd") 
samples$summary("sigma", "mean", "sd") 

# Extract posterior samples 
draws_df <- as_draws_df(samples$draws()) 

# Now let's plot a histogram for theta. 
ggplot(draws_df) +
  geom_density(aes(mu), color="blue", alpha=0.3) +
  geom_density(aes(muprior), color="red", alpha=0.3) +
  xlab("Mean") +
  ylab("Posterior Density") +
  theme_classic()
ggplot(draws_df) +
  geom_density(aes(sigma), color="blue", alpha=0.3) +
  geom_density(aes(sigmaprior), color="red", alpha=0.3) +
  xlab("Mean") +
  ylab("Posterior Density") +
  theme_classic()

# Now let's plot predictive checks for theta (prior and posterior)
ggplot(draws_df) +
  geom_density(aes(priorpredk), fill="red", linetype="dashed", alpha=0.1) +
  geom_density(aes(postpredk), fill="blue", alpha=0.3) +
  geom_density(data = df, aes(x), fill="black") +
  xlab("Mean") +
  ylab("Posterior Density") +
  theme_classic()


## Diagnostics
ggplot(draws_df, aes(.iteration, mu, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, sigma, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()

## Plotting a joint distribution of mu and sigma

p <- ggplot(draws_df, aes(mu, sigma)) +
  geom_point(alpha=0.1) +
  geom_point(aes(x = mean(mu), y = mean(sigma)), color="red", size=5, shape=4) +
  xlab("Mean") +
  ylab("Standard Deviation") +
  geom_rug(size = 0.1) +
  theme_classic()

m<- brm(x ~ 1,
        df,
        family=gaussian,
        prior=c(
          prior(normal(0, sqrt(1000)),class=Intercept),
          prior(uniform(0,10), class=sigma)),
        sample_prior=T,seed = 123,
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
