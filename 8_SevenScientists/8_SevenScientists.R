## Load libraries
pacman::p_load(here, 
               cmdstanr, 
               posterior,
               brms)

# Define data
x <- c(-27.020, 3.570, 8.191, 9.898, 9.603, 9.945, 10.056)
n <- length(x)

data <- list(x = x, n = n)

df <- tibble(x = x, ID = as.factor(1:n)) # for plotting and brms

## Specify where the model is
## My model (w divergences, but reasonable priors)
file <- file.path(here("8_SevenScientists", "8_SevenScientists.stan"))
## Original model (no divergences unless you sample the priors)
# file <- file.path(here("8_SevenScientists", "8_SevenScientists_b.stan"))
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
  geom_density(aes(mu), color="blue", alpha=0.3) +
  geom_density(aes(muprior), color="red", alpha=0.3) +
  xlab("Mean") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df) +
  geom_density(aes(`sigmaprior`), color="red", alpha=0.3) +
  geom_density(aes(`sigma[1]`), color="blue", alpha=0.3) +
  geom_density(aes(`sigma[2]`), color="green", alpha=0.3) +
  geom_density(aes(`sigma[3]`), color="yellow", alpha=0.3) +
  geom_density(aes(`sigma[4]`), color="black", alpha=0.3) +
  geom_density(aes(`sigma[5]`), color="grey", alpha=0.3) +
  geom_density(aes(`sigma[6]`), color="black", alpha=0.3) +
  geom_density(aes(`sigma[7]`), color="grey", alpha=0.3) +
  xlab("Sigma") +
  ylab("Posterior Density") +
  theme_classic()

## Diagnostics
samples$cmdstan_diagnose() # summary
diagnostics_df <- as_draws_df(samples$sampler_diagnostics())
print(diagnostics_df)

ggplot(draws_df, aes(.iteration, mu, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, `sigma[1]`, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, `sigma[2]`, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, `sigma[3]`, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()

## Plotting a joint distribution of mu and sigma

ggplot(draws_df, aes(mu, `sigma[1]`)) +
  geom_point(alpha=0.1) +
  geom_point(data=draws_df[diagnostics_df$divergent__==1,], aes(mu, `sigma[1]`), color="red") +
  xlab("Mean") +
  ylab("Standard Deviation") +
  geom_rug(size = 0.1) +
  theme_classic()
ggplot(draws_df, aes(mu, `sigma[2]`)) +
  geom_point(alpha=0.1) +
  geom_point(data=draws_df[diagnostics_df$divergent__==1,], aes(mu, `sigma[2]`), color="red") +
  xlab("Mean") +
  ylab("Standard Deviation") +
  geom_rug(size = 0.1) +
  theme_classic()
ggplot(draws_df, aes(mu, `sigma[3]`)) +
  geom_point(alpha=0.1) +
  geom_point(data=draws_df[diagnostics_df$divergent__==1,], aes(mu, `sigma[3]`), color="red") +
  xlab("Mean") +
  ylab("Standard Deviation") +
  geom_rug(size = 0.1) +
  theme_classic()

stanfit <- rstan::read_stan_csv(samples$output_files())
posterior_cp <- as.array(stanfit)

mcmc_trace(posterior_cp)
mcmc_rank_overlay(posterior_cp)

np_cp <- nuts_params(stanfit)
color_scheme_set("darkgray")
mcmc_parcoord(posterior_cp, np = np_cp)

mcmc_pairs(posterior_cp, np = np_cp, pars = c("mu","sigma[1]","sigma[2]"),
           off_diag_args = list(size = 0.75))

# assign to an object so we can reuse later
scatter_theta_cp <- mcmc_scatter(
  posterior_cp,
  pars = c("mu","sigma[1]"),
  np = np_cp,
  size = 1
)
scatter_theta_cp

m <- brm(bf(x ~ 1, 
            sigma ~ 0 + ID),
        df,
        family=gaussian,
        prior=c(
          prior(normal(0, 10), class = Intercept),
          prior(normal(0, 10), class = b, dpar=sigma)),
        sample_prior=T,
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
