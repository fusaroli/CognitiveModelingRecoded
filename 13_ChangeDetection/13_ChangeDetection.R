pacman::p_load(
  cmdstanr,
  posterior,
  here,
  job,
  brms
)

c <- scan(here("13_ChangeDetection", "13_data.txt"))
n <- length(c)
t <- 1:n

data <- list(c=c, n=n, t=t) # to be passed on to Stan

df <- tibble(c=c, t=t)

myinits <- list(
  list(mu=c(1, 1), sigma=1, tau=n / 2))

## Specify where the model is
file <- file.path(here("13_ChangeDetection", "13_ChangeDetection.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))
job::job({
samples <- mod$sample(
  data = data,
  seed = 123,
  chains = 1,
  parallel_chains = 1,
  threads_per_chain = 1,
  iter_warmup = 200,
  iter_sampling = 300,
  refresh = 10,
  init = myinits,
  max_treedepth = 20,
  adapt_delta = 0.8,
)
})


# ADD DIAGNOSTICS

## FOR BRMS CHECK

# The data
df <- tibble(c=c, t=t)

bform <- bf(
  c ~ Intercept1 * step(change - t) +  # Section 1
    Intercept2 * step(t - change),  # Section 2,
  Intercept1 + Intercept2 ~ 1,  # Fixed intercept and slopes
  change ~ 1,  # Per-person changepoints around pop mean
  nl = TRUE
)

# Priors
bprior <- prior(normal(35, 10), nlpar = "Intercept1") +
  prior(normal(35, 10), nlpar = "Intercept2") +
  prior(uniform(0, 1178), nlpar = "change")  # Within observed range



# Fit it!

m <- brm(
  bform,
  df,
  family = gaussian,
  prior = bprior,
  sample_prior = T,
  seed = 123,
  chains = 1,
  cores = 1,
  iter = 200,
  refresh=10,
  backend = "cmdstanr",
  threads = threading(2),
  control = list(
    max_treedepth = 20,
    adapt_delta = 0.8)
)

stancode(m)