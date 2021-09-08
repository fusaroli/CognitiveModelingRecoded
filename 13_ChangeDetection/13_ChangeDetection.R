pacman::p_load(
  cmdstanr,
  posterior,
  here,
  job
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
  threads_per_chain = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  refresh = 10,
  init = myinits,
  max_treedepth = 20,
  adapt_delta = 0.99,
)
})


# ADD DIAGNOSTICS

## FOR BRMS CHECK

# The data
df <- tibble(c=c, t=t)

# The model
m_f <- bf(
  c ~ Intercept * step(change - t) +  # Section 1
    (slope1 * change + slope2 * (age - change)) * step(age - change),  # Section 2
  Intercept + slope1 + slope2 ~ 1,  # Fixed intercept and slopes
  change ~ 1 + (1|person),  # Per-person changepoints around pop mean
  nl = TRUE
)

bform <- bf(
  c ~ b0 + b1 * (age - omega) * step(omega - age) + 
    b2 * (age - omega) * step(age - omega),
  b0 + b1 + b2 + alpha ~ 1 + (1|person),
  # to keep omega within the age range of 0 to 10
  nlf(omega ~ inv_logit(alpha) * 10),
  nl = TRUE
)

# Priors
bprior3 <- prior(normal(0, 5), nlpar = "Intercept") +
  prior(normal(0, 2), nlpar = "slope1") +
  prior(normal(0, 2), nlpar = "slope2") +
  prior(uniform(0, 10), nlpar = "change")  # Within observed range

# Initial values
inits3 = list(list(
  slope1 = slope1,
  slope2 = slope2,
  Intercept = intercept
))

# Fit it!
fit3 <- brm(bform3, data = df, prior = bprior3, chains = 1, inits = inits3)