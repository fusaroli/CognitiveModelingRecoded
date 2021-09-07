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
