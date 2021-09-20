pacman::p_load(
  cmdstanr,
  posterior,
  here,
  job,
  brms,
  tidyverse,
  ggbeeswarm
)


cheat.dat  <- read.table(here("22_Cheating", "cheat.csv"), header=F, sep=",")
cheatt.dat <- read.table(here("22_Cheating", "cheatt.csv"), header=F, sep="")
truth <- cheatt.dat$V1  # truth = 1 if cheater
k <- apply(cheat.dat, 1, sum)  # total correct per participant
p <- length(k)  # number of people
n <- 40         # total trials

data <- list(p=p, k=k, n=n, truth=truth) # To be passed on to Stan

file <- file.path(here("22_Cheating", "22_Cheating.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE), pedantic = TRUE)

## N.B. divergences relate to the priors
samples <- mod$sample(
  data = data,
  seed = 123,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 2000,
  iter_sampling = 2000,
  refresh = 500,
  #init = myinits,
  max_treedepth = 20,
  adapt_delta = 0.99,
)

samples

samples$summary()  # summary, same as print(samples)