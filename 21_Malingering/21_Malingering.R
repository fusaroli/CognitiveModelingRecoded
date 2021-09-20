pacman::p_load(
  cmdstanr,
  posterior,
  here,
  job,
  brms,
  tidyverse,
  ggbeeswarm
)

# Create data
k <- c(45, 45, 44, 45, 44, 45, 45, 45, 45, 45, 30,
       20, 6, 44, 44, 27, 25, 17, 14, 27, 35, 30)
p <- length(k) # number of people
n <- 45        # number of questions

data <- list(p=p, k=k, n=n) # To be passed on to Stan

file <- file.path(here("21_Malingering", "21_Malingering.stan"))
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


## BLABLA DIAGNOSTICS

# BRMS
df <- tibble(trials = n, correct = k, ID = as.factor(seq(p)))


## Basic model

malinger_f <- bf(
  correct | trials(trials) ~ 1 + (1|ID)
)

mix <- mixture(binomial, binomial)

get_prior(malinger_f, df, mix)

priormix <- c(
  prior(normal(0,1.5), class= Intercept, dpar=mu1),
  prior(normal(0,1.5), class= Intercept, dpar=mu2),
  prior(normal(0,1), class= sd, dpar=mu1),
  prior(normal(0,1), class= sd, dpar=mu2)
)

m <- brm(
  malinger_f,
  df,
  mix,
  prior=priormix,
  sample_prior = T,
  chains = 2,
  cores = 2,
  iter = 2000,
  refresh = 100,
  backend = "cmdstanr",
  threads = threading(2),
  control = list(
    max_treedepth = 20,
    adapt_delta = 0.9)
)

pp_check(m, ndraws=100)


# would it work having both mean and theta varying by individual? 

malinger_f1 <- bf(
  correct | trials(trials) ~ 1 + (1|p|ID),
  theta1 ~ 0 + ID
)

mix1 <- mixture(binomial, binomial, order="none")

get_prior(malinger_f1, df, mix1)

priormix1 <- c(
  prior(normal(0,1.5), class= Intercept, dpar=mu1),
  prior(normal(0,1.5), class= Intercept, dpar=mu2),
  prior(normal(0,1), class= sd, dpar=mu1),
  prior(normal(0,1), class= sd, dpar=mu2),
  prior(normal(0,1.5), class= b, dpar=theta1)
)

m1 <- brm(
  malinger_f1,
  df,
  mix1,
  prior=priormix1,
  sample_prior = T,
  chains = 2,
  cores = 2,
  iter = 4000,
  refresh = 200,
  backend = "cmdstanr",
  threads = threading(2),
  control = list(
    max_treedepth = 20,
    adapt_delta = 0.99)
)
m1
