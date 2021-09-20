pacman::p_load(
  cmdstanr,
  posterior,
  here,
  brms,
  tidybayes,
  ggridges
)

dset <- 2  # Choose dataset/model

if (dset == 1) {
  k <- c(1,1,1,1,0,0,1,1,0,1,0,0,1,0,0,1,0,1,0,0,
         0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,
         0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,
         1,0,1,1,0,1,1,1,0,1,0,0,1,0,0,0,0,1,0,0,
         1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,0,1,0,0,
         0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,1,1,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,1,
         1,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0)
}
if (dset == 2) {
  k <- c(1,1,1,1,0,0,1,1,0,1,0,0,NA,0,0,1,0,1,0,0,
         0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,
         0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,
         1,0,1,1,0,1,1,1,0,1,0,0,1,0,0,0,0,1,0,0,
         1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,0,1,0,0,
         0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
         0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,1,1,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,1,
         1,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,NA,0,0)
}

k <- matrix(k, nrow=10, byrow=T)
np <- nrow(k)
nq <- ncol(k)

if (dset == 1) {
  file <- file.path(here("18_TwentyQuestions", "18_TwentyQuestions.stan"))
  
  data <- list(k=k, np=np, nq=nq)  # to be passed on to Stan
  
} else if (dset == 2) {
  file <- file.path(here("18_TwentyQuestions", "18_TwentyQuestionsNA.stan"))
  k_na <- is.na(k) + 0  # Matrix locating missing values: 1 = missing
  n_na <- sum(k_na)  # number of missing values
  k[is.na(k)] <- 99  # some numeric value, since Stan doesn't eat NAs
  
  # to be passed on to Stan:
  data <- list(k=k, np=np, nq=nq, k_na=k_na, n_na=n_na)  
}

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

draws_p <- draws_df %>%
  spread_draws(p[id]) %>%
  mutate(id = as.factor(id))

draws_q <- draws_df %>%
  spread_draws(q[question]) %>%
  mutate(question = as.factor(question))

#draws_theta <- draws_df %>%
#  spread_draws(theta[id, question])  %>%
#  mutate(id = as.factor(id), question = as.factor(question))


ggplot(draws_p, aes(x = p, y = id)) + geom_density_ridges() +
  theme_classic()
ggplot(draws_q, aes(x = q, y = question)) + geom_density_ridges() +
  theme_classic()

## brms

if (dset == 1) {
  k <- c(1,1,1,1,0,0,1,1,0,1,0,0,1,0,0,1,0,1,0,0,
         0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,
         0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,
         1,0,1,1,0,1,1,1,0,1,0,0,1,0,0,0,0,1,0,0,
         1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,0,1,0,0,
         0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,1,1,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,1,
         1,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0)
}
if (dset == 2) {
  k <- c(1,1,1,1,0,0,1,1,0,1,0,0,NA,0,0,1,0,1,0,0,
         0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,
         0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,
         1,0,1,1,0,1,1,1,0,1,0,0,1,0,0,0,0,1,0,0,
         1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,0,1,0,0,
         0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
         0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         0,1,1,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,1,
         1,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,NA,0,0)
}

df <- tibble(
  correct = k,
  id = rep(c(1:10), each = 20),
  question = rep(c(1:20), times = 10)
)

## N.B. current implementation has population level hyper priors and logodds priors 
## N.B: current implementation does not support | mi(). Also the stan code above is not 
## inputing while fitting, just generating 0 and 1 after fitting.
m <- brm(
  correct  ~ 1 + (1|id) + (1|question),
  df,
  family = bernoulli(),
  prior = c(
    prior(normal(0,1), class=Intercept),
    prior(normal(0,.1), class=sd)
  ),
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
