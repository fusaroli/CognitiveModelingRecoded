# Sample size N and effect size E in the Bem experiments
N <- c(100, 150, 97, 99, 100, 150, 200, 100, 50)     # sample sizes
E <- c(.25, .20, .25, .20, .22, .15, .09, .19, .42)  # effect sizes

x <- matrix(cbind(N,E), nrow=9) 
n <- nrow(x)  # number of experiments

data <- list(x=x, n=n)  # to be passed on to Stan

df <- tibble(
  SampleSize = N,
  EffectSize = E
)

ggplot(df, aes(SampleSize, EffectSize)) +
  geom_point() +
  geom_smooth(method=lm) +
  theme_classic()

ggplot(df, aes(scale(SampleSize), scale(EffectSize))) +
  geom_point() +
  geom_smooth(method=lm) +
  theme_classic()

file <- file.path(here("34_ESP", "34_1_OptionalStopping.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE), pedantic = TRUE)

samples <- mod$sample(
  data = data,
  seed = 123,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 2000,
  iter_sampling = 2000,
  #init=0.5,
  refresh = 500,
  max_treedepth = 20,
  adapt_delta = 0.99,
)

draws_df <- as_draws_df(samples$draws()) 

ggplot(draws_df)+
  geom_density(aes(muprior), fill="red", alpha=0.3) +
  geom_density(aes(`mu[1]`), fill="blue", alpha=0.3) +
  geom_density(aes(`mu[2]`), fill="green", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(`sigmaprior`), fill="red", alpha=0.3) +
  geom_density(aes(`sigma[1]`), fill="blue", alpha=0.3) +
  geom_density(aes(`sigma[2]`), fill="green", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(`rprior`), fill="red", alpha=0.3) +
  geom_density(aes(`r`), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  #geom_point(aes(mubetaprior, sigmabetaprior), color="red", alpha=0.05) +
  geom_point(aes(`mu[1]`, `mu[2]`), color="green", alpha=0.1) +
  theme_classic()

ggplot(draws_df)+
  #geom_point(aes(mubetaprior, sigmabetaprior), color="red", alpha=0.05) +
  geom_point(aes(`mu[1]`, `sigma[1]`), color="green", alpha=0.1) +
  theme_classic()

ggplot(draws_df)+
  #geom_point(aes(mubetaprior, sigmabetaprior), color="red", alpha=0.05) +
  geom_point(aes(`mu[2]`, `sigma[2]`), color="green", alpha=0.1) +
  theme_classic()

ggplot(draws_df)+
  #geom_point(aes(mubetaprior, sigmabetaprior), color="red", alpha=0.05) +
  geom_point(aes(`mu[1]`, `r`), color="green", alpha=0.1) +
  theme_classic()
