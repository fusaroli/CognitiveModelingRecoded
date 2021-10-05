k <- c(36, 32, 36, 36, 28, 40, 40, 24, 36, 36, 28, 40, 28, 
       36, 20, 24, 24, 16, 20, 32, 40, 32, 36, 24, 28, 44,
       40, 36, 40, 32, 32, 40, 28, 20, 24, 32, 24, 24, 20, 
       28, 24, 28, 28, 32, 20, 44, 16, 36, 32, 28, 24, 32,
       40, 28, 32, 32, 28, 24, 28, 40, 28, 20, 20, 20, 24,
       24, 36, 28, 20, 20, 40, 32, 20, 36, 28, 28, 24, 20,
       28, 32, 48, 24, 32, 32, 40, 40, 40, 36, 36, 32, 20,
       28, 40, 32, 20, 20, 16, 16, 28, 40)

x <- c(50, 80, 79, 56, 50, 80, 53, 84, 74, 67, 50, 45, 62, 
       65, 71, 71, 68, 63, 67, 58, 72, 73, 63, 54, 63, 70, 
       81, 71, 66, 74, 70, 84, 66, 73, 78, 64, 54, 74, 62, 
       71, 70, 79, 66, 64, 62, 63, 60, 56, 72, 72, 79, 67, 
       46, 67, 77, 55, 63, 44, 84, 65, 41, 62, 64, 51, 46,
       53, 26, 67, 73, 39, 62, 59, 75, 65, 60, 69, 63, 69, 
       55, 63, 86, 70, 67, 54, 80, 71, 71, 55, 57, 41, 56, 
       78, 58, 76, 54, 50, 61, 60, 32, 67)

nsubjs  <- length(k)
ntrials <- 60
sigmax  <- 3

# To be passed on to Stan
data <- list(k=k, x=x, sigmax=sigmax, nsubjs=nsubjs, ntrials=ntrials) 

df <- tibble(
  correct = k,
  trials = 60,
  extraversion = x,
  extra_me = 3,
  ID = 1:100
)


ggplot(df, aes(extraversion, correct/trials)) +
  geom_point() +
  geom_smooth(method=lm)+
  theme_classic()

file <- file.path(here("34_ESP", "34_3_Extraversion.stan"))
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
