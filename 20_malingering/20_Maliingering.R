
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
p <- length(k)  # number of people
n <- 45         # number of questions

data <- list(p=p, k=k, n=n)  # To be passed on to Stan

file <- file.path(here("20_Malingering", "20_Malingering.stan"))
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

# Extract posterior samples 
draws_df <- as_draws_df(samples$draws()) 

# Now let's plot a histogram for mu and sigma. 
ggplot(draws_df) +
  geom_density(aes(`psiprior[1]`), fill="pink", alpha=0.3) +
  geom_density(aes(`psiprior[2]`), fill="lightblue", alpha=0.3) +
  geom_density(aes(`psi[1]`), fill="red", alpha=0.3) +
  geom_density(aes(`psi[2]`), fill="blue", alpha=0.3) +
  xlab("Psy") +
  ylab("Posterior Density") +
  theme_classic()

ggplot(draws_df) +
  geom_point(aes(`psiprior[1]`,`psiprior[2]`), fill="pink", color="pink", alpha=0.3) +
  geom_point(aes(`psi[1]`,`psi[2]`), fill="blue", color="blue", alpha=0.3) +
  theme_classic()


ggplot(draws_df) +
  geom_point(aes(`psi[1]`,`psi[2]`), fill="blue", color="blue", alpha=0.3) +
  theme_classic()

sumD <- draws_df %>% select(starts_with("z")) %>%
  pivot_longer(everything(), names_to = "ID", values_to = "Malingering") %>%
  mutate(ID=parse_number(as.character(ID))) %>%
  group_by(ID) %>% summarize(Malinger=as.factor(mean(Malingering, na.rm=T))) %>%
  mutate(score=k)

ggplot(sumD, aes(Malinger, score, color=Malinger)) + geom_beeswarm() + theme_classic()


df <- tibble(trials = n, correct = k, ID = as.factor(seq(p)))


## Basic model

malinger_f <- bf(
  correct | trials(trials) ~ 1
)

mix <- mixture(binomial, binomial)

get_prior(malinger_f, df, mix)

priormix <- c(
  prior(normal(0,1.5), class= Intercept, dpar=mu1),
  prior(normal(0,1.5), class= Intercept, dpar=mu2)
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

## In order to extract chances of malingering by ID
malinger_f1 <- bf(
  correct | trials(trials) ~ 1,
  theta1 ~ 0 + ID
)

mix1 <- mixture(binomial, binomial, order="none")

get_prior(malinger_f1, df, mix1)

priormix1 <- c(
  prior(normal(0,1.5), class= b, dpar=theta1),
  prior(normal(0,1.5), class= Intercept, dpar=mu1),
  prior(normal(0,1.5), class= Intercept, dpar=mu2)
)

m1 <- brm(
  malinger_f1,
  df,
  mix1,
  prior=priormix1,
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
