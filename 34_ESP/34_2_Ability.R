
# Proportion correct on erotic pictures, block 1 and block 2:

prc1.ero <- c(0.6000000, 0.5333333, 0.6000000, 0.6000000, 0.4666667, 
              0.6666667, 0.6666667, 0.4000000, 0.6000000, 0.6000000,
              0.4666667, 0.6666667, 0.4666667, 0.6000000, 0.3333333,
              0.4000000, 0.4000000, 0.2666667, 0.3333333, 0.5333333,
              0.6666667, 0.5333333, 0.6000000, 0.4000000, 0.4666667, 
              0.7333333, 0.6666667, 0.6000000, 0.6666667, 0.5333333,
              0.5333333, 0.6666667, 0.4666667, 0.3333333, 0.4000000,
              0.5333333, 0.4000000, 0.4000000, 0.3333333, 0.4666667,
              0.4000000, 0.4666667, 0.4666667, 0.5333333, 0.3333333,
              0.7333333, 0.2666667, 0.6000000, 0.5333333, 0.4666667,
              0.4000000, 0.5333333, 0.6666667, 0.4666667, 0.5333333,
              0.5333333, 0.4666667, 0.4000000, 0.4666667, 0.6666667,
              0.4666667, 0.3333333, 0.3333333, 0.3333333, 0.4000000,
              0.4000000, 0.6000000, 0.4666667, 0.3333333, 0.3333333,
              0.6666667, 0.5333333, 0.3333333, 0.6000000, 0.4666667,
              0.4666667, 0.4000000, 0.3333333, 0.4666667, 0.5333333,
              0.8000000, 0.4000000, 0.5333333, 0.5333333, 0.6666667,
              0.6666667, 0.6666667, 0.6000000, 0.6000000, 0.5333333,
              0.3333333, 0.4666667, 0.6666667, 0.5333333, 0.3333333,
              0.3333333, 0.2666667, 0.2666667, 0.4666667, 0.6666667)

prc2.ero <- c(0.3333333, 0.6000000, 0.5333333, 0.2666667, 0.6666667,
              0.5333333, 0.6666667, 0.4666667, 0.4666667, 0.6666667,
              0.4000000, 0.6666667, 0.2666667, 0.4000000, 0.4666667,
              0.3333333, 0.5333333, 0.6000000, 0.3333333, 0.4000000,
              0.4666667, 0.4666667, 0.6000000, 0.5333333, 0.5333333,
              0.6000000, 0.5333333, 0.6666667, 0.6000000, 0.2666667,
              0.4666667, 0.4000000, 0.6000000, 0.5333333, 0.4000000,
              0.4666667, 0.5333333, 0.3333333, 0.4000000, 0.4666667,
              0.8000000, 0.6000000, 0.2000000, 0.6000000, 0.4000000,
              0.4000000, 0.2666667, 0.2666667, 0.6000000, 0.4000000,
              0.4000000, 0.4000000, 0.4000000, 0.4000000, 0.6666667,
              0.7333333, 0.5333333, 0.5333333, 0.3333333, 0.6000000,
              0.5333333, 0.5333333, 0.4666667, 0.5333333, 0.4666667,
              0.5333333, 0.4000000, 0.4000000, 0.4666667, 0.6000000,
              0.6000000, 0.6000000, 0.4666667, 0.6000000, 0.6666667,
              0.5333333, 0.4666667, 0.6000000, 0.2000000, 0.5333333,
              0.4666667, 0.4000000, 0.5333333, 0.5333333, 0.5333333,
              0.5333333, 0.6000000, 0.6666667, 0.4000000, 0.4000000,
              0.5333333, 0.8000000, 0.6000000, 0.4000000, 0.2000000,
              0.6000000, 0.6666667, 0.4666667, 0.4666667, 0.4666667)             

x <- matrix(cbind(prc1.ero, prc2.ero), nrow=100) 
n <- nrow(x) # number of participants

data <- list(x=x, n=n) # To be passed on to Stan

df <- tibble(
  correct = as.vector(x),
  block = rep(c(1,2), each = 100),
  ID = rep(c(1:100), 2)
)

df1 <- tibble(Session1 = as.vector(x)[1:100], Session2 = as.vector(x)[101:200],ID=1:100) 
df2 <- df1 %>%
  group_by(Session1, Session2) %>%
  summarize(size=n())

ggplot(df2, aes(Session1, Session2)) +
  geom_point(aes(size=size), shape=0) +
  theme_classic()

ggplot(df1, aes(Session1, Session2)) +
  geom_point(shape=0) +
  geom_smooth(method=lm) +
  theme_classic()

file <- file.path(here("34_ESP", "34_2_Ability.stan"))
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

