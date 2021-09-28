pacman::p_load(
  tidyverse,
  here,
  cmdstanr,
  posterior,
  brms,
  bayesplot
)

t     <- c(1, 2, 4, 7, 12, 21, 35, 59, 99, 200)
nt    <- length(t)
slist <- 1:4
ns    <- length(slist)

k1 <- matrix(c(18, 18, 16, 13, 9, 6, 4, 4, 4, NA,
               17, 13,  9,  6, 4, 4, 4, 4, 4, NA,
               14, 10,  6,  4, 4, 4, 4, 4, 4, NA,
               NA, NA, NA, NA,NA,NA,NA,NA,NA, NA), nrow=ns, ncol=nt, byrow=T)

k <- k1[1:(ns - 1), 1:(nt - 1)]   # Excluding NAs (for Stan solution)

n <- 18

data <- list(k=k, n=n, t=t, ns=ns, nt=nt) # To be passed on to Stan

file <- file.path(here("31_MemoryRetention", "31_MemoryRetention_1.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE), pedantic = TRUE)

samples <- mod$sample(
  data = data,
  seed = 123,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 2000,
  iter_sampling = 2000,
  refresh = 500,
  init = myinits,
  max_treedepth = 20,
  adapt_delta = 0.99,
)

# Extract posterior samples and include sampling of the prior:
draws_df <- as_draws_df(samples$draws()) 

ggplot(draws_df)+
  geom_density(aes(alphaprior), fill="red", alpha=0.3) +
  geom_density(aes(alpha), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(betaprior), fill="red", alpha=0.3) +
  geom_density(aes(beta), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(alphaprior, betaprior), fill="red", alpha=0.3) +
  geom_density(aes(alpha, beta), fill="blue", alpha=0.3) +
  theme_classic()

## 

p <- ggplot(draws_df, aes(alpha, beta)) +
  geom_point(alpha=0.1) +
  xlab("alpha") +
  ylab("beta") +
  geom_rug(size=0.1) +
  theme_classic()

p1 <- ggExtra::ggMarginal(p, type="histogram", fill = "lightblue")

# Predictive checks as in the book (STILL TO FIX)
nsamples=nrow(draws_df)

priorCheck <- draws_df %>%
  group_by(priorpredk1, priorpredk2) %>%
  summarise(prop = n()/nsamples)

priorPredCheck <- ggplot(priorCheck,aes(priorpredk1,priorpredk2,size=prop)) +
  scale_size(range=c(0,5)) +
  geom_point(shape=0) + guides(size="none") +
  ylab("Success Rate 2") +
  xlab("Success Rate 1") +
  scale_y_continuous(minor_breaks=NULL,breaks=seq(0,10,1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  geom_point(aes(x=data$k1, y=data$k2), colour="red", shape=4) +
  theme_classic()

postCheck <- draws_df %>%
  group_by(postpredk1, postpredk2) %>%
  summarise(prop = n()/nsamples)

posteriorPredCheck <- ggplot(postCheck,aes(postpredk1,postpredk2,size=prop)) +
  scale_size(range=c(0,5)) +
  geom_point(shape=0) + guides(size="none") +
  ylab("Success Rate 2") +
  xlab("Success Rate 1") +
  scale_y_continuous(minor_breaks=NULL,breaks=seq(0,10,1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  geom_point(aes(x=data$k1, y=data$k2), colour="red", shape=4) +
  theme_classic()

priorPredCheck + posteriorPredCheck
