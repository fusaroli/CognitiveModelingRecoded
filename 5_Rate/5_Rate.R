
## Load the relevant libraries
pacman::p_load(tidyverse,
               here,
               cmdstanr,
               bayesplot,
               posterior,
               rstan,
               bayesplot)

## Create the data
data <- list(
  n1 = 10,
  n2 = 10,
  k1 = 5,
  k2 = 7
)

## Define initial values
myinits <- list(
  list(theta=.1, thetaprior=.1),  # chain 1 starting value
  list(theta=.9, thetaprior=.9))  # chain 2 starting value


## Specify where the model is
file <- file.path(here("5_Rate", "5_Rate.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))

# The following command calls Stan with specific options.
samples <- mod$sample(
  data = data,
  seed = 123,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 1000,
  iter_sampling = 2000,
  refresh = 500,
  max_treedepth = 20,
  adapt_delta = 0.99,
)

# The commands below are useful for a quick overview:
samples$summary()  # summary, same as print(samples)
samples$summary("theta", "mean", "sd") # more specific summary

# Extract posterior samples and include sampling of the prior:
draws_df <- as_draws_df(samples$draws()) 

# Now let's plot a histogram for theta. 
ggplot(draws_df) +
  geom_density(aes(theta), color="blue", alpha=0.3) +
  geom_density(aes(thetaprior), color="red", alpha=0.3) +
  xlab("Rate") +
  ylab("Posterior Density") +
  theme_classic()

# Now let's plot predictive checks for theta (prior and posterior)
ggplot(draws_df) +
  geom_histogram(aes(priorpredk1), fill="red", linetype="dashed", alpha=0.1, binwidth=0.5) +
  geom_histogram(aes(postpredk1), fill="blue", alpha=0.3, binwidth=0.5) +
  geom_point(aes(x=data$k1, y=10), shape=4, size=5, color="black") +
  geom_point(aes(x=data$k2, y=10), shape=4, size=5, color="black") +
  scale_x_continuous(breaks=seq(0,10,1)) +
  xlab("Success Count") +
  ylab("Posterior Density") +
  theme_classic()

## Diagnostics: traceplot
ggplot(draws_df, aes(.iteration, theta, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()


# Extract diagnostics
samples$cmdstan_diagnose() # summary
diagnostics_df <- as_draws_df(samples$sampler_diagnostics())
print(diagnostics_df)


# Alternative predictive checks as in the book
nsamples=nrow(draws_df)

priorCheck <- as_tibble(expand.grid(priorpredk1 = seq(0, data$n1, 1), priorpredk2 = seq(0, data$n2, 1), prop=0))

for (i in 0:data$n1){
  for (j in 0:data$n2){
    match.preds <- sum(draws_df$priorpredk1==i & draws_df$priorpredk2==j)/nsamples
    if (match.preds > 0){
      priorCheck$prop[priorCheck$priorpredk1==i & priorCheck$priorpredk2==j] = match.preds
    }
  }
}

priorPredCheck <- ggplot(priorCheck,aes(priorpredk1,priorpredk2,size=prop)) +
  scale_size(range=c(0,5)) +
  geom_point(shape=0) + guides(size="none") +
  ylab("Success Rate 2") +
  xlab("Success Rate 1") +
  scale_y_continuous(minor_breaks=NULL,breaks=seq(0,10,1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  geom_point(aes(x=data$k1, y=data$k2), colour="red", shape=4) +
  theme_classic()

postCheck <- as_tibble(expand.grid(postpredk1 = seq(0, data$n1, 1), postpredk2 = seq(0, data$n2, 1), prop=0))

for (i in 0:data$n1){
  for (j in 0:data$n2){
    match.preds <- sum(draws_df$postpredk1==i & draws_df$postpredk2==j)/nsamples
    if (match.preds > 0){
      postCheck$prop[postCheck$postpredk1==i & postCheck$postpredk2==j] = match.preds
    }
  }
}

posteriorPredCheck <- ggplot(postCheck,aes(postpredk1,postpredk2,size=prop)) +
    scale_size(range=c(0,5)) +
    geom_point(shape=0) + guides(size="none") +
    ylab("Success Rate 2") +
    xlab("Success Rate 1") +
    scale_y_continuous(minor_breaks=NULL,breaks=seq(0,10,1)) +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    geom_point(aes(x=data$k1, y=data$k2), colour="red", shape=4) +
    theme_classic()

library(patchwork)
priorPredCheck + posteriorPredCheck

## GENERATE THE CODE IN BRMS AND COMPARE
d <- tibble(k=c(5,7), n=10)
library(brms)
m <- brms::brm(
  k|trials(n)~ 1,
  d,
  family = binomial(link = "identity"),
  prior = prior(beta(1,1), class = Intercept),
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

