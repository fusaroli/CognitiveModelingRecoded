x <- c(70,80,79,83,77,75,84,78,75,75,78,82,74,81,72,70,75,72,76,77)
y <- c(56,80,63,62,67,71,68,76,79,67,76,74,67,70,62,65,72,72,69,71)

n1 <- length(x)
n2 <- length(y)

# Rescale
y <- y - mean(x)
y <- y / sd(x)
x <- (x - mean(x)) / sd(x); 

data <- list(x=x, y=y, n1=n1, n2=n2) # to be passed on to Stan

df <- tibble(x=x, y=y)

file <- file.path(here("25_TwoSamples", "TwoSamples.stan"))
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
  #init = myinits,
  max_treedepth = 20,
  adapt_delta = 0.99,
)

# Extract posterior samples and include sampling of the prior:
draws_df <- as_draws_df(samples$draws()) 

# Now let's plot alpha
ggplot(draws_df) +
  geom_density(aes(alphaprior), fill="red", alpha=0.3) +
  geom_density(aes(alpha), fill="blue", alpha=0.3) +
  xlab("alpha") +
  ylab("Posterior Density") +
  theme_classic()

# Now let's plot mu
ggplot(draws_df) +
  geom_density(aes(muprior), fill="red", alpha=0.3) +
  geom_density(aes(mu1), fill="blue", alpha=0.3) +
  geom_density(aes(mu2), fill="green", alpha=0.3) +
  xlab("Mu") +
  ylab("Posterior Density") +
  theme_classic()

# Now let's plot sigma
ggplot(draws_df) +
  geom_density(aes(sigmaprior), fill="red", alpha=0.3) +
  geom_density(aes(sigma1), fill="blue", alpha=0.3) +
  geom_density(aes(sigma2), fill="green", alpha=0.3) +
  xlab("Sigma") +
  ylab("Posterior Density") +
  theme_classic()

## Against each other
ggplot(draws_df) +
  geom_point(aes(mu1, sigma1), alpha=0.3, color="blue") +
  geom_point(aes(mu2, sigma2), alpha=0.3, color="green") +
  xlab("Mu") +
  ylab("Sigma") +
  theme_classic()

ggplot(draws_df) +
  geom_point(aes(muprior, sigmaprior), alpha=0.1) +
  geom_point(aes(mu1, sigma1), alpha=0.3, color="blue") +
  geom_point(aes(mu2, sigma2), alpha=0.3, color="green") +
  xlab("Mu") +
  ylab("Sigma") +
  theme_classic()


# Now let's plot predictive checks for theta (prior and posterior)
ggplot(draws_df) +
  geom_density(aes(PredictedOutcomePrior), fill="red", linetype="dashed", alpha=0.3) +
  geom_density(aes(PredictedOutcomePosterior), fill="blue", alpha=0.3) +
  geom_density(data  = df, aes(x-y), fill="grey", linetype="dashed", alpha=0.3) +
  xlab("Difference") +
  ylab("Posterior Density") +
  theme_classic()

#============ BFs based on logspline fit ===========================
library(logspline) # this package can be installed from within R

fit.posterior <- logspline(draws_df$alpha)

# 95% confidence interval:
x0 <- qlogspline(0.025,fit.posterior)
x1 <- qlogspline(0.975,fit.posterior)

posterior <- dlogspline(0, fit.posterior) # this gives the pdf at point delta = 0
prior     <- dlogspline(0, logspline(draws_df$alphaprior))  # height of order-restricted prior at delta = 0
BF01      <- prior/posterior
BF01

#============ Plot Prior and Posterior  ===========================
ggplot(draws_df) +
  geom_density(aes(alphaprior), linetype="dashed", color="red") +
  geom_density(aes(alpha), color="blue") +
  geom_point(aes(x=0, y = posterior), colour="blue", size=4)+
  geom_point(aes(x=0, y = prior), colour="red", size=4)+
  xlab("BF for mean") +
  ylab("density") +
  geom_label(
    label=as.character(round(dlogspline(0, fit.posterior),2)), 
    x = 0,
    y = posterior +.05,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="#69b3a2", alpha=0.3) +
  geom_label(
    label=as.character(round(2*dnorm(0, 0, 1),2)), 
    x = 0,
    y = prior +.05,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="#69b3a2", alpha=0.3) +
  theme_classic()
