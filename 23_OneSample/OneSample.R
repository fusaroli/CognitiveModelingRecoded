# Read data Dr. Smith
Winter <- c(-0.05,0.41,0.17,-0.13,0.00,-0.05,0.00,0.17,0.29,0.04,0.21,0.08,0.37,
            0.17,0.08,-0.04,-0.04,0.04,-0.13,-0.12,0.04,0.21,0.17,0.17,0.17,
            0.33,0.04,0.04,0.04,0.00,0.21,0.13,0.25,-0.05,0.29,0.42,-0.05,0.12,
            0.04,0.25,0.12)

Summer <- c(0.00,0.38,-0.12,0.12,0.25,0.12,0.13,0.37,0.00,0.50,0.00,0.00,-0.13,
            -0.37,-0.25,-0.12,0.50,0.25,0.13,0.25,0.25,0.38,0.25,0.12,0.00,0.00,
            0.00,0.00,0.25,0.13,-0.25,-0.38,-0.13,-0.25,0.00,0.00,-0.12,0.25,
            0.00,0.50,0.00)

x <- Winter - Summer  # allowed because it is a within-subjects design
x <- x / sd(x)        # standardize

n <- length(Winter) # number of subjects

data <- list(x = x, n = n) # to be passed on to Stan
df <- tibble(x=x)

file <- file.path(here("23_OneSample", "OneSample.stan"))
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

# Now let's plot mu
ggplot(draws_df) +
  geom_density(aes(mu), fill="blue", alpha=0.3) +
  geom_density(aes(muprior), fill="red", alpha=0.3) +
  xlab("Mu") +
  ylab("Posterior Density") +
  theme_classic()

# Now let's plot sigma
ggplot(draws_df) +
  geom_density(aes(sigma), fill="blue", alpha=0.3) +
  geom_density(aes(sigmaprior), fill="red", alpha=0.3) +
  xlab("Sigma") +
  ylab("Posterior Density") +
  theme_classic()

## Against each other
ggplot(draws_df,aes(mu , sigma)) +
  geom_point(alpha=0.3) +
  xlab("Mu") +
  ylab("Sigma") +
  theme_classic()

ggplot(draws_df) +
  geom_point(aes(muprior , sigmaprior), alpha=0.1) +
  geom_point(aes(mu , sigma), alpha=0.3, color = "blue") +
  xlab("Mu") +
  ylab("Sigma") +
  theme_classic()

# Now let's plot predictive checks for theta (prior and posterior)
ggplot(draws_df) +
  geom_density(aes(PredictedOutcomePrior), fill="red", linetype="dashed", alpha=0.3) +
  geom_density(aes(PredictedOutcomePosterior), fill="blue", alpha=0.3) +
  geom_density(data  = df, aes(x), fill="grey", linetype="dashed", alpha=0.3) +
  xlab("Difference") +
  ylab("Posterior Density") +
  theme_classic()


#============ BFs based on logspline fit ===========================
library(logspline) # this package can be installed from within R

fit.posterior <- logspline(draws_df$mu)

# 95% confidence interval:
x0 <- qlogspline(0.025,fit.posterior)
x1 <- qlogspline(0.975,fit.posterior)

posterior <- dlogspline(0, fit.posterior) # this gives the pdf at point delta = 0
prior     <- dnorm(0)          # height of order-restricted prior at delta = 0
BF01      <- posterior/prior
BF01

#============ Plot Prior and Posterior  ===========================
ggplot(draws_df) +
  geom_density(aes(muprior), linetype="dashed", color="red") +
  geom_density(aes(mu), color="blue") +
  geom_point(aes(x=0, y = dlogspline(0, fit.posterior)), colour="blue", size=4)+
  geom_point(aes(x=0, y = dnorm(0, 0, 1)), colour="red", size=4)+
  xlab("BF for mean") +
  ylab("density") +
  geom_label(
    label=as.character(round(dlogspline(0, fit.posterior),2)), 
    x = 0,
    y = dlogspline(0, fit.posterior) +.2,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="#69b3a2", alpha=0.3) +
  geom_label(
    label=as.character(round(dnorm(0, 0, 1),2)), 
    x = 0,
    y = dnorm(0, 0, 1) +.2,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="#69b3a2", alpha=0.3) +
  theme_classic()



