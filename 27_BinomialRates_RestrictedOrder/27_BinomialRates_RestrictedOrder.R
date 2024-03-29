# pledger data:
s1 <- 424
s2 <- 5416
n1 <- 777
n2 <- 9072

data <- list(s1=s1, s2=s2, n1=n1, n2=n2) # to be passed on to Stan

# Analytical Bayes factor:
log.BF01 <- lchoose(n1, s1) + lchoose(n2, s2) + log(n1 + 1) + log(n2 + 1) - 
  lchoose((n1 + n2), (s1 + s2)) - log(n1 + n2 + 1)
BF01 <- exp(log.BF01)

file <- file.path(here("26_BinomialRates", "26_BinomialRates.stan"))
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
  geom_density(aes(deltaprior), fill="red", alpha=0.3) +
  geom_density(aes(delta), fill="blue", alpha=0.3) +
  xlab("delta") +
  ylab("Posterior Density") +
  theme_classic()

# Now let's plot mu
ggplot(draws_df) +
  geom_density(aes(thetaprior1), fill="red", alpha=0.3) +
  geom_density(aes(thetaprior2), fill="red", alpha=0.3) +
  geom_density(aes(theta1), fill="blue", alpha=0.3) +
  geom_density(aes(theta2), fill="green", alpha=0.3) +
  xlab("theta") +
  ylab("Posterior Density") +
  theme_classic()


## Against each other
ggplot(draws_df) +
  geom_point(aes(theta1, theta2), alpha=0.3, color="blue") +
  xlab("theta1") +
  ylab("theta2") +
  theme_classic()

## Against each other
ggplot(draws_df) +
  geom_point(aes(thetaprior1, thetaprior2), alpha=0.3, color="grey") +
  geom_point(aes(theta1, theta2), alpha=0.3, color="blue") +
  xlab("theta1") +
  ylab("theta2") +
  theme_classic()


# Now let's plot predictive checks for theta (prior and posterior)
p1 <- ggplot(draws_df) +
  geom_density(aes(PredictedOutcomePrior1), fill="red", linetype="dashed", alpha=0.3) +
  geom_density(aes(PredictedOutcomePosterior1), fill="blue", alpha=0.3) +
  geom_point(aes(x=s1, y=0), color="black") +
  xlab("Condom users") +
  ylab("Posterior Density") +
  theme_classic()

p2 <- ggplot(draws_df) +
  geom_density(aes(PredictedOutcomePrior2), fill="red", linetype="dashed", alpha=0.3) +
  geom_density(aes(PredictedOutcomePosterior2), fill="blue", alpha=0.3) +
  geom_point(aes(x=s2, y=0), color="black") +
  xlab("Condom users") +
  ylab("Posterior Density") +
  theme_classic()

p1+p2


#####################################################
# Order-restriction. H2: delta < 0
######################################################

#============ BFs based on logspline fit ===========================
library(polspline) # this package can be installed from within R

fit.posterior <- logspline(draws_df$delta, lbound=0, ubound=1)
posterior     <- dlogspline(0, fit.posterior) # this gives the pdf at point delta = 0
BF02          <- 1/(posterior/2) # 0.26, BF20 = 3.78

#============ Plot Prior and Posterior  ===========================
ggplot(draws_df) +
  geom_density(aes(deltaprior), linetype="dashed", color="red") +
  geom_density(aes(delta), color="blue") +
  geom_point(aes(x=0, y = posterior), colour="blue", size=4)+
  geom_point(aes(x=0, y = prior), colour="red", size=4)+
  xlab("BF for mean") +
  ylab("density") +
  geom_label(
    label=as.character(round(dlogspline(0, fit.posterior),2)), 
    x = .1,
    y = posterior,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="#69b3a2", alpha=0.3) +
  geom_label(
    label=as.character(round(2*dnorm(0, 0, 1),2)), 
    x = .1,
    y = prior,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="#69b3a2", alpha=0.3) +
  theme_classic()
