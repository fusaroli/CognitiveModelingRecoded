### Geurts data:
# Normal Controls:         
num.errors <- c(15,10,61,11,60,44,63,70,57,11,67,21,89,12,63,11,96,10,37,19,44,
                18,78,27,60,14)
nc         <- c(89,74,128,87,128,121,128,128,128,78,128,106,128,83,128,100,128,
                73,128,86,128,86,128,100,128,79)
kc         <- nc - num.errors
nsc        <- length(kc)

# ADHD:
num.errors <- c(88,50,58,17,40,18,21,50,21,69,19,29,11,76,46,36,37,72,27,92,13,
                39,53,31,49,57,17,10,12,21,39,43,49,17,39,13,68,24,21,27,48,54,
                41,75,38,76,21,41,61,24,28,21)
na         <- c(128,128,128,86,128,117,89,128,110,128,93,107,87,128,128,113,128,
                128,98,128,93,116,128,116,128,128,93,86,86,96,128,128,128,86,128,
                78,128,111,100,95,128,128,128,128,128,128,98,127,128,93,110,96)
ka         <- na - num.errors
nsa        <- length(ka)

# two-sided p-value = .72
t.test(kc / nc, ka / na, alternative=c("two.sided"), paired=F)

data <- list(nc=nc,kc=kc,nsc=nsc,na=na,ka=ka,nsa=nsa) # to be passed on to Stan

file <- file.path(here("29_geurts", "29_geurts.stan"))
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
ggplot(draws_df)+
  geom_density(aes(deltaprior), fill="red", alpha=0.3) +
  geom_density(aes(delta), fill="blue", alpha=0.3) +
  theme_classic()


ggplot(draws_df)+
  geom_density(aes(alphaprior), fill="red", alpha=0.3) +
  geom_density(aes(alpha), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(muprior), fill="red", alpha=0.3) +
  geom_density(aes(mu), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(sigmaprior), fill="red", alpha=0.3) +
  geom_density(aes(sigma), fill="blue", alpha=0.3) +
  theme_classic()

#============ BFs based on logspline fit ===========================
library(polspline) # this package can be installed from within R
fit.posterior <- logspline(draws_df$delta)
fit.prior <- logspline(draws_df$deltaprior)
# 95% confidence interval:
x0 <- qlogspline(0.025,fit.posterior)
x1 <- qlogspline(0.975,fit.posterior)

posterior     <- dlogspline(0, fit.posterior) # this gives the pdf at point delta = 0
prior         <- dlogspline(0, fit.prior)  # height of order--restricted prior at delta = 0
BF01          <- posterior/prior
BF01 

df <- tibble(
  successes = c(kc,ka), 
  words = c(nc,na), 
  ID = c(100 + (1:nsc), 200 + (1:nsa)), 
  condition = c(rep("control", nsc), rep("adhd", nsa)))

geurts_f <- bf(successes|trials(words) ~ 1 + condition + (1|gr(ID, by="condition")))

geurts_p <- c(
  prior(normal(0,1), class = Intercept),
  prior(normal(0, 0.5), class = b),
  prior(normal(0,0.5), class = sd)
)

geurts_m <- brm(
  geurts_f,
  df,
  family = binomial(),
  prior = geurts_p,
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
geurts_m
stancode(geurts_m)
plot(hypothesis(geurts_m, "b_conditioncontrol > 0", class=""))

posterior <- as_draws_df(geurts_m)

ggplot(posterior) +
  geom_density(aes(prior_b), fill="red", alpha=.3) +
  geom_density(aes(b_conditioncontrol), fill="blue", alpha=.3) +
  theme_classic()

ggplot(posterior) +
  geom_density(aes(prior_Intercept), fill="red", alpha=.3) +
  geom_density(aes(Intercept), fill="blue", alpha=.3) +
  theme_classic()

ggplot(posterior) +
  geom_density(aes(prior_sd_ID), fill="red", alpha=.3) +
  geom_density(aes(`sd_ID__Intercept:"condition"condition`), fill="blue", alpha=.3) +
  theme_classic()
