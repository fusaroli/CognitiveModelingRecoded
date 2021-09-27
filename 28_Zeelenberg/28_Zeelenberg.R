
pacman::p_load(
  tidyverse,
  here,
  cmdstanr,
  posterior,
  brms
)

### Zeelenberg data:

# Study Both:         
sb <- c(15,11,15,14,15,18,16,16,18,16,15,13,18,12,11,13,17,18,16,11,17,18,
        12,18,18,14,21,18,17,10,11,12,16,18,17,15,19,12,21,15,16,20,15,19,
        16,16,14,18,16,19,17,11,19,18,16,16,11,19,18,12,15,18,20, 8,12,19,
        16,16,16,12,18,17,11,20)
nb <- 21

# Study Neither: 
sn <- c(15,12,14,15,13,14,10,17,13,16,16,10,15,15,10,14,17,18,19,12,19,18,
        10,18,16,13,15,20,13,15,13,14,19,19,19,18,13,12,19,16,14,17,15,16,
        15,16,13,15,14,19,12,11,17,13,18,13,13,19,18,13,13,16,18,14,14,17,
        12,12,16,14,16,18,13,13)
nn <- 21
ns <- length(sb)

data = list(sb=sb, sn=sn, nb=nb, nn=nn, ns=ns) # to be passed on to Stan

file <- file.path(here("28_Zeelenberg", "28_Zeelenberg.stan"))
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
  geom_density(aes(mualphaprior), fill="red", alpha=0.3) +
  geom_density(aes(mualpha), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(sigmaalphaprior), fill="red", alpha=0.3) +
  geom_density(aes(sigmaalpha), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(muprior), fill="red", alpha=0.3) +
  geom_density(aes(mu), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(sigmaprior), fill="red", alpha=0.3) +
  geom_density(aes(sigma), fill="blue", alpha=0.3) +
  theme_classic()


ggplot(draws_df)+
  geom_density(aes(mualphaprior), fill="red", alpha=0.3) +
  geom_density(aes(mualpha), fill="green", alpha=0.3) +
  geom_density(aes(`alpha[1]`), fill="blue", alpha=0.3) +
  geom_density(aes(`alpha[2]`), fill="blue", alpha=0.3) +
  geom_density(aes(`alpha[3]`), fill="blue", alpha=0.3) +
  theme_classic()



#============ BFs based on logspline fit ===========================
library(polspline) # this package can be installed from within R
fit.posterior <- logspline(draws_df$delta, lbound=0)

# 95% confidence interval:
x0 <- qlogspline(0.025,fit.posterior)
x1 <- qlogspline(0.975,fit.posterior)

posterior     <- dlogspline(0, fit.posterior) # this gives the pdf at point delta = 0
prior         <- 2 * dnorm(0)  # height of order--restricted prior at delta = 0
BF01          <- prior/posterior


df <- tibble(successes=c(sb,sb), trials=21, ID=rep(1:ns, 2), condition=rep(c("both", "none"), each=ns))

zeel_f <- bf(successes|trials(trials) ~ 1 + condition + (1+condition|ID))

zeel_p <- c(
  prior(normal(0,1), class=Intercept),
  prior(normal(0, 0.5), class=b, lb=0),
  prior(normal(0,0.5), class=sd),
  prior(lkj(1), class=cor)
)

zeel_m <- brm(
  zeel_f,
  df,
  family = binomial(),
  prior = zeel_p,
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
zeel_m
stancode(zeel_m)
plot(hypothesis(zeel_m, "b_conditionnone > 0", class=""))

posterior <- as_draws_df(zeel_m)

ggplot(posterior) +
  geom_density(aes(prior_b), fill="red", alpha=.3) +
  geom_density(aes(b_conditionnone), fill="blue", alpha=.3) +
  theme_classic()

ggplot(posterior) +
  geom_density(aes(prior_Intercept), fill="red", alpha=.3) +
  geom_density(aes(Intercept), fill="blue", alpha=.3) +
  theme_classic()

ggplot(posterior) +
  geom_density(aes(prior_sd_ID), fill="red", alpha=.3) +
  geom_density(aes(sd_ID__Intercept), fill="blue", alpha=.3) +
  geom_density(aes(sd_ID__conditionnone), fill="blue", alpha=.3) +
  theme_classic()
