pacman::p_load(
  tidyverse,
  here,
  cmdstanr,
  posterior
)

std_i <- matrix(
  c(3,1,1,3,4,0,0,4,4,4,0,0,4,1,0,3,4,3,0,1,4,4,0,0,4,1,0,3,4,3,0,1,4,
    0,0,4,4,2,0,2,3,2,1,2,4,2,0,2,4,0,0,4,3,4,1,0,3,4,1,0,4,1,0,3,3,2,
    1,2,4,1,0,3,3,1,1,3,4,1,0,3,3,2,1,2,4,0,0,4,3,4,1,0,4,4,0,0,4,2,0,
    2,4,4,0,0,4,4,0,0,4,4,0,0,4,4,0,0,4,0,0,4,2,3,2,1,4,0,0,4,4,4,0,0,
    2,3,2,1,4,0,0,4,4,3,0,1,4,4,0,0,4,1,0,3,4,1,0,3,4,4,0,0),
  nrow=40,ncol=4, byrow=T)


std_d <- matrix(
  c(4,1,0,3,4,0,0,4,4,0,0,4,4,0,0,4,4,0,0,4,4,0,0,4,4,0,0,4,4,0,0,4,4,
    0,0,4,4,0,0,4,4,2,0,2,2,2,2,2,4,0,0,4,4,0,0,4,3,2,1,2,3,0,1,4,3,1,
    1,3,2,1,2,3,4,4,0,0,4,2,0,2,4,1,0,3,2,0,2,4,4,0,0,4,3,0,1,4,4,0,0,
    4,3,0,1,4,4,0,0,4,3,4,1,0,1,3,3,1,4,4,0,0,4,4,0,0,3,1,1,3,3,1,1,3,
    4,0,0,4,3,0,1,4,4,0,0,4,4,0,0,4,4,0,0,4,4,0,0,4,4,2,0,2),
  nrow=40,ncol=4, byrow=T)

hits_i <- std_i[, 1]
hits_d <- std_d[, 1]
falsealarms_i <- std_i[, 2]
falsealarms_d <- std_d[, 2]
missed_i <- std_i[, 3]
missed_d <- std_d[, 3]
correctrejections_i <- std_i[, 4]
correctrejections_d <- std_d[, 4]

signal_i <- hits_i + missed_i
signal_d <- hits_d + missed_d
noise_i <- falsealarms_i + correctrejections_i
noise_d <- falsealarms_d + correctrejections_d
signal_i <- signal_i[1]; 
signal_d <- signal_d[1]; 
noise_i <- noise_i[1] #Each subject gets same number of signal and noise trials 
noise_d <- noise_d[1] 

k_i <- nrow(std_i) 
k_d <- nrow(std_d) 
  
data <- list(
  hits_i = hits_i, 
  hits_d = hits_d, 
  falsealarms_i = falsealarms_i,
  falsealarms_d = falsealarms_d, 
  signal_i = signal_i,
  signal_d = signal_d, 
  noise_i = noise_i, 
  noise_d = noise_d, 
  k_i = k_i, 
  k_d = k_d) # To be passed on to Stan

  
  
file <- file.path(here("32_SDT", "32_SDT_2.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE), pedantic = TRUE)

samples <- mod$sample(
  data = data,
  seed = 123,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  init=0,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 500,
  max_treedepth = 20,
  adapt_delta = 0.99,
)

draws_df <- as_draws_df(samples$draws()) 


ggplot(draws_df)+
  geom_density(aes(muprior), fill="red", alpha=0.3) +
  geom_density(aes(`mu_crit_i`), fill="blue", alpha=0.3) +
  geom_density(aes(`mu_crit_d`), fill="blue", alpha=0.3) +
  geom_density(aes(`mu_disc_i`), fill="green", alpha=0.3) +
  geom_density(aes(`mu_disc_d`), fill="green", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(sigmaprior), fill="red", alpha=0.3) +
  geom_density(aes(`sigma_crit_i`), fill="blue", alpha=0.3) +
  geom_density(aes(`sigma_crit_d`), fill="blue", alpha=0.3) +
  geom_density(aes(`sigma_disc_i`), fill="green", alpha=0.3) +
  geom_density(aes(`sigma_disc_d`), fill="green", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(criterionprior), fill="red", alpha=0.3) +
  geom_density(aes(`criterion_i[1]`), fill="blue", alpha=0.3) +
  geom_density(aes(`criterion_i[2]`), fill="blue", alpha=0.3) +
  geom_density(aes(`criterion_i[3]`), fill="blue", alpha=0.3) +
  geom_density(aes(`criterion_d[1]`), fill="green", alpha=0.3) +
  geom_density(aes(`criterion_d[2]`), fill="green", alpha=0.3) +
  geom_density(aes(`criterion_d[3]`), fill="green", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(`discriminabilityprior`), fill="red", alpha=0.3) +
  geom_density(aes(`discriminability_i[1]`), fill="blue", alpha=0.3) +
  geom_density(aes(`discriminability_i[2]`), fill="blue", alpha=0.3) +
  geom_density(aes(`discriminability_i[3]`), fill="blue", alpha=0.3) +
  geom_density(aes(`discriminability_d[1]`), fill="green", alpha=0.3) +
  geom_density(aes(`discriminability_d[2]`), fill="green", alpha=0.3) +
  geom_density(aes(`discriminability_d[3]`), fill="green", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(criterionprior, discriminabilityprior), color="red", alpha=0.05) +
  geom_point(aes(`criterion_i[1]`, `discriminability_i[1]`), color="green", alpha=0.1) +
  geom_point(aes(`criterion_i[2]`, `discriminability_i[2]`), color="green", alpha=0.1) +
  geom_point(aes(`criterion_d[1]`, `discriminability_d[1]`), color="blue", alpha=0.1) +
  geom_point(aes(`criterion_d[2]`, `discriminability_d[2]`), color="blue", alpha=0.1) +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(muprior, sigmaprior), color="red", alpha=0.05) +
  geom_point(aes(`mu_crit_i`, `sigma_crit_i`), color="green", alpha=0.1) +
  geom_point(aes(`mu_disc_i`, `sigma_disc_i`), color="blue", alpha=0.1) + 
  geom_point(aes(`mu_crit_d`, `sigma_crit_d`), color="orange", alpha=0.1) +
  geom_point(aes(`mu_disc_d`, `sigma_disc_d`), color="yellow", alpha=0.1) +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(muprior, sample(muprior)), color="red", alpha=0.05) +
  geom_point(aes(`mu_crit_i`, `mu_disc_i`), color="green", alpha=0.1) +
  geom_point(aes(`mu_crit_d`, `mu_disc_d`), color="blue", alpha=0.1) +
  geom_point(aes(`mu_disc_i`, `mu_disc_d`), color="orange", alpha=0.1) + 
  geom_point(aes(`mu_crit_i`, `mu_crit_d`), color="yellow", alpha=0.1) +
  theme_classic()


# Extract diagnostics
samples$cmdstan_diagnose() # summary
diagnostics_df <- as_draws_df(samples$sampler_diagnostics())
print(diagnostics_df)

draws_df$divergence=diagnostics_df$divergent__
x <- draws_df[draws_df$divergence==1,]


ggplot(draws_df)+
  geom_point(aes(`mu_crit_i`, `sigma_crit_i`), alpha=0.1) +
  geom_point(data=x, aes(`mu_crit_i`, `sigma_crit_i`), color="red") +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(`mu_crit_d`, `sigma_crit_d`), alpha=0.1) +
  geom_point(data=x, aes(`mu_crit_d`, `sigma_crit_d`), color="red") +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(`mu_disc_i`, `sigma_disc_i`), alpha=0.1) +
  geom_point(data=x, aes(`mu_disc_i`, `sigma_disc_i`), color="red") +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(`mu_disc_d`, `sigma_disc_d`), alpha=0.1) +
  geom_point(data=x, aes(`mu_disc_d`, `sigma_disc_d`), color="red") +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(`mu_crit_i`, `mu_disc_i`), alpha=0.1) +
  geom_point(data=x, aes(`mu_crit_i`, `mu_disc_i`), color="red") +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(`mu_crit_d`, `mu_disc_d`), alpha=0.1) +
  geom_point(data=x, aes(`mu_crit_d`, `mu_disc_d`), color="red") +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(`mu_crit_i`, `mu_crit_d`), alpha=0.1) +
  geom_point(data=x, aes(`mu_crit_i`, `mu_crit_d`), color="red") +
  theme_classic()

ggplot(draws_df, aes(.iteration, sigma_disc_i, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, sigma_disc_d, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, sigma_crit_i, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, sigma_crit_d, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()

ggplot(draws_df, aes(.iteration, mu_disc_i, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, mu_disc_d, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, mu_crit_i, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, mu_crit_d, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()

## Plots
ggplot(draws_df) +
  geom_point(aes(`mu_disc_i`, `mu_crit_i`), color="red", alpha=0.1) +
  geom_point(aes(`mu_disc_d`, `mu_crit_d`), color="blue", alpha=0.1) +
  geom_rug(aes(`mu_disc_i`, `mu_crit_i`), color="red", size=0.1, alpha=0.1) +
  geom_rug(aes(`mu_disc_d`, `mu_crit_d`), color="blue", size=0.1, alpha=0.1) +
  theme_classic()

### Now brms to the rescue

df <- tibble(
  hits = c(hits_i, hits_d),
  falsealarms = c(falsealarms_i,falsealarms_d),
  signal = c(hits_i + missed_i, hits_d + missed_d),
  noise = c(falsealarms_i + correctrejections_i, falsealarms_d + correctrejections_d)) # To be passed on to Stan

pivot_longer()