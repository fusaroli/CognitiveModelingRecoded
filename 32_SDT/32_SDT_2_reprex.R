library(cmdstanr)

std_i <- matrix(
  c(3,1,1,3,4,0,0,4,4,4,0,0,4,1,0,3,4,3,0,1,4,4,0,0,4,1,0,3,4,3,0,1,4,
    0,0,4,4,2,0,2,3,2,1,2,4,2,0,2,4,0,0,4,3,4,1,0,3,4,1,0,4,1,0,3,3,2,
    1,2,4,1,0,3,3,1,1,3,4,1,0,3,3,2,1,2,4,0,0,4,3,4,1,0,4,4,0,0,4,2,0,
    2,4,4,0,0,4,4,0,0,4,4,0,0,4,4,0,0,4,0,0,4,2,3,2,1,4,0,0,4,4,4,0,0,
    2,3,2,1,4,0,0,4,4,3,0,1,4,4,0,0,4,1,0,3,4,1,0,3,4,4,0,0),
  nrow=40,ncol=4, byrow=T)

  
hits_i <- std_i[, 1]
falsealarms_i <- std_i[, 2]
missed_i <- std_i[, 3]
correctrejections_i <- std_i[, 4]

signal_i <- hits_i + missed_i
noise_i <- falsealarms_i + correctrejections_i
signal_i <- signal_i[1]; 
noise_i <- noise_i[1] #Each subject gets same number of signal and noise trials 

k_i <- nrow(std_i) 

data <- list(
  hits_i = hits_i, 
  falsealarms_i = falsealarms_i,
  signal_i = signal_i,
  noise_i = noise_i, 
  k_i = k_i) # To be passed on to Stan

stan_file <- write_stan_file("
// Signal detection theory model
//

// The input data 
data {
  
  int<lower=1> k_i; // n of participants
  int<lower=0> signal_i; // cases
  int<lower=0> hits_i[k_i]; // true positives
  int<lower=0> noise_i; // non-casees
  int<lower=0> falsealarms_i[k_i]; // false positives
  
}

// The parameters accepted by the model.
parameters {
  
  vector[k_i] criterion_i;
  real criterionprior;
  
  vector[k_i] discriminability_i;
  real discriminabilityprior;
  
  real mu_crit_i;
  real mu_disc_i;
  real muprior;
  
  real<lower=0> sigma_crit_i;
  real<lower=0> sigma_disc_i;
  real<lower=0> sigmaprior;
  
}

transformed parameters{
  real<lower=0,upper=1> theta_h_i[k_i];
  real<lower=0,upper=1> theta_f_i[k_i];
  
  real<lower=0,upper=1> theta_h_prior;
  real<lower=0,upper=1> theta_f_prior;
  
  // Reparameterization Using Equal-Variance Gaussian SDT
  theta_h_prior = Phi(discriminabilityprior / 2 - criterionprior);
  theta_f_prior = Phi(-discriminabilityprior / 2 - criterionprior);
    
  for(i in 1:k_i) {
    theta_h_i[i] = Phi(discriminability_i[i] / 2 - criterion_i[i]);
    theta_f_i[i] = Phi(-discriminability_i[i] / 2 - criterion_i[i]);
  }
  
}

// The model to be estimated. 
model {

  // These Priors over Discriminability and Bias Correspond 
  // to Uniform Priors over the Hit and False Alarm Rates
  mu_crit_i ~ normal(0, 5);
  mu_disc_i ~ normal(0, 5);
  muprior ~ normal(0, 5);
  
  sigma_crit_i ~ normal(0, 3);
  sigma_disc_i ~ normal(0, 3);
  sigmaprior ~ normal(0, 3);
  
  discriminability_i ~ normal(mu_disc_i, sigma_disc_i);
  discriminabilityprior ~ normal(muprior, sigmaprior);
  criterion_i ~ normal(mu_crit_i, sigma_crit_i);
  criterionprior ~ normal(muprior, sigmaprior);
  
  // Observed counts
  hits_i ~ binomial(signal_i, theta_h_i);
  falsealarms_i ~ binomial(noise_i, theta_f_i);
  
}")
  
mod <- cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE), pedantic = TRUE)

samples <- mod$sample(
  data = data,
  seed = 123,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 100,
  max_treedepth = 20,
  adapt_delta = 0.99,
)

# Some basic quality checks of prior / posterior updates (was the prior too narrow/broad/badly positioned?)

draws_df <- as_draws_df(samples$draws()) 

ggplot(draws_df)+
  geom_density(aes(muprior), fill="red", alpha=0.3) +
  geom_density(aes(`mu_crit_i`), fill="blue", alpha=0.3) +
  geom_density(aes(`mu_disc_i`), fill="green", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(sigmaprior), fill="red", alpha=0.3) +
  geom_density(aes(`sigma_crit_i`), fill="blue", alpha=0.3) +
  geom_density(aes(`sigma_disc_i`), fill="green", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(criterionprior), fill="red", alpha=0.3) +
  geom_density(aes(`criterion_i[1]`), fill="blue", alpha=0.3) +
  geom_density(aes(`criterion_i[2]`), fill="blue", alpha=0.3) +
  geom_density(aes(`criterion_i[3]`), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(`discriminabilityprior`), fill="red", alpha=0.3) +
  geom_density(aes(`discriminability_i[1]`), fill="blue", alpha=0.3) +
  geom_density(aes(`discriminability_i[2]`), fill="blue", alpha=0.3) +
  geom_density(aes(`discriminability_i[3]`), fill="blue", alpha=0.3) +
  theme_classic()

## Now quality checks of joint distributions against the prior

ggplot(draws_df)+
  geom_point(aes(criterionprior, sample(discriminabilityprior)), color="red", alpha=0.05) + # sample() is needed because of how I generated the prior samples (badly)
  geom_point(aes(`criterion_i[1]`, `discriminability_i[1]`), color="blue", alpha=0.1) +
  geom_point(aes(`criterion_i[2]`, `discriminability_i[2]`), color="green", alpha=0.1) +
  geom_point(aes(`criterion_i[3]`, `discriminability_i[3]`), color="purple", alpha=0.1) +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(muprior, sigmaprior), color="red", alpha=0.05) +
  geom_point(aes(`mu_crit_i`, `sigma_crit_i`), color="green", alpha=0.1) +
  geom_point(aes(`mu_disc_i`, `sigma_disc_i`), color="blue", alpha=0.1) + 
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(muprior, sample(muprior)), color="red", alpha=0.05) +
  geom_point(aes(`mu_crit_i`, `mu_disc_i`), color="green", alpha=0.1) +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(sigmaprior, sample(sigmaprior)), color="red", alpha=0.05) +
  geom_point(aes(`sigma_crit_i`, `sigma_disc_i`), color="green", alpha=0.1) +
  theme_classic()


# Extract diagnostics for divergences

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
  geom_point(aes(`mu_disc_i`, `sigma_disc_i`), alpha=0.1) +
  geom_point(data=x, aes(`mu_disc_i`, `sigma_disc_i`), color="red") +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(`mu_crit_i`, `mu_disc_i`), alpha=0.1) +
  geom_point(data=x, aes(`mu_crit_i`, `mu_disc_i`), color="red") +
  theme_classic()


## Trace plots
ggplot(draws_df, aes(.iteration, sigma_disc_i, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, sigma_crit_i, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()

ggplot(draws_df, aes(.iteration, mu_disc_i, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, mu_crit_i, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()

## Plots
ggplot(draws_df) +
  geom_point(aes(`mu_disc_i`, `mu_crit_i`), color="red", alpha=0.1) +
  geom_point(aes(`mu_disc_d`, `mu_crit_d`), color="blue", alpha=0.1) +
  geom_rug(aes(`mu_disc_i`, `mu_crit_i`), color="red", size=0.1, alpha=0.1) +
  geom_rug(aes(`mu_disc_d`, `mu_crit_d`), color="blue", size=0.1, alpha=0.1) +
  theme_classic()

