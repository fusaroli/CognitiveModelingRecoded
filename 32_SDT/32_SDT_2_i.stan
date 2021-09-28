//
// This Stan model does something
//

// The input data 
data {
  
  int<lower=1> k_i; // n of participants
  int<lower=0> signal_i; // cases
  int<lower=0> hits_i[k_i]; // true positives
  int<lower=0> noise_i; // non-casees
  int<lower=0> falsealarms_i[k_i]; // false positives
  
}

// The parameters accepted by the model..
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
  
}

