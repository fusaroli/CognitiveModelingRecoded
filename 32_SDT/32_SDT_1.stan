//
// This Stan model does something
//

// The input data 
data {
  int<lower=1>  k; // n of participants
  int<lower=0> signal[k]; // cases
  int<lower=0> hits[k]; // true positives
  int<lower=0> noise[k]; // non-casees
  int<lower=0> falsealarms[k]; // false positives
  
}

// The parameters accepted by the model..
parameters {
  vector[k] criterion;
  real criterionprior;
  vector[k] discriminability;
  real discriminabilityprior;
}

transformed parameters{
  real<lower=0,upper=1> theta_h[k];
  real<lower=0,upper=1> theta_f[k];
  real<lower=0,upper=1> theta_h_prior;
  real<lower=0,upper=1> theta_f_prior;
  
  // Reparameterization Using Equal-Variance Gaussian SDT
  theta_h_prior = Phi(discriminabilityprior / 2 - criterionprior);
  theta_f_prior = Phi(-discriminabilityprior / 2 - criterionprior);
    
  for(i in 1:k) {
    theta_h[i] = Phi(discriminability[i] / 2 - criterion[i]);
    theta_f[i] = Phi(-discriminability[i] / 2 - criterion[i]);
  }
  
}

// The model to be estimated. 
model {

  // These Priors over Discriminability and Bias Correspond 
  // to Uniform Priors over the Hit and False Alarm Rates
  discriminability ~ normal(0, inv_sqrt(.5));
  discriminabilityprior ~ normal(0, inv_sqrt(.5));
  criterion ~ normal(0, inv_sqrt(2));
  criterionprior ~ normal(0, inv_sqrt(2));
  
  // Observed counts
  hits ~ binomial(signal, theta_h);
  falsealarms ~ binomial(noise, theta_f);
}

