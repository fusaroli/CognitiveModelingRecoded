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
  
  int<lower=1> k_d; // n of participants
  int<lower=0> signal_d; // cases
  int<lower=0> hits_d[k_d]; // true positives
  int<lower=0> noise_d; // non-casees
  int<lower=0> falsealarms_d[k_d]; // false positives
  
}

// The parameters accepted by the model..
parameters {
  
  real<lower=0,upper=1> xic_i;
  real<lower=0,upper=1> xid_i;
  real<lower=0,upper=1> xic_d;
  real<lower=0,upper=1> xid_d;
  real<lower=0,upper=1> xi_prior;
  vector[k_i] deltac_i;
  vector[k_i] deltad_i;
  vector[k_d] deltac_d;
  vector[k_d] deltad_d;
  real deltaprior;
  
  real mu_crit_i;
  real mu_crit_d;
  real mu_disc_i;
  real mu_disc_d;
  real muprior;
  
  real<lower=0> sigma_crit_i_new;
  real<lower=0> sigma_crit_d_new;
  real<lower=0> sigma_disc_i_new;
  real<lower=0> sigma_disc_d_new;
  real<lower=0> sigma_prior_new;
  
}

transformed parameters{
  
  vector[k_i] criterion_i;
  vector[k_i] discriminability_i;
  vector[k_d] criterion_d;
  vector[k_d] discriminability_d;
  real discriminabilityprior;
  real criterionprior;
  
  real<lower=0> sigma_crit_i;
  real<lower=0> sigma_crit_d;
  real<lower=0> sigma_disc_i;
  real<lower=0> sigma_disc_d;
  real<lower=0> sigmaprior;
  
  real<lower=0,upper=1> theta_h_i[k_i];
  real<lower=0,upper=1> theta_f_i[k_i];
  
  real<lower=0,upper=1> theta_h_d[k_d];
  real<lower=0,upper=1> theta_f_d[k_d];
  
  real<lower=0,upper=1> theta_h_prior;
  real<lower=0,upper=1> theta_f_prior;
  
  sigma_crit_i = fabs(xic_i) * sigma_crit_i_new;
  sigma_crit_d = fabs(xic_d) * sigma_crit_d_new;
  sigma_disc_i = fabs(xid_i) * sigma_disc_i_new;
  sigma_disc_d = fabs(xid_d) * sigma_disc_d_new;
  sigmaprior = fabs(xi_prior) * sigma_prior_new;
  
  criterion_i = mu_crit_i + xic_i * deltac_i;
  discriminability_i = mu_disc_i + xid_i * deltad_i;
  criterion_d = mu_crit_d + xic_d * deltac_d;
  discriminability_d = mu_disc_d + xid_d * deltad_d;
  
  criterionprior = muprior + xi_prior * deltaprior;
  discriminabilityprior = muprior + xi_prior * deltaprior;
  
  
  // Reparameterization Using Equal-Variance Gaussian SDT
  theta_h_prior = Phi(discriminabilityprior / 2 - criterionprior);
  theta_f_prior = Phi(-discriminabilityprior / 2 - criterionprior);
    
  for(i in 1:k_i) {
    theta_h_i[i] = Phi(discriminability_i[i] / 2 - criterion_i[i]);
    theta_f_i[i] = Phi(-discriminability_i[i] / 2 - criterion_i[i]);
  }
  for(d in 1:k_d) {
    theta_h_d[d] = Phi(discriminability_d[d] / 2 - criterion_d[d]);
    theta_f_d[d] = Phi(-discriminability_d[d] / 2 - criterion_d[d]);
  }
  
}

// The model to be estimated. 
model {

  // These Priors over Discriminability and Bias Correspond 
  // to Uniform Priors over the Hit and False Alarm Rates
  mu_crit_i ~ normal(0, 5);
  mu_crit_d ~ normal(0, 5);
  mu_disc_i ~ normal(0, 5);
  mu_disc_d ~ normal(0, 5);
  muprior ~ normal(0, 5);
  
  sigma_crit_i_new ~ normal(0, 3);
  sigma_crit_d_new ~ normal(0, 3);
  sigma_disc_i_new ~ normal(0, 3);
  sigma_disc_d_new ~ normal(0, 3);
  sigma_prior_new ~ normal(0, 3);
  
  xic_i ~ beta(1, 1);  // can be removed
  xid_i ~ beta(1, 1);  // can be removed
  xic_d ~ beta(1, 1);  // can be removed
  xid_d ~ beta(1, 1);  // can be removed
  xi_prior ~ beta(1, 1);  // can be removed
  
  deltac_i ~ normal(0, sigma_crit_i_new);
  deltad_i ~ normal(0, sigma_disc_i_new);
  deltac_d ~ normal(0, sigma_crit_d_new);
  deltad_d ~ normal(0, sigma_disc_d_new);
  deltaprior ~ normal(0, sigma_prior_new);
  
  // Observed counts
  hits_i ~ binomial(signal_i, theta_h_i);
  falsealarms_i ~ binomial(noise_i, theta_f_i);
  hits_d ~ binomial(signal_d, theta_h_d);
  falsealarms_d ~ binomial(noise_d, theta_f_d);
}

