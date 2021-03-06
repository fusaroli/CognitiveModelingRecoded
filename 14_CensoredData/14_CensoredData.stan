//
// This Stan model does something

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> n; 
  int<lower=0> nfails; 
  int<lower=0> z_observed;
}


parameters {
  real<lower=0.25, upper=1> theta; // 0.25 is chance level for the 4-choice questions

}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // Prior
  target += beta_lpdf(theta | 1,1);
  // Observed Data
  target += binomial_lpmf(z_observed | n, theta); 
  // Unobserved Data
  target += nfails * log(binomial_cdf(25 | n, theta) - binomial_cdf(14 | n, theta));
}

generated quantities {
  real<lower=0.25, upper=1> thetaprior; 
  real priorpred;
  real postpred;
    
  thetaprior = beta_rng(1,1);
  // Prior Predictive
  priorpred = binomial_rng(n, thetaprior);
  // Posterior Predictive
  postpred = binomial_rng(n, theta);
}

