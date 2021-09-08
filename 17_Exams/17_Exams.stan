//
// This Stan model does something

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> p; // number of people
  int<lower=0> k[p]; // number of correct answers
  int<lower=1> n; // number of questions
}

transformed data {
  real psi;
  // First Group Guesses
  psi = .5;
}

parameters {
  // Second Group Has Some Unknown Greater Rate Of Success
  vector<lower=0,upper=1>[p] phi; 
  real<lower=.5,upper=1> phiprior;
  real<lower=.5,upper=1> mu;  // Second Group Mean
  real<lower=.5,upper=1> muprior;  // Second Group Mean
  real<lower=0> sigma;
  real<lower=0> sigmaprior;
  real<lower=0,upper=1> predphi;
}

transformed parameters {
  vector[2] lp_parts[p];
  
  // Data Follow Binomial With Rate Given By Each Person's Group Assignment
  // Each Person Belongs To One Of Two Latent Groups
  for (i in 1:p) {
    lp_parts[i, 1] = log(.5) + binomial_lcdf(k[i] | n, phi[i]);
    lp_parts[i, 2] = log(.5) + binomial_lcdf(k[i] | n, psi); 
  }
}

model {
 phiprior ~ uniform(.5,1);
 // Second Group Precision
 sigma ~ normal(0, 10); // new
 sigmaprior ~ normal(0, 10); // new
 // Posterior Predictive For Second Group
 predphi ~ normal(mu, sigma)T[0,1]; // new
 
 // Second Group Drawn From A Censored Gaussian Distribution
 for (i in 1:p)                       // new
    phi[i] ~ normal(mu, sigma)T[0,1]; // new
 
 for (i in 1:p)
    target += log_sum_exp(lp_parts[i]);  
}

generated quantities {
  int<lower = 0,upper = 1> z[p];
  vector<lower=0,upper=1>[p] theta; // new
  
  for (i in 1:p) {
    vector[2] prob;
    prob = softmax(lp_parts[i]);
    z[i] = bernoulli_rng(prob[1]);
    theta[i] = (z[i] == 0) * psi + (z[i] == 1) * phi[i]; // new
  }
}
