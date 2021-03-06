//
// This Stan model does something
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> n; // n of questions
  int<lower=1> p; // n of participants
  int<lower=0,upper=n> k[p];
}

// The parameters 
parameters {
  vector<lower=0,upper=1>[2] psi;
  
} 
transformed parameters {
  vector[2] lp_parts[p];
  for (i in 1:p) {
    lp_parts[i, 1] = log(.5) + binomial_lpmf(k[i] | n, psi[1]);
    lp_parts[i, 2] = log(.5) + binomial_lpmf(k[i] | n, psi[2]);
  } 
}
// The model to be estimated. 
model {
  // Bona Fide Group has Unknown Success Rate Above Chance
  target += uniform_lpdf(psi[1] | 0.5, 1);
  // Malingering Group has Unknown Success Rate Below Bona Fide
  target += uniform_lpdf(psi[2] | 0, psi[1]);
  
  for (i in 1:p)
    target +=log_sum_exp(lp_parts[i]);    
}

generated quantities {
  vector<lower=0,upper=1>[2] psiprior; // n.b. this creates divergences
  int<lower=0,upper=1> z[p];
  
  psiprior[1] = uniform_rng(.5, 1);  
  psiprior[2] = uniform_rng(0, psiprior[1]);
  
  
  for (i in 1:p) {
    vector[2] prob;
    prob = softmax(lp_parts[i]);
    z[i] = bernoulli_rng(prob[2]);
  }
}
