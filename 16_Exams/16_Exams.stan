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
  real<lower=.5,upper=1> phi; 
  real<lower=.5,upper=1> phiprior;
}

transformed parameters {
  vector[2] lp_parts[p];
  // Data Follow Binomial With Rate Given By Each Person's Group Assignment
  for (i in 1:p) {
    lp_parts[i,1] = log(.5) + binomial_lcdf(k[i] | n, phi);
    lp_parts[i,2] = log(.5) + binomial_lcdf(k[i] | n, psi); 
  }
}

model {
 phiprior ~ uniform(.5,1);
 
  for (i in 1:p)
    target += log_sum_exp(lp_parts[i]);  
}
generated quantities {
  int<lower = 0,upper = 1> z[p];
  
  for (i in 1:p) {
    vector[2] prob;
    prob = softmax(lp_parts[i]);
    z[i] = bernoulli_rng(prob[1]);
  }
}