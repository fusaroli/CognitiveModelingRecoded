//
// This Stan model does something
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> n; // n of questions
  int<lower=1> p; // n of participants
  int<lower=0,upper=n> k[p];
}

parameters {
  real<lower=0,upper=1> phi;
  real<lower=0,upper=1> mubon;
  real<lower=0> mudiff;
  real<lower=40,upper=800> lambdabon;
  real<lower=4,upper=100> lambdamal;
  matrix<lower=0,upper=1>[p,2] theta;
} 

transformed parameters {
  vector[2] lp_parts[p];
  vector<lower=0>[2] alpha;
  vector<lower=0>[2] beta;
  real<lower=0,upper=1> mumal;
    
  // Additivity on Logit Scale
  mumal = inv_logit(logit(mubon) - mudiff);
  
  // Transformation to Group Mean and Precision
  alpha[1] = mubon * lambdabon;
  beta[1] = lambdabon * (1 - mubon);
  alpha[2] = mumal * lambdamal;
  beta[2] = lambdamal * (1 - mumal);
    
  // Data are Binomial with Rate Given by 
  // Each Person’s Group Assignment
  for (i in 1:p) {
    lp_parts[i,1] = log1m(phi) + binomial_lpmf(k[i] | n, theta[i,1]);
    lp_parts[i,2] = log(phi) + binomial_lpmf(k[i] | n, theta[i,2]);
  } 
}

// The model to be estimated. 
model {
  // Priors
  target += beta_lpdf(mubon | 1, 1);  // can be removed
  target += normal_lpdf(mudiff | 0, 1 / sqrt(.5))- normal_lccdf(0.0 | 0, 1);  // Constrained to be Positive
  // Relatively Uninformative Prior on Base Rate
  target += beta_lpdf(phi | 5, 5);
  
  for (i in 1:p)
    target += beta_lpdf(theta[i] | alpha, beta);
    
  for (i in 1:p)
    target += log_sum_exp(lp_parts[i]);   
}

generated quantities {
  int<lower=0,upper=1> z[p];
    
  for (i in 1:p) {
    vector[2] prob;
    prob = softmax(lp_parts[i]);
    // Each Person Belongs to One of Two Latent Groups
    z[i] = bernoulli_rng(prob[2]);
  }
}
