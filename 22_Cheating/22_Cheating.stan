//
// This Stan model does something
//

// The input data 
data { 
  int<lower=1> n;
  int<lower=1> p;
  int<lower=1,upper=n> k[p];
  int<lower=0,upper=1> truth[p];
}
// The parameters accepted by the model. 
parameters {
  real<lower=0,upper=1> phi;
  real<lower=0,upper=1> mubon;
  real<lower=0> mudiff;
  real<lower=5,upper=50> lambdabon;
  real<lower=5,upper=50> lambdache;
  vector<lower=0,upper=1>[p] theta;
} 

// The model to be estimated. 
transformed parameters {
  vector[2] lp_parts[p];
  vector<lower=0>[2] alpha;
  vector<lower=0>[2] beta;
  real<lower=0,upper=1> muche;
    
  // Additivity on Logit Scale
  muche = inv_logit(logit(mubon) + mudiff);
  
  // Transformation to Group Mean and Precision
  alpha[1] = mubon * lambdabon;
  beta[1] = lambdabon * (1 - mubon);
  alpha[2] = muche * lambdache;
  beta[2]  = lambdache * (1 - muche);
    
  // Data are Binomial with Rate Given by 
  // Each Person’s Group Assignment
  for (i in 1:p) {
    lp_parts[i,1] = log1m(phi) + beta_lpdf(theta[i] | alpha[1], beta[1]);
    lp_parts[i,2] = log(phi) + beta_lpdf(theta[i] | alpha[2], beta[2]);
  } 
}
model {
  // Priors
  target += beta_lpdf(mubon | 1, 1);  // can be removed
  target += normal_lpdf(mudiff | 0, 1 / sqrt(.5))- normal_lccdf(0.0 | 0, 1);
  // Relatively Uninformative Prior on Base Rate
  target += beta_lpdf(phi | 5, 5);
    
  for (i in 1:p)  
    target += log_sum_exp(lp_parts[i]);    
    
  target += binomial_lpmf(k | n, theta);
}
generated quantities {
  int<lower=0,upper=1> z[p];
  real pc;
  vector[p] pct;
  
  for (i in 1:p) {
    vector[2] prob;
    prob = softmax(lp_parts[i]);
    // Each Person Belongs to One of Two Latent Groups
    z[i] = bernoulli_rng(prob[2]);
    // Correct Count
    pct[i] = if_else(z[i] == truth[i], 1, 0);
  }
  pc = sum(pct);
}