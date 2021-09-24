//
// This Stan model does something

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> n1;
  int<lower=0> n2;
  vector[n1] x;
  vector[n2] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu1;
  real mu2;
  real muprior;
  real<lower=0> sigma1;
  real<lower=0> sigma2;
  real<lower=0> sigmaprior;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  mu1 ~ normal(0,1);
  mu2 ~ normal(0,1);
  muprior ~ normal(0,1);
  
  // sigma ~ cauchy(0,1); // obviously crazy
  // sigmaprior ~ cauchy(0,1); // obviously crazy
  
  sigma1 ~ normal(0,1); // better
  sigma2 ~ normal(0,1); // better
  sigmaprior ~ normal(0,1); // better
  
  // Data
  x ~ normal(mu1, sigma1);
  y ~ normal(mu2, sigma2);
}

generated quantities{
  real PredictedOutcomePrior;
  real PredictedOutcomePosterior;
  real alphaprior;
  real alpha;
  alphaprior = normal_rng(0, 1)-normal_rng(0, 1);
  alpha = mu1 - mu2;
  PredictedOutcomePrior = normal_rng(muprior, sigmaprior)-normal_rng(muprior, sigmaprior);
  PredictedOutcomePosterior = normal_rng(mu1, sigma1) - normal_rng(mu2, sigma2);
}







