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
  real<lower=0> sigma1;
  real<lower=0> sigma2;
  
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  target += normal_lpdf(mu1 | 0,1);
  target += normal_lpdf(mu2 | 0,1);
  
  // sigma ~ cauchy(0,1); // obviously crazy
  // sigmaprior ~ cauchy(0,1); // obviously crazy
  
  target += normal_lpdf(sigma1 | 0,1)- normal_lccdf(0.0 | 0, 1); // better
  target += normal_lpdf(sigma2 | 0,1)- normal_lccdf(0.0 | 0, 1); // better
  
  // Data
  target += normal_lpdf(x | mu1, sigma1);
  target += normal_lpdf(y | mu2, sigma2);
}

generated quantities{
  real muprior;
  real<lower=0> sigmaprior;
  real PredictedOutcomePrior;
  real PredictedOutcomePosterior;
  real alphaprior;
  real alpha;
  
  muprior = normal_rng(0,1);
  sigmaprior = normal_rng(0,1); // better
  
  alphaprior = normal_rng(0, 1)-normal_rng(0, 1);
  alpha = mu1 - mu2;
  PredictedOutcomePrior = normal_rng(muprior, sigmaprior)-normal_rng(muprior, sigmaprior);
  PredictedOutcomePosterior = normal_rng(mu1, sigma1) - normal_rng(mu2, sigma2);
}







