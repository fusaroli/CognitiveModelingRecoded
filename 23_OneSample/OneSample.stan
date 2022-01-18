//
// This Stan model does something

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> n;
  vector[n] x;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu;
  real<lower=0> sigma;
  
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  target += normal_lpdf(mu | 0, 1);
  
  // sigma ~ cauchy(0,1); // obviously crazy
  // sigmaprior ~ cauchy(0,1); // obviously crazy
  
  target += normal_lpdf(sigma | 0, 1) - normal_lccdf(0.0 | 0, 1); // better
  target += normal_lpdf(x | mu, sigma);
}

generated quantities{
  real muprior;
  real<lower=0> sigmaprior;
  real PredictedOutcomePrior;
  real PredictedOutcomePosterior;
  
  
  muprior = normal_rng(0,1);
  sigmaprior = normal_rng(0,1); // better
  PredictedOutcomePrior = normal_rng(muprior, sigmaprior);
  PredictedOutcomePosterior = normal_rng(muprior, sigmaprior);
}







