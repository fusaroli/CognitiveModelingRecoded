//
// This Stan model does something
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> n1;
  int<lower=0> n2;
  int<lower=0, upper=n1> s1;
  int<lower=0, upper=n2> s2;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0, upper=1> theta2;
  real<lower=0, upper=theta2> theta1;
  real<lower=0, upper=1> thetaprior2;
  real<lower=0, upper=thetaprior2> thetaprior1;
}

transformed parameters {
  real<lower=-1, upper=1> deltaprior;
  real<lower=-1, upper=1> delta;
  deltaprior = thetaprior2 - thetaprior1;
  delta = theta2 - theta1;
  
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  thetaprior1 ~ beta(1,1);
  thetaprior2 ~ beta(1,1);
  theta1 ~ beta(1,1);
  theta2 ~ beta(1,1);
  
  s1 ~ binomial(n1, theta1);
  s2 ~ binomial(n2, theta2);
}

generated quantities {
  real PredictedOutcomePrior1;
  real PredictedOutcomePrior2;
  real PredictedOutcomePosterior1;
  real PredictedOutcomePosterior2;
  
  // Prior Predictive
  PredictedOutcomePrior1 = binomial_rng(n1, thetaprior1);
  PredictedOutcomePrior2 = binomial_rng(n2, thetaprior2);
  // Posterior Predictive
  PredictedOutcomePosterior1 = binomial_rng(n1, theta1);
  PredictedOutcomePosterior2 = binomial_rng(n2, theta2);
}
