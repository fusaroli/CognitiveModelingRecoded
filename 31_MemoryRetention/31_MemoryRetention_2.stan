//
// This Stan model does something

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> ns; // n of participants
  int<lower=0> nt; // n of temporal points
  int<lower=0> n; // n of stimuli to remember
  int<lower=0> k[ns - 1,nt - 1]; // successes
  int<lower=0> t[nt]; // time from training
}

// The parameters accepted by the model.
parameters {
  vector<lower=0,upper=1>[ns] alpha;
  vector<lower=0,upper=1>[ns] beta;
  real<lower=0,upper=1> alphaprior;
  real<lower=0,upper=1> betaprior;
}

transformed parameters {
  
  matrix<lower=0,upper=1>[ns,nt] theta;
  vector<lower=0,upper=1>[nt] thetaprior;
  
  // Retention Rate At Each Lag For Each Subject Decays Exponentially
  for (i in 1:ns) {
    for (j in 1:nt) {
      theta[i,j] = fmin(1, exp(-alpha[i] * t[j]) + beta[i]);
    }
  }
  for (w in 1:nt) {
    thetaprior[w] = fmin(1, exp(-alphaprior * t[w]) + betaprior);
  }
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // Priors
  alpha ~ beta(1, 1);  // can be removed
  beta ~ beta(1, 1);  // can be removed
  alphaprior ~ beta(1, 1);  // can be removed
  betaprior ~ beta(1, 1);  // can be removed
  
  // Observed Data
  for (i in 1:(ns - 1))
    for (j in 1:(nt - 1))
      k[i,j] ~ binomial(n, theta[i,j]);
}

generated quantities {
  int<lower=0,upper=n> postpredk[ns,nt];
  int<lower=0,upper=n> priorpredk[nt];
  
  // Predicted Data
  for (i in 1:ns)
    for (j in 1:nt)
      postpredk[i,j] = binomial_rng(n , theta[i,j]);
  
  for (w in 1:nt)
      priorpredk[w] = binomial_rng(n , thetaprior[w]);
}
