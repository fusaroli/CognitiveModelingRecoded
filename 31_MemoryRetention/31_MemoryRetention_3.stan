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
  
  real<lower = 0, upper = 1> mualpha;
  real<lower = 0, upper = 1> mubeta;
  
  real<lower = 0> sigmaalpha;
  real<lower = 0> sigmabeta;
}

transformed parameters {
  
  matrix<lower=0,upper=1>[ns,nt] theta;
  
  // Retention Rate At Each Lag For Each Subject Decays Exponentially
  for (i in 1:ns) {
    for (j in 1:nt) {
      theta[i,j] = fmin(1, exp(-alpha[i] * t[j]) + beta[i]);
    }
  }
  
}

// The model to be estimated. 
model {
  // Priors
  // Priors For Group Distributions
  mualpha ~ beta(1, 1);  // can be removed
  mubeta ~ beta(1, 1);  // can be removed
  sigmaalpha ~ normal(0,1)T[0,];
  sigmabeta ~ normal(0,1)T[0,];
  
  
  for (i in 1:ns)  {
    alpha[i] ~ normal(mualpha, sigmaalpha)T[0,1];
    beta[i] ~ normal(mubeta, sigmabeta)T[0,1];
  }
  
  
  // Observed Data
  for (i in 1:(ns - 1))
    for (j in 1:(nt - 1))
      k[i,j] ~ binomial(n, theta[i,j]);
}

generated quantities {
  real<lower=0,upper=1> alphaprior;
  real<lower=0,upper=1> betaprior;
  vector<lower=0,upper=1>[nt] thetaprior;
  int<lower=0,upper=n> postpredk[ns,nt];
  int<lower=0,upper=n> priorpredk[nt];
  
  alphaprior = beta_rng(1, 1);  // can be removed
  betaprior = beta_rng(1, 1);  // can be removed
  
  
  for (w in 1:nt) {
    thetaprior[w] = fmin(1, exp(-alphaprior * t[w]) + betaprior);
  }
  // Predicted Data
  for (i in 1:ns)
    for (j in 1:nt)
      postpredk[i,j] = binomial_rng(n , theta[i,j]);
  
  for (w in 1:nt)
      priorpredk[w] = binomial_rng(n , thetaprior[w]);
}
