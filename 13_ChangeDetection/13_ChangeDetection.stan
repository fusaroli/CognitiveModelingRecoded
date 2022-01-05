//## Notes to Stan model #######################################################
// The model is not very effective.  
// 1) Don't change seed or lower iterations. This model converges slowly so if
//    you change the values, you'll need to increment iterations significantly  
// 2) Code is quite dissimilar to original WinBUGS model - using conditionals 
//    instead of step function. This will happen in further models more often.
//    There is a difference in what functions are efficient in BUGS and Stan.
//##############################################################################

// Change Detection
data { 
  int n;
  vector[n] t;
  vector[n] c;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[2] mu;
  real<lower=0> sigma;
  real<lower=0,upper=n> tau;
} 

model { 
  // Group Means
  mu ~ normal(0, 10);
  // Standard deviation
  sigma ~ normal(0, 10);
    
  // Which Side is Time of Change Point?
  // Data Come From A Gaussian
  for (i in 1:n) {
    if ((t[i] - tau) < 0.0)
      c[i] ~ normal(mu[1], sigma);
    else 
      c[i] ~ normal(mu[2], sigma);
  }
}

generated quantities{
  vector[2] muprior;
  real<lower=0> sigmaprior;
  muprior[1] = normal_rng(0, 10);
  muprior[2] = normal_rng(0, 10);
  sigmaprior = normal_rng(0, 10);
  
}

