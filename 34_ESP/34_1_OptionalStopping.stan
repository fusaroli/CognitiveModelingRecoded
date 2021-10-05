//
// This Stan model does something
//

// Pearson Correlation
data { 
  int<lower=0> n;
  vector[2] x[n];
}

parameters {
  vector[2] mu;
  real muprior;
  vector<lower=0>[2] sigma;
  real sigmaprior;
  real<lower=-1,upper=1> r;
  real<lower=-1,upper=1> rprior;
} 

transformed parameters {
  cov_matrix[2] T;
  cov_matrix[2] Tprior;
  
  // Reparameterization
  T[1,1] = square(sigma[1]);
  T[1,2] = r * sigma[1] * sigma[2];
  T[2,1] = r * sigma[1] * sigma[2];
  T[2,2] = square(sigma[2]);
  
  Tprior[1,1] = square(sigmaprior);
  Tprior[1,2] = rprior * sigmaprior * sigmaprior;
  Tprior[2,1] = rprior * sigmaprior * sigmaprior;
  Tprior[2,2] = square(sigmaprior);
}

model {
  // Priors
  mu ~ normal(0, 100);
  muprior ~ normal(0, 100);
  sigma ~ normal(0, 10);
  sigmaprior ~ normal(0, 10);
  
  // Data
  x ~ multi_normal(mu, T);
}

