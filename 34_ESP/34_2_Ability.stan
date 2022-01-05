//
// This Stan model does something
//

// Pearson Correlation
data { 
  int<lower=0> n;
  vector[2] x[n];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector<lower=0, upper=1>[2] mu;
  vector<lower=0>[2] sigma;
  real<lower=-1, upper=1> r;
}

transformed parameters {
  cov_matrix[2] T;
  // Reparameterization
  T[1,1] = square(sigma[1]);
  T[1,2] = r * sigma[1] * sigma[2];
  T[2,1] = r * sigma[1] * sigma[2];
  T[2,2] = square(sigma[2]);
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // Priors
  mu ~ beta(1,1);
  sigma ~ normal(0, .5);
  r ~ normal(0,.5);
  // Data
  x ~ multi_normal(mu, T);
}

generated quantities {
  real<lower=0, upper=1> muprior;
  real<lower=0> sigmaprior;
  real<lower=-1, upper=1> rprior;
  muprior = beta_rng(1,1);
  sigmaprior = normal_rng(0, .5);
  rprior = normal_rng(0,.5);
}
