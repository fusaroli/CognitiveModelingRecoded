//
// This Stan program build a correlation model including measurement error
//

// The data consist in 
data { 
  int<lower=0> n;
  vector[2] x[n];
  vector[2] sigmaerror;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[2] mu;
  vector<lower=0>[2] sigma;
  real<lower=-1, upper=1> r;
  vector[2] y[n];
}

transformed parameters {
  cov_matrix[2] T;
  
  // Reparameterization
  T[1,1] = square(sigma[1]);
  T[1,2] = r * sigma[1] * sigma[2];
  T[2,1] = r * sigma[1] * sigma[2];
  T[2,2] = square(sigma[2]);

}


model {
  // Priors
  target += normal_lpdf(mu | 0, 10);
  target += normal_lpdf(sigma | 0, 10);
  target += normal_lpdf(r | 0, 0.5);
  
  // Data
  target += multi_normal_lpdf(x | mu, T);
  for (i in 1:n)
    target += normal_lpdf(x[i] | y[i], sigmaerror);
}

generated quantities{
  // Defining the prior parameters
  real muprior;
  real<lower=0> sigmaprior;
  real<lower=-1, upper=1> rprior;
  cov_matrix[2] Tprior;
  
  // Generating the prior parameters
  muprior = normal_rng(0, 10);
  sigmaprior = normal_rng(0, 10);
  rprior = normal_rng(0, .5);
  
  // Reparameterization
  Tprior[1,1] = square(sigmaprior);
  Tprior[1,2] = rprior * sigmaprior * sigmaprior;
  Tprior[2,1] = rprior * sigmaprior * sigmaprior;
  Tprior[2,2] = square(sigmaprior);
  
}

