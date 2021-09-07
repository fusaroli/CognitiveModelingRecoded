// This Stan model does something

// Repeated Measures of IQ
data { 
  int<lower=1> n;
  int<lower=1> m;
  matrix[n, m] x;
}
parameters {
  vector<lower=0,upper=300>[n] mu;
  real<lower=0,upper=300> muprior;
  real<lower=0,upper=100> sigma;
  real<lower=0,upper=100> sigmaprior;
} 

model {
  // Data Come From Gaussians With Different Means But Common Standard Deviation
  mu ~ normal(100, 30);
  muprior ~ normal(100, 30);
  sigma ~ normal(0, 15);
  sigmaprior ~ normal(0, 15);
  for (i in 1:n)
    for (j in 1:m)  
      x[i,j] ~ normal(mu[i], sigma);
}

generated quantities {
  real priorpredk;
  vector[n] postpredk;
  
  // Prior Predictive
  priorpredk = normal_rng(muprior, sigmaprior);
  
  // Posterior Predictive
  for (j in 1:n)  
    postpredk[j] = normal_rng(mu[j], sigmaprior);
  
}


