// The Seven Scientists
data { 
  int<lower=1> n;
  vector[n] x;
}
parameters {
  real mu;
  real muprior;
  vector<lower=0>[n] lambda;
  //  vector<lower=0>[n] lambdaprior;
} 
transformed parameters {
  vector<lower=0>[n] sigma;
  // vector<lower=0>[n] sigmaprior;
  
  for (i in 1:n)
    sigma[i] = inv_sqrt(lambda[i]);
    
  // for (j in 1:n)
  //    sigmaprior[j] = inv_sqrt(lambdaprior[j]);
}
model {
  // Priors
  mu ~ normal(0, sqrt(1000));
  muprior ~ normal(0, sqrt(1000));
  lambda ~ gamma(.001, .001);
  // lambdaprior ~ gamma(.001, .001);
  
  // Data Come From Gaussians With Common Mean But Different Precisions
  x ~ normal(mu, sigma);
}