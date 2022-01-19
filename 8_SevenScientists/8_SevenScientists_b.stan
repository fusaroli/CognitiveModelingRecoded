// The Seven Scientists
data { 
  int<lower=1> n;
  vector[n] x;
}
parameters {
  real mu;
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
  target += normal_lpdf(mu | 0, sqrt(1000));
  target += gamma_lpdf(lambda | .001, .001);
  
  // Data Come From Gaussians With Common Mean But Different Precisions
  target += normal_lpdf(x | mu, sigma);
}

generated quantities{
  real muprior;
  real lambdaprior;
  real preds_x;

  muprior = normal_rng(0, sqrt(1000));
  lambdaprior = gamma_rng(.001, .001);
  
  preds_x = normal_rng(muprior, inv_sqrt(lambdaprior));

  
}