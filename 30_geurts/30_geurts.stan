//
// This Stan model does something
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> nsa; // number of adhd participants
  int<lower=1> nsc; // number of control participants
  int<lower=0> nc[nsc]; // number of cards for controls
  int<lower=0> na[nsa]; // number of cards for adhd
  int<lower=0> kc[nsc]; // number of successes for controls
  int<lower=0> ka[nsa]; // number of successes for adhd
}
// Geurts


// The parameters accepted by the model. 
parameters {
  
  real delta;
  real deltaprior;
  
  real mu;
  real muprior;
  real<lower=0> sigma;
  real<lower=0> sigmaprior;
  
  vector[nsc] phi_c;                     
  vector[nsa] phi_a;
  
}

transformed parameters{
  real alpha;
  real alphaprior;
  
  vector<lower=0,upper=1>[nsc] theta_c; // 
  vector<lower=0,upper=1>[nsa] theta_a; // 
  
  alphaprior = deltaprior * sigmaprior;       // transform standardized difference in non standardized difference
  alpha = delta * sigma;       // transform standardized difference in non standardized difference
  
  // Probit transformation
  for (i in 1:nsc) 
    theta_c[i] = Phi(phi_c[i]); // from zscores to probability
  
  for (j in 1:nsa) 
    theta_a[j] = Phi(phi_a[j]); // from zscores to probability
}

// The model to be estimated. 
model {
  mu ~ normal(0,1);
  muprior ~ normal(0,1);
  sigma ~ normal(0,1);
  sigmaprior ~ normal(0,1);
  
  delta ~ normal(0,1)T[0,];
  deltaprior ~ normal(0,1)T[0,];
  
  phi_c ~ normal(mu + alpha/2, sigma);
  phi_a ~ normal(mu - alpha/2, sigma);
  
  // Data
  kc ~ binomial(nc, theta_c); // both priming model
  ka ~ binomial(na, theta_a); //  no priming model
}

