//
// This Stan model does something
//

// Pearson Correlation
data { 
  int<lower=0> ntrials;
  int<lower=0> nsubjs;
  int<lower=0> k[nsubjs]; // successes
  vector<lower=0>[nsubjs] x; // extraversion
  int<lower=0> sigmax;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[2] mu;
  real<lower=0> muprior;
  vector<lower=0>[2] sigma;
  real<lower=0> sigmaprior;
  vector[2] thetap[nsubjs];
  real<lower=-1,upper=1> r;
  real<lower=-1,upper=1> rprior;
} 

transformed parameters {
  vector<lower=0,upper=100>[nsubjs] theta[2];
  cov_matrix[2] T;
  
  // Reparametrization
  T[1, 1] = square(sigma[1]);
  T[1, 2] = r * sigma[1] * sigma[2];
  T[2, 1] = r * sigma[1] * sigma[2];
  T[2, 2] = square(sigma[2]);
  for (i in 1:nsubjs) {
    theta[1,i] = Phi(thetap[i,1]);
    theta[2,i] = 100 * Phi(thetap[i,2]);
  }
}

model {
  // Priors
  mu ~ normal(0, 1);
  muprior ~ normal(0, 1);
  sigma ~ normal(0, 3);
  sigmaprior ~ normal(0, 3);
  r ~ normal(0, 0.5);
  rprior ~ normal(0, 0.5);
  // Data
  thetap ~ multi_normal(mu, T);
  k ~ binomial(ntrials, theta[1]);
  x ~ normal(theta[2], sigmax);
}

