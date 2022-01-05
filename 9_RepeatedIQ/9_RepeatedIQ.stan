// This Stan model does something

// Repeated Measures of IQ
data { 
  int<lower=1> n;
  int<lower=1> m;
  matrix[n, m] x;
}
parameters {
  vector<lower=0,upper=300>[n] mu;
  real<lower=0,upper=100> sigma;
} 

model {
  // Data Come From Gaussians With Different Means But Common Standard Deviation
  mu ~ normal(100, 15);
  sigma ~ normal(0, 15);
  for (i in 1:n)
    for (j in 1:m)  
      x[i,j] ~ normal(mu[i], sigma);
}

generated quantities {
  real<lower=0,upper=300> muprior;
  real<lower=0,upper=100> sigmaprior;
  real priorpredk;
  vector[n] postpredk;
  
  muprior = normal_rng(100, 15);
  sigmaprior = normal_rng(0, 15);
  
  // Prior Predictive
  priorpredk = normal_rng(muprior, sigmaprior);
  
  // Posterior Predictive
  for (j in 1:n)  
    postpredk[j] = normal_rng(mu[j], sigmaprior);
  
}

// // generated with brms 2.16.1
// functions {
//   /* integer sequence of values
//    * Args: 
//    *   start: starting integer
//    *   end: ending integer
//    * Returns: 
//    *   an integer sequence from start to end
//    */ 
//   int[] sequence(int start, int end) { 
//     int seq[end - start + 1];
//     for (n in 1:num_elements(seq)) {
//       seq[n] = n + start - 1;
//     }
//     return seq; 
//   } 
//   // compute partial sums of the log-likelihood
//   real partial_log_lik_lpmf(int[] seq, int start, int end, data vector Y, data matrix X, vector b, real Intercept_sigma) {
//     real ptarget = 0;
//     int N = end - start + 1;
//     // initialize linear predictor term
//     vector[N] mu = X[start:end] * b;
//     // initialize linear predictor term
//     vector[N] sigma = Intercept_sigma + rep_vector(0.0, N);
//     for (n in 1:N) {
//       // apply the inverse link function
//       sigma[n] = exp(sigma[n]);
//     }
//     ptarget += normal_lpdf(Y[start:end] | mu, sigma);
//     return ptarget;
//   }
// }
// data {
//   int<lower=1> N;  // total number of observations
//   vector[N] Y;  // response variable
//   int<lower=1> K;  // number of population-level effects
//   matrix[N, K] X;  // population-level design matrix
//   int grainsize;  // grainsize for threading
//   int prior_only;  // should the likelihood be ignored?
// }
// 
// transformed data {
//   int seq[N] = sequence(1, N);
// }
// 
// parameters {
//   vector[K] b;  // population-level effects
//   real Intercept_sigma;  // temporary intercept for centered predictors
// }
// 
// transformed parameters {
// }
// 
// model {
//   // likelihood including constants
//   if (!prior_only) {
//     target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, X, b, Intercept_sigma);
//   }
//   // priors including constants
//   target += normal_lpdf(b | 100, 15);
//   target += normal_lpdf(Intercept_sigma | 0, 15);
// }
// 
// generated quantities {
//   // actual population-level intercept
//   real b_sigma_Intercept = Intercept_sigma;
//   // additionally sample draws from priors
//   real prior_b = normal_rng(100,15);
//   real prior_Intercept_sigma = normal_rng(0,15);
// }
