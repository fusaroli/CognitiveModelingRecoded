//
// This Stan model does something

// The input data is a vector 'x' of length 'n'.
data {
  int<lower=1> n;
  vector[n] x;
}

// The parameters accepted by the model. Our model
// accepts a common 'mu' and 7 'sigma's.
parameters {
  real mu;
  vector<lower=0>[n] sigma;
  
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // Priors
  mu ~ normal(0, 10);
  sigma ~ normal(0, 10);
  // Data Come From Gaussians With Common Mean But Different Precisions
  x ~ normal(mu, sigma);
}

generated quantities{
  real muprior;
  real<lower=0> sigmaprior;
  real preds_x;
  
  muprior = normal_rng(0, 10);
  sigmaprior = normal_rng(0, 10);
  
  preds_x = normal_rng(muprior, sigmaprior);

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
//   real partial_log_lik_lpmf(int[] seq, int start, int end, data vector Y, real Intercept, data matrix X_sigma, vector b_sigma) {
//     real ptarget = 0;
//     int N = end - start + 1;
//     // initialize linear predictor term
//     vector[N] mu = Intercept + rep_vector(0.0, N);
//     // initialize linear predictor term
//     vector[N] sigma = X_sigma[start:end] * b_sigma;
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
//   int<lower=1> K_sigma;  // number of population-level effects
//   matrix[N, K_sigma] X_sigma;  // population-level design matrix
//   int grainsize;  // grainsize for threading
//   int prior_only;  // should the likelihood be ignored?
// }
// transformed data {
//   int seq[N] = sequence(1, N);
// }
// parameters {
//   real Intercept;  // temporary intercept for centered predictors
//   vector[K_sigma] b_sigma;  // population-level effects
// }
// transformed parameters {
// }
// model {
//   // likelihood including constants
//   if (!prior_only) {
//     target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, Intercept, X_sigma, b_sigma);
//   }
//   // priors including constants
//   target += normal_lpdf(Intercept | 0, 10);
//   target += normal_lpdf(b_sigma | 0, 10);
// }
// generated quantities {
//   // actual population-level intercept
//   real b_Intercept = Intercept;
//   // additionally sample draws from priors
//   real prior_Intercept = normal_rng(0,10);
//   real prior_b_sigma = normal_rng(0,10);
// }
