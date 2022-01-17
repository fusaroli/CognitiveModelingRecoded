// This Stan model infers a difference in rate between two separate "experiments"
// 

// The input data is two integer numbers: n and k.
data {
  int<lower=1> n1; 
  int<lower=1> n2;
  int<lower=0> k1;
  int<lower=0> k2;
}

// The model accepts 2 parameters, two rates
parameters {
  real<lower=0, upper=1> theta1;
  real<lower=0, upper=1> theta2;
}

// Calculating the difference within the model
// Since it's purely determined by existing parameters, delta is defined as a transformed parameter
transformed parameters {
  real<lower=-1,upper=1> delta;
  delta = theta1 - theta2;
}

// The model to be estimated; prior and likelihood
model {
  // THe prior for both thetas is a uniform distribution between 0 and 1
  target += beta_lpdf(theta1 | 1, 1);
  target += beta_lpdf(theta2 | 1, 1);
  
  // The model consists of 2 binomial distributions
  target += binomial_lpmf(k1 | n1, theta1);
  target += binomial_lpmf(k2 | n2, theta2);
}

// generated with brms 2.16.1
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
//   real partial_log_lik_lpmf(int[] seq, int start, int end, data int[] Y, data int[] trials, data matrix Xc, vector b, real Intercept) {
//     real ptarget = 0;
//     int N = end - start + 1;
//     // initialize linear predictor term
//     vector[N] mu = Intercept + Xc[start:end] * b;
//     ptarget += binomial_lpmf(Y[start:end] | trials[start:end], mu);
//     return ptarget;
//   }
// }
// data {
//   int<lower=1> N;  // total number of observations
//   int Y[N];  // response variable
//   int trials[N];  // number of trials
//   int<lower=1> K;  // number of population-level effects
//   matrix[N, K] X;  // population-level design matrix
//   int grainsize;  // grainsize for threading
//   int prior_only;  // should the likelihood be ignored?
// }
// transformed data {
//   int Kc = K - 1;
//   matrix[N, Kc] Xc;  // centered version of X without an intercept
//   vector[Kc] means_X;  // column means of X before centering
//   int seq[N] = sequence(1, N);
//   for (i in 2:K) {
//     means_X[i - 1] = mean(X[, i]);
//     Xc[, i - 1] = X[, i] - means_X[i - 1];
//   }
// }
// parameters {
//   vector[Kc] b;  // population-level effects
//   real Intercept;  // temporary intercept for centered predictors
// }
// transformed parameters {
// }
// model {
//   // likelihood including constants
//   if (!prior_only) {
//     target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, trials, Xc, b, Intercept);
//   }
//   // priors including constants
//   target += beta_lpdf(Intercept | 1, 1);
// }
// generated quantities {
//   // actual population-level intercept
//   real b_Intercept = Intercept - dot_product(means_X, b);
//   // additionally sample draws from priors
//   real prior_Intercept = beta_rng(1,1);
