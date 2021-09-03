// This Stan program defines a model
// inferring a rate (theta) from a number of trials (n) and successes (k)

// The input data is two integer numbers: n and k.
data {
  int<lower=1> n; 
  int<lower=0> k;
}

// The parameters accepted by the model. Our model
// accepts only theta, the rate.
parameters {
  real<lower=0, upper=1> theta;
}

// The model to be estimated; prior and likelihood

model {
  // THe prior for theta is a uniform distribution between 0 and 1
  theta ~ beta(1, 1);
  // theta ~ uniform(0, 1) ## equivalent to beta?
  
  // The model consists in a binomial distribution with a rate theta, 
  // and a number of trials n generating k successes
  k ~ binomial(n, theta);
  
}


// // From brms (laughing at my code)
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
//   real partial_log_lik_lpmf(int[] seq, int start, int end, data int[] Y, data int[] trials, real Intercept) {
//     real ptarget = 0;
//     int N = end - start + 1;
//     // initialize linear predictor term
//     vector[N] mu = Intercept + rep_vector(0.0, N);
//     ptarget += binomial_lpmf(Y[start:end] | trials[start:end], mu);
//     return ptarget;
//   }
// }
// data {
//   int<lower=1> N;  // total number of observations
//   int Y[N];  // response variable
//   int trials[N];  // number of trials
//   int grainsize;  // grainsize for threading
//   int prior_only;  // should the likelihood be ignored?
// }
// transformed data {
//   int seq[N] = sequence(1, N);
// }
// parameters {
//   real Intercept;  // temporary intercept for centered predictors
// }
// transformed parameters {
// }
// model {
//   // likelihood including constants
//   if (!prior_only) {
//     target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, trials, Intercept);
//   }
//   // priors including constants
//   target += beta_lpdf(Intercept | 1, 1);
// }
// generated quantities {
//   // actual population-level intercept
//   real b_Intercept = Intercept;
// }
