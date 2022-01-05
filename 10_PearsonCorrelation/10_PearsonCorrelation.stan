//
// This Stan program does something
//

data { 
  int<lower=0> n;
  vector[2] x[n];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[2] mu;
  vector<lower=0>[2] sigma;
  real<lower=-1, upper=1> r;
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
  mu ~ normal(0, 10);
  sigma ~ normal(0, 10);
  r ~ normal(0, 0.5);
  
  // Data
  x ~ multi_normal(mu, T);
}

generated quantities {
  real muprior;
  real<lower=0> sigmaprior;
  real<lower=-1, upper=1> rprior;
  cov_matrix[2] Tprior;
  
  muprior = normal_rng(0, 10);
  sigmaprior = normal_rng(0, 10);
  rprior = normal_rng(0, 0.5);
  
  // Reparameterization
  Tprior[1,1] = square(sigmaprior);
  Tprior[1,2] = r * sigmaprior * sigmaprior;
  Tprior[2,1] = r * sigmaprior * sigmaprior;
  Tprior[2,2] = square(sigmaprior);

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
//   real partial_log_lik_lpmf(int[] seq, int start, int end, data vector Y_x1, real Intercept_x1, real sigma_x1, data vector Y_x2, real Intercept_x2, real sigma_x2, data int nresp, data vector[] Y, matrix Lrescor) {
//     real ptarget = 0;
//     int N = end - start + 1;
//     int N_x1 = end - start + 1;
//     int N_x2 = end - start + 1;
//     // initialize linear predictor term
//     vector[N_x1] mu_x1 = Intercept_x1 + rep_vector(0.0, N_x1);
//     // initialize linear predictor term
//     vector[N_x2] mu_x2 = Intercept_x2 + rep_vector(0.0, N_x2);
//     // multivariate predictor array
//     vector[nresp] Mu[N];
//     vector[nresp] sigma = transpose([sigma_x1, sigma_x2]);
//     // cholesky factor of residual covariance matrix
//     matrix[nresp, nresp] LSigma = diag_pre_multiply(sigma, Lrescor);
//     // combine univariate parameters
//     for (n in 1:N) {
//       Mu[n] = transpose([mu_x1[n], mu_x2[n]]);
//     }
//     ptarget += multi_normal_cholesky_lpdf(Y[start:end] | Mu, LSigma);
//     return ptarget;
//   }
// }
// data {
//   int<lower=1> N;  // total number of observations
//   int<lower=1> N_x1;  // number of observations
//   vector[N_x1] Y_x1;  // response variable
//   int<lower=1> N_x2;  // number of observations
//   vector[N_x2] Y_x2;  // response variable
//   int<lower=1> nresp;  // number of responses
//   int nrescor;  // number of residual correlations
//   int grainsize;  // grainsize for threading
//   int prior_only;  // should the likelihood be ignored?
// }
// transformed data {
//   vector[nresp] Y[N];  // response array
//   int seq[N] = sequence(1, N);
//   for (n in 1:N) {
//     Y[n] = transpose([Y_x1[n], Y_x2[n]]);
//   }
// }
// parameters {
//   real Intercept_x1;  // temporary intercept for centered predictors
//   real<lower=0> sigma_x1;  // dispersion parameter
//   real Intercept_x2;  // temporary intercept for centered predictors
//   real<lower=0> sigma_x2;  // dispersion parameter
//   cholesky_factor_corr[nresp] Lrescor;  // parameters for multivariate linear models
// }
// transformed parameters {
// }
// model {
//   // likelihood including constants
//   if (!prior_only) {
//     target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y_x1, Intercept_x1, sigma_x1, Y_x2, Intercept_x2, sigma_x2, nresp, Y, Lrescor);
//   }
//   // priors including constants
//   target += normal_lpdf(Intercept_x1 | 0, 10);
//   target += normal_lpdf(sigma_x1 | 0, 10)
//     - 1 * normal_lccdf(0 | 0, 10);
//   target += normal_lpdf(Intercept_x2 | 0, 10);
//   target += normal_lpdf(sigma_x2 | 0, 10)
//     - 1 * normal_lccdf(0 | 0, 10);
//   target += lkj_corr_cholesky_lpdf(Lrescor | 1);
// }
// generated quantities {
//   // actual population-level intercept
//   real b_x1_Intercept = Intercept_x1;
//   // actual population-level intercept
//   real b_x2_Intercept = Intercept_x2;
//   // residual correlations
//   corr_matrix[nresp] Rescor = multiply_lower_tri_self_transpose(Lrescor);
//   vector<lower=-1,upper=1>[nrescor] rescor;
//   // additionally sample draws from priors
//   real prior_Intercept_x1 = normal_rng(0,10);
//   real prior_sigma_x1 = normal_rng(0,10);
//   real prior_Intercept_x2 = normal_rng(0,10);
//   real prior_sigma_x2 = normal_rng(0,10);
//   real prior_rescor = lkj_corr_rng(nresp,1)[1, 2];
//   // extract upper diagonal of correlation matrix
//   for (k in 1:nresp) {
//     for (j in 1:(k - 1)) {
//       rescor[choose(k - 1, 2) + j] = Rescor[j, k];
//     }
//   }
//   // use rejection sampling for truncated priors
//   while (prior_sigma_x1 < 0) {
//     prior_sigma_x1 = normal_rng(0,10);
//   }
//   while (prior_sigma_x2 < 0) {
//     prior_sigma_x2 = normal_rng(0,10);
//   }
// }
