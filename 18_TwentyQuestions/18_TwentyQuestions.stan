//## Notes to Stan model #######################################################
// 1) There are two models in this script. The first one is not able to 
//    incorporate missing values as is required for exercise 6.3.2. The second
//    model is able to do that. Actually you can use the second model on both 
//    datasets - with or without NAs. 
// 2) Models with missing values can be tricky. It's important to know that
//    Stan treats variables either as know or unknown, therefore you have to
//    separate both parts first and then make it work somehow. Chapter on
//    Missing Data in Stan manual is useful.
//##############################################################################

// Twenty Questions
data { 
  int<lower=0> np;  // rows: persons
  int<lower=0> nq;  // columns: questions
  int k[np,nq];     // cells: answer - correct or not
}
parameters {
  real<lower=0,upper=1> p[np]; // rate of success for each person
  real<lower=0,upper=1> q[nq]; // rate of success for each question
} 
transformed parameters {
  real<lower=0,upper=1> theta[np, nq];  // rate of success given person and question
  // Probability Correct Is Product Of Question By Person Rates
  for (i in 1:np)
    for (j in 1:nq)
      theta[i,j] = p[i] * q[j];
}
model {
  // Priors For People and Questions
  target += beta_lpdf(p | 1, 1); // flat prior
  target += beta_lpdf(q | 1, 1); // flat prior
    
  // Correctness Of Each Answer Is Bernoulli Trial
  for (i in 1:np)
    for (j in 1:nq)
      target += bernoulli_lpmf(k[i, j] | theta[i,j]);
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
//   real partial_log_lik_lpmf(int[] seq, int start, int end, data int[] Y, real Intercept, data int[] J_1, data vector Z_1_1, vector r_1_1, data int[] J_2, data vector Z_2_1, vector r_2_1) {
//     real ptarget = 0;
//     int N = end - start + 1;
//     // initialize linear predictor term
//     vector[N] mu = Intercept + rep_vector(0.0, N);
//     for (n in 1:N) {
//       // add more terms to the linear predictor
//       int nn = n + start - 1;
//       mu[n] += r_1_1[J_1[nn]] * Z_1_1[nn] + r_2_1[J_2[nn]] * Z_2_1[nn];
//     }
//     ptarget += bernoulli_logit_lpmf(Y[start:end] | mu);
//     return ptarget;
//   }
// }
// data {
//   int<lower=1> N;  // total number of observations
//   int Y[N];  // response variable
//   int grainsize;  // grainsize for threading
//   // data for group-level effects of ID 1
//   int<lower=1> N_1;  // number of grouping levels
//   int<lower=1> M_1;  // number of coefficients per level
//   int<lower=1> J_1[N];  // grouping indicator per observation
//   // group-level predictor values
//   vector[N] Z_1_1;
//   // data for group-level effects of ID 2
//   int<lower=1> N_2;  // number of grouping levels
//   int<lower=1> M_2;  // number of coefficients per level
//   int<lower=1> J_2[N];  // grouping indicator per observation
//   // group-level predictor values
//   vector[N] Z_2_1;
//   int prior_only;  // should the likelihood be ignored?
// }
// transformed data {
//   int seq[N] = sequence(1, N);
// }
// parameters {
//   real Intercept;  // temporary intercept for centered predictors
//   vector<lower=0>[M_1] sd_1;  // group-level standard deviations
//   vector[N_1] z_1[M_1];  // standardized group-level effects
//   vector<lower=0>[M_2] sd_2;  // group-level standard deviations
//   vector[N_2] z_2[M_2];  // standardized group-level effects
// }
// transformed parameters {
//   vector[N_1] r_1_1;  // actual group-level effects
//   vector[N_2] r_2_1;  // actual group-level effects
//   r_1_1 = (sd_1[1] * (z_1[1]));
//   r_2_1 = (sd_2[1] * (z_2[1]));
// }
// model {
//   // likelihood including constants
//   if (!prior_only) {
//     target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, Intercept, J_1, Z_1_1, r_1_1, J_2, Z_2_1, r_2_1);
//   }
//   // priors including constants
//   target += normal_lpdf(Intercept | 0, 1);
//   target += normal_lpdf(sd_1 | 0, 0.1)
//     - 1 * normal_lccdf(0 | 0, 0.1);
//   target += std_normal_lpdf(z_1[1]);
//   target += normal_lpdf(sd_2 | 0, 0.1)
//     - 1 * normal_lccdf(0 | 0, 0.1);
//   target += std_normal_lpdf(z_2[1]);
// }
// generated quantities {
//   // actual population-level intercept
//   real b_Intercept = Intercept;
//   // additionally sample draws from priors
//   real prior_Intercept = normal_rng(0,1);
//   real prior_sd_1 = normal_rng(0,0.1);
//   real prior_sd_2 = normal_rng(0,0.1);
//   // use rejection sampling for truncated priors
//   while (prior_sd_1 < 0) {
//     prior_sd_1 = normal_rng(0,0.1);
//   }
//   while (prior_sd_2 < 0) {
//     prior_sd_2 = normal_rng(0,0.1);
//   }
// }