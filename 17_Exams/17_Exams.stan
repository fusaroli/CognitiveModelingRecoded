//
// This Stan model does something

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> p; // number of people
  int<lower=0> k[p]; // number of correct answers
  int<lower=1> n; // number of questions
}

transformed data {
  real psi;
  // First Group Guesses
  psi = .5;
}

parameters {
  // Second Group Has Some Unknown Greater Rate Of Success
  vector<lower=0,upper=1>[p] phi; 
  real<lower=.5,upper=1> phiprior;
  real<lower=.5,upper=1> mu;  // Second Group Mean
  real<lower=.5,upper=1> muprior;  // Second Group Mean
  real<lower=0> sigma;
  real<lower=0> sigmaprior;
  real<lower=0,upper=1> predphi;
}

transformed parameters {
  vector[2] lp_parts[p];
  
  // Data Follow Binomial With Rate Given By Each Person's Group Assignment
  // Each Person Belongs To One Of Two Latent Groups
  for (i in 1:p) {
    lp_parts[i, 1] = log(.5) + binomial_lcdf(k[i] | n, phi[i]);
    lp_parts[i, 2] = log(.5) + binomial_lcdf(k[i] | n, psi); 
  }
}

model {
 phiprior ~ uniform(.5,1);
 // Second Group Precision
 sigma ~ normal(0, 10); // new
 sigmaprior ~ normal(0, 10); // new
 // Posterior Predictive For Second Group
 predphi ~ normal(mu, sigma)T[0,1]; // new
 
 // Second Group Drawn From A Censored Gaussian Distribution
 for (i in 1:p)                       // new
    phi[i] ~ normal(mu, sigma)T[0,1]; // new
 
 for (i in 1:p)
    target += log_sum_exp(lp_parts[i]);  
}

generated quantities {
  int<lower = 0,upper = 1> z[p];
  vector<lower=0,upper=1>[p] theta; // new
  
  for (i in 1:p) {
    vector[2] prob;
    prob = softmax(lp_parts[i]);
    z[i] = bernoulli_rng(prob[1]);
    theta[i] = (z[i] == 0) * psi + (z[i] == 1) * phi[i]; // new
  }
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
//   real partial_log_lik_lpmf(int[] seq, int start, int end, data int[] Y, data int[] trials, real Intercept_mu1, real Intercept_mu2, real theta1, real theta2, data int[] J_1, data vector Z_1_mu1_1, vector r_1_mu1_1, data int[] J_2, data vector Z_2_mu2_1, vector r_2_mu2_1) {
//     real ptarget = 0;
//     int N = end - start + 1;
//     // initialize linear predictor term
//     vector[N] mu1 = Intercept_mu1 + rep_vector(0.0, N);
//     // initialize linear predictor term
//     vector[N] mu2 = Intercept_mu2 + rep_vector(0.0, N);
//     for (n in 1:N) {
//       // add more terms to the linear predictor
//       int nn = n + start - 1;
//       mu1[n] += r_1_mu1_1[J_1[nn]] * Z_1_mu1_1[nn];
//     }
//     for (n in 1:N) {
//       // add more terms to the linear predictor
//       int nn = n + start - 1;
//       mu2[n] += r_2_mu2_1[J_2[nn]] * Z_2_mu2_1[nn];
//     }
//     // likelihood of the mixture model
//     for (n in 1:N) {
//       int nn = n + start - 1;
//       real ps[2];
//       ps[1] = log(theta1) + binomial_logit_lpmf(Y[nn] | trials[nn], mu1[n]);
//       ps[2] = log(theta2) + binomial_logit_lpmf(Y[nn] | trials[nn], mu2[n]);
//       ptarget += log_sum_exp(ps);
//     }
//     return ptarget;
//   }
// }
// data {
//   int<lower=1> N;  // total number of observations
//   int Y[N];  // response variable
//   int trials[N];  // number of trials
//   vector[2] con_theta;  // prior concentration
//   int grainsize;  // grainsize for threading
//   // data for group-level effects of ID 1
//   int<lower=1> N_1;  // number of grouping levels
//   int<lower=1> M_1;  // number of coefficients per level
//   int<lower=1> J_1[N];  // grouping indicator per observation
//   // group-level predictor values
//   vector[N] Z_1_mu1_1;
//   // data for group-level effects of ID 2
//   int<lower=1> N_2;  // number of grouping levels
//   int<lower=1> M_2;  // number of coefficients per level
//   int<lower=1> J_2[N];  // grouping indicator per observation
//   // group-level predictor values
//   vector[N] Z_2_mu2_1;
//   int prior_only;  // should the likelihood be ignored?
// }
// transformed data {
//   int seq[N] = sequence(1, N);
// }
// parameters {
//   simplex[2] theta;  // mixing proportions
//   ordered[2] ordered_Intercept;  // to identify mixtures
//   vector<lower=0>[M_1] sd_1;  // group-level standard deviations
//   vector[N_1] z_1[M_1];  // standardized group-level effects
//   vector<lower=0>[M_2] sd_2;  // group-level standard deviations
//   vector[N_2] z_2[M_2];  // standardized group-level effects
// }
// transformed parameters {
//   // identify mixtures via ordering of the intercepts
//   real Intercept_mu1 = ordered_Intercept[1];
//   // identify mixtures via ordering of the intercepts
//   real Intercept_mu2 = ordered_Intercept[2];
//   // mixing proportions
//   real<lower=0,upper=1> theta1;
//   real<lower=0,upper=1> theta2;
//   vector[N_1] r_1_mu1_1;  // actual group-level effects
//   vector[N_2] r_2_mu2_1;  // actual group-level effects
//   theta1 = theta[1];
//   theta2 = theta[2];
//   r_1_mu1_1 = (sd_1[1] * (z_1[1]));
//   r_2_mu2_1 = (sd_2[1] * (z_2[1]));
// }
// model {
//   // likelihood including constants
//   if (!prior_only) {
//     target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, trials, Intercept_mu1, Intercept_mu2, theta1, theta2, J_1, Z_1_mu1_1, r_1_mu1_1, J_2, Z_2_mu2_1, r_2_mu2_1);
//   }
//   // priors including constants
//   target += normal_lpdf(Intercept_mu1 | 0, 0.001);
//   target += normal_lpdf(Intercept_mu2 | 0, 1);
//   target += dirichlet_lpdf(theta | con_theta);
//   target += student_t_lpdf(sd_1 | 3, 0, 4.4)
//     - 1 * student_t_lccdf(0 | 3, 0, 4.4);
//   target += std_normal_lpdf(z_1[1]);
//   target += student_t_lpdf(sd_2 | 3, 0, 4.4)
//     - 1 * student_t_lccdf(0 | 3, 0, 4.4);
//   target += std_normal_lpdf(z_2[1]);
// }
// generated quantities {
//   // actual population-level intercept
//   real b_mu1_Intercept = Intercept_mu1;
//   // actual population-level intercept
//   real b_mu2_Intercept = Intercept_mu2;
//   // additionally sample draws from priors
//   real prior_Intercept_mu1 = normal_rng(0,0.001);
//   real prior_Intercept_mu2 = normal_rng(0,1);
//   simplex[2] prior_theta = dirichlet_rng(con_theta);
//   real prior_sd_1 = student_t_rng(3,0,4.4);
//   real prior_sd_2 = student_t_rng(3,0,4.4);
//   // use rejection sampling for truncated priors
//   while (prior_sd_1 < 0) {
//     prior_sd_1 = student_t_rng(3,0,4.4);
//   }
//   while (prior_sd_2 < 0) {
//     prior_sd_2 = student_t_rng(3,0,4.4);
//   }
// }
