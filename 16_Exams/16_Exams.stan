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
  real<lower=.5,upper=1> phi; 
}

transformed parameters {
  vector[2] lp_parts[p];
  // Data Follow Binomial With Rate Given By Each Person's Group Assignment
  for (i in 1:p) {
    lp_parts[i,1] = log(.5) + binomial_lcdf(k[i] | n, phi);
    lp_parts[i,2] = log(.5) + binomial_lcdf(k[i] | n, psi); 
  }
}

model {
 
  for (i in 1:p)
    target += log_sum_exp(lp_parts[i]);  
}
generated quantities {
  int<lower = 0,upper = 1> z[p];
  
  for (i in 1:p) {
    vector[2] prob;
    prob = softmax(lp_parts[i]);
    z[i] = bernoulli_rng(prob[1]);
  }
}

generated quantities {
  real<lower=.5,upper=1> phiprior;
  phiprior = uniform_rng(.5,1);
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
//   real partial_log_lik_lpmf(int[] seq, int start, int end, data int[] Y, data int[] trials, real Intercept_mu1, real Intercept_mu2, real theta1, real theta2) {
//     real ptarget = 0;
//     int N = end - start + 1;
//     // initialize linear predictor term
//     vector[N] mu1 = Intercept_mu1 + rep_vector(0.0, N);
//     // initialize linear predictor term
//     vector[N] mu2 = Intercept_mu2 + rep_vector(0.0, N);
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
//   int prior_only;  // should the likelihood be ignored?
// }
// transformed data {
//   int seq[N] = sequence(1, N);
// }
// parameters {
//   simplex[2] theta;  // mixing proportions
//   ordered[2] ordered_Intercept;  // to identify mixtures
// }
// transformed parameters {
//   // identify mixtures via ordering of the intercepts
//   real Intercept_mu1 = ordered_Intercept[1];
//   // identify mixtures via ordering of the intercepts
//   real Intercept_mu2 = ordered_Intercept[2];
//   // mixing proportions
//   real<lower=0,upper=1> theta1;
//   real<lower=0,upper=1> theta2;
//   theta1 = theta[1];
//   theta2 = theta[2];
// }
// model {
//   // likelihood including constants
//   if (!prior_only) {
//     target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, trials, Intercept_mu1, Intercept_mu2, theta1, theta2);
//   }
//   // priors including constants
//   target += normal_lpdf(Intercept_mu1 | 0, 0.001);
//   target += normal_lpdf(Intercept_mu2 | 0, 1);
//   target += dirichlet_lpdf(theta | con_theta);
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
// }