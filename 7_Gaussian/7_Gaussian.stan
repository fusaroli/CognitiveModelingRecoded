// Inferringa Gaussian 

//## Notes to Stan model #######################################################
// 1) If parameter's prior distribution is not specified, Stan will assume that
//    you want it to be distributed uniformly with boundaries given by variable 
//    constraints. Here constrains <lower=0,upper=10> give uniform (0, 100)
// 2) In Stan, most of the sampling statements can be vectorized. In this example
//    you can see it in the statement for vector x. Instead of using for loop for  
//    each element of the vector, we can simple write it as above. This saves code, 
//    speeds up computation. For more information read Vectorization chapter in
//    the Stan manual (p.231 in version 2.4.0)
//##############################################################################

// The input data is a vector 'y' of length 'n'.
data {
  int<lower=0> n;
  vector[n] x;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu;
  real<lower=0,upper=10> sigma; 
}

// The model to be estimated. 
model {
  // Prior
  mu ~ normal(0, sqrt(1000));
  // Data Come From A Gaussian
  x ~ normal(mu, sigma);
}

generated quantities {
  real muprior;
  real<lower=0,upper=10> sigmaprior; 
  real priorpredk;
  real postpredk;
    
  muprior = normal_rng(0, sqrt(1000));
  // Prior Predictive
  priorpredk = normal_rng(muprior, sigmaprior);
  // Posterior Predictive
  postpredk = normal_rng(mu, sigma);
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
//   real partial_log_lik_lpmf(int[] seq, int start, int end, data vector Y, real Intercept, real sigma) {
//     real ptarget = 0;
//     int N = end - start + 1;
//     // initialize linear predictor term
//     vector[N] mu = Intercept + rep_vector(0.0, N);
//     ptarget += normal_lpdf(Y[start:end] | mu, sigma);
//     return ptarget;
//   }
// }
// data {
//   int<lower=1> N;  // total number of observations
//   vector[N] Y;  // response variable
//   int grainsize;  // grainsize for threading
//   int prior_only;  // should the likelihood be ignored?
// }
// transformed data {
//   int seq[N] = sequence(1, N);
// }
// parameters {
//   real Intercept;  // temporary intercept for centered predictors
//   real<lower=0> sigma;  // dispersion parameter
// }
// transformed parameters {
// }
// model {
//   // likelihood including constants
//   if (!prior_only) {
//     target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, Intercept, sigma);
//   }
//   // priors including constants
//   target += normal_lpdf(Intercept | 0, sqrt(1000));
//   target += uniform_lpdf(sigma | 0, 10)
//     - 1 * uniform_lccdf(0 | 0, 10);
// }
// generated quantities {
//   // actual population-level intercept
//   real b_Intercept = Intercept;
//   // additionally sample draws from priors
//   real prior_Intercept = normal_rng(0,sqrt(1000));
//   real prior_sigma = uniform_rng(0,10);
//   // use rejection sampling for truncated priors
//   while (prior_sigma < 0) {
//     prior_sigma = uniform_rng(0,10);
//   }
// }