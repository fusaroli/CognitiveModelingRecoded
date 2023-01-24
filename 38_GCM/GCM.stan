// Generalized Context Model
data { 
  int nstim;                   // n of stimuli (8)
  int t;                       // n of datapoints (8 stimuli per 40 participants)
  array[nstim] int a;                // category for each stimulus (1 or 2)
  array[nstim] int y;                // decision (how many time was that stimulus judged as category 1)
  matrix[nstim,nstim] d1;      // similarity for dimension 1
  matrix[nstim,nstim] d2;      // similarity for dimension 2
}

transformed data {
  real<lower=0,upper=1> b;     // bias as a real number between 0 and 1
  b = .5;                      // we fix it at 0.5 (no bias)
}

parameters {
  real<lower=0,upper=5> c;    // scaling parameter (how fast similarity decrease with distance). Why is upper 5??
  real<lower=0,upper=1> w;    // weight parameter (how much attention should be paid to feature 1 related to feature 2 - summing up to 1)
} 

transformed parameters {
  vector<lower=0,upper=1>[nstim] r; // probability of choosing category 1
  array[nstim,nstim,2] real tmp1;         // similarity for category 1 NB. this includes all stimuli, gets subsetted later
  array[nstim,nstim,2] real tmp2;         // similarity for category 2 NB. this includes all stimuli, gets subsetted later
  
  for (i in 1:nstim) {
    vector[nstim] numerator;        // similarity with stimuli of category 1
    vector[nstim] denominator;      // similarity with stimuli of category 1 + similarity with those of cat 2
    for (j in 1:nstim) {
      real s;                       // similarity estimate
      // Similarities
      s = exp(-c * (w * d1[i,j] + (1 - w) * d2[i,j])); // estimating similarity from distance on both features
      // Decision Probabilities
      tmp1[i,j,1] = b * s;                             // including bias for category 1
      tmp1[i,j,2] = 0;
      tmp2[i,j,1] = 0;
      tmp2[i,j,2] = (1 - b) * s;                       // including bias for category 2
      
      numerator[j] = tmp1[i,j,a[j]];                   // take all similarity for cat 1 (and zeroes those for cat 2)             
      denominator[j] = tmp1[i,j,a[j]] + tmp2[i,j,a[j]];// numerator + take all similarity for cat 2 (and zeroes those for cat 1)
    }
    r[i] = sum(numerator) / sum(denominator);
  }  
}

model {
  // Prior
  target += beta_lpdf(w | 1, 1);
  // Missing a prior for c
  // target += beta_lpdf(c | 1, 1); Figure out how to bound at 0-5
  
  
  // Decision Data
  target += binomial_lpmf(y | t, r);
}

generated quantities {
  real<lower=0, upper=1> w_prior;
  // real<lower=0, upper=5> c_prior; MISSING
  
  // real priorpredk;
  vector[nstim] predy;
  
  // Prior
  w_prior = beta_rng(1, 1);
  // c_prior = beta_rng(1, 1) * 5; MISSING
  
  // Prior Predictive MISSING. I NEED TO SETUP THE WHOLE REPARAMETRIZATION HERE
  // priorpredk = binomial_rng(t, r_prior);
  
  for (i in 1:nstim)
    predy[i] = binomial_rng(t, r[i]);
}



