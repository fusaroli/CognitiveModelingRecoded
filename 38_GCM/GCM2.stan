// Generalized Context Model With Individual Differences
data { 
  int nstim;  // n of stimuli (8)
  int nsubj;  // n of subjects (40)
  int n;      // ??
  array[nstim] int a; // category for each stimulus (1 or 2)
  array[nstim,nsubj] int y; // decision (stimuli b)
  matrix[nstim,nstim] d1;   // distance for dimension 1
  matrix[nstim,nstim] d2;   // distance for dimension 2
}

transformed data {
  real<lower=0,upper=1> b;     // bias as a real number between 0 and 1
  b = .5;                      // we fix it at 0.5 (no bias)
}

parameters {
  vector<lower=0,upper=5>[nsubj] c;    // scaling parameter (how fast similarity decrease with distance). Why is upper 5??
  vector<lower=0,upper=1>[nsubj] w;    // weight parameter (how much attention should be paid to feature 1 related to feature 2 - summing up to 1)
} 

transformed parameters {
  matrix<lower=0,upper=1>[nstim,nsubj] r; // probability of choosing category 1
  
  for (i in 1:nstim) {
    vector[nstim] numerator;
    vector[nstim] denominator;
    for (k in 1:nsubj) {
      for (j in 1:nstim) {
        real s;
        // Similarities
        s = exp(-c[k] * (w[k] * d1[i,j] + (1 - w[k]) * d2[i,j])); 
        
        // Base Decision Probabilities
        numerator[j] = (a[j] == 1) * b * s;
        denominator[j] = (a[j] == 1) * b * s + (a[j] == 2) * (1 - b) * s;
      }
      // Decision Probabilities
      r[i,k] = sum(numerator) / sum(denominator);
    } 
  }  
}

model {
  // priors
  target += beta_lpdf(w | 1, 1);
  // target += beta_lpdf(c | 1, 1) * 5; // MISSING
  
  // Decision Data
  for (i in 1:nstim)
    y[i] ~ binomial(n, r[i]);
}

generated quantities {
  matrix[nstim,nsubj] predy;
  
  for (i in 1:nstim)
    for (k in 1:nsubj)
      predy[i,k] = binomial_rng(n, r[i,k]);
}

