// Generalized Context Model With Contaminants and Two Attention Groups

data { 
  int nstim;  // n of stimuli (8)
  int nsubj;  // n of subjects (40)
  int n;      // ??
  array[nstim] int a; // category for each stimulus (1 or 2)
  array[nsubj,nstim] int y; // decision (stimuli b)
  matrix[nstim,nstim] d1;   // similarity for dimension 1
  matrix[nstim,nstim] d2;   // similarity for dimension 2
}

transformed data {
  real<lower=0,upper=1> b;     // bias as a real number between 0 and 1
  b = .5;                      // we fix it at 0.5 (no bias)
}

parameters {
  vector<lower=0>[nsubj] c;   // scaling parameter (how fast similarity decrease with distance). 
  vector<lower=0,upper=1>[nsubj] w; // weight parameter (how much attention should be paid to feature 1 related to feature 2 - summing up to 1)
  vector<lower=0>[2] cpredg;
  vector<lower=0,upper=1>[2] wpredg;  
  real<lower=0,upper=1> phic;
  real<lower=0,upper=1> phig;
  real<lower=0,upper=1> muctmp;
  real<lower=0,upper=1> muwtmp;
  real<lower=0,upper=1> delta;
  real<lower=0,upper=1> sigmactmp;
  real<lower=0,upper=1> sigmawtmp;
} 


transformed parameters {
  matrix<lower=0,upper=1>[nsubj,nstim] r; 
  real<lower=0,upper=5> muc;
  vector<lower=0,upper=1>[2] muw;
  real<lower=.01,upper=3> sigmac;
  real<lower=.01,upper=1> sigmaw;
  vector[2] lp_parts_c[nsubj];
  vector[2] lp_parts_g[nsubj];
  // Mean Generalization
  muc = 5 * muctmp;
  // Mean Attention
  muw[1] = muwtmp;
  muw[2] = fmin(1, delta + muw[1]);
  // Standard Deviation Generalization
  sigmac = fmax(.01, 3 * sigmactmp);
  // Standard Deviation Attention
  sigmaw = fmax(.01, sigmawtmp);
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
      r[k,i] = sum(numerator) / sum(denominator);
    } 
  }  
  for (k in 1:nsubj) { 
    lp_parts_g[k,1] = log1m(phig) + normal_lpdf(w[k] | muw[1], sigmaw)
                       - log(normal_cdf(1 | muw[1], sigmaw) 
                             - normal_cdf(0 | muw[1], sigmaw));
    lp_parts_g[k,2] = log(phig) + normal_lpdf(w[k] | muw[2], sigmaw)
                       - log(normal_cdf(1 | muw[2], sigmaw) 
                             - normal_cdf(0 | muw[2], sigmaw));
  }
  for (k in 1:nsubj) {
    lp_parts_c[k,1] = log1m(phic) + binomial_lpmf(y[k] | n, r[k]);
    lp_parts_c[k,2] = log(phic) + binomial_lpmf(y[k] | n, .5);
  }
}
model {
  // Subject Parameters
  for (k in 1:nsubj)
    c[k] ~ normal(muc, sigmac)T[0,];
  // Predicted Group Parameters
  for (g in 1:2) {
    wpredg[g] ~ normal(muw[g], sigmaw)T[0,1];
    cpredg[g] ~ normal(muc, sigmac)T[0,];
  }
  // Decision Data
  for (k in 1:nsubj) 
    target += log_sum_exp(lp_parts_g[k]);
  for (k in 1:nsubj)   
    target += log_sum_exp(lp_parts_c[k]);
}
generated quantities {
  matrix[3,nstim] predyg;
  vector<lower=0,upper=3>[nsubj] z;
  for (i in 1:nstim) {
    matrix[2,nstim] numeratorpredg;
    matrix[2,nstim] denominatorpredg;
    vector[3] rpredg;
    for (j in 1:nstim) { 
      for (g in 1:2) {
        real spredg;
        spredg = exp(-cpredg[g] * (wpredg[g] * d1[i,j] 
                                    + (1 - wpredg[g]) * d2[i,j]));
        numeratorpredg[g,j]   = (a[j] == 1) * b * spredg;
        denominatorpredg[g,j] = (a[j] == 1) * b * spredg
                                  + (a[j] == 2) * (1 - b) * spredg;
      }      
    }
    for (g in 1:2)
      rpredg[g] = sum(numeratorpredg[g]) / sum(denominatorpredg[g]); 
    rpredg[3] = 0.5;
    
    // Groups
    for (g in 1:3)
      predyg[g,i] = binomial_rng(n, rpredg[g]);
  }
  for (k in 1:nsubj) {
    vector[2] prob_c;
    vector[2] prob_g;
    int zc;
    int zg;
    prob_c = softmax(lp_parts_c[k]);
    prob_g = softmax(lp_parts_g[k]);
    zc = bernoulli_rng(prob_c[2]);
    zg = bernoulli_rng(prob_g[2]);
    z[k] = (zc == 0) * (zg + 1) + 3 * (zc == 1);
  }
}

