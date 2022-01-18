// Twenty Questions w missing data
data { 
  int<lower=0> np;  // rows: people
  int<lower=0> nq;  // columns: questions
  int k[np,nq];     // correct answer
  int k_na[np,nq];  // locating NAs in k
  int n_na;         // number of NAs
}
parameters {
  real<lower=0,upper=1> p[np];  // rate by person
  real<lower=0,upper=1> q[nq];  // rate by question
} 
transformed parameters {
  real<lower=0,upper=1> theta[np,nq];
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
      if (k_na[i,j] == 0)     // If k[i,j] is not missing
        target += bernoulli_lpmf(k[i, j] | theta[i,j]);
}
generated quantities {
  int na_array[n_na];
  int index;
  
  index = 1;
  for (i in 1:np) {
    for (j in 1:nq) {   
      if (k_na[i,j] == 1) {   // If k[i,j] is missing
        na_array[index] = bernoulli_rng(theta[i,j]);
        index = index + 1;
      }
    }
  }
}