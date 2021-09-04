// Inferring Return Rate and Number of Surveys from Observed Returns

// N.B. This model is more cumbersome than the model in the book, 
// as Stan does not directly support discrete distributions

// Here we define the observed variables
data { 
  int<lower=0> nmax; // max n of surveys
  int<lower=0> m; // n of people distributing the surveys
  int<lower=0,upper=nmax> k[m]; // n of returned surveys
}

transformed data {
  int<lower=0> nmin;  // Minimal possible n
  nmin = max(k);
}

// the model has only one directly modeled parameter, theta
parameters {
  real<lower=0,upper=1> theta;
}

transformed parameters {
  vector[nmax] lp_parts;  // Log probability for each n

  // First part of the trick for mixture model
  for (n in 1:nmax)
    if (n < nmin)
      lp_parts[n] = log(1.0 / nmax) + negative_infinity();  // Zero probability
    else
      lp_parts[n] = log(1.0 / nmax) + binomial_lpmf(k | n, theta);
}

model {
  // Second part of the trick for mixture model
  target += log_sum_exp(lp_parts);
}

generated quantities {
  int<lower=1,upper=nmax> n;
  simplex[nmax] prob_n;
  
  // Transforming lp_parts to probabilities of each n
  prob_n = softmax(lp_parts);
  n = categorical_rng(prob_n);
}

