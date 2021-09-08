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

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  y ~ normal(mu, sigma);
}

