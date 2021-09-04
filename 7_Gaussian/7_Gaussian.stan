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
  real muprior;
  real<lower=0,upper=10> sigma; 
  real<lower=0,upper=10> sigmaprior; 
}

// The model to be estimated. 
model {
  // Prior
  mu ~ normal(0, sqrt(1000));
  muprior ~ normal(0, sqrt(1000));
  // Data Come From A Gaussian
  x ~ normal(mu, sigma);
}

generated quantities {
  real priorpredk;
  real postpredk;
    
  // Prior Predictive
  priorpredk = normal_rng(muprior, sigmaprior);
  // Posterior Predictive
  postpredk = normal_rng(mu, sigma);
}