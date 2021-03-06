// ## Notes to Stan model #######################################################
//  1) This is the first time we use simplex data type. Simplex is similar to 
//     vector, but with a property that sum of all it's elements is equal to 1.
// 2) Sampling statements for parameters alpha, beta and gamma could be removed
//    leading to uniform prior on (0, 1) interval which is the same as beta(1, 1)
// 3) Variable n was removed here. Stan doesn't need this information as
//    an argument for multinomial distribution. Always make sure that you know 
//    what arguments are required for a function / sampling statement. In many 
//    cases these are different from BUGS. Very useful for this are last pages 
//    of Stan manual
// #############################################################################

data { 
  int<lower=0> n;
  int<lower=0> y[n];
}

parameters {
  // Underlying Rates
  // Rate Objective Method Decides 'one'
  real<lower=0,upper=1> alpha;
  // Rate Surrogate Method Decides 'one' When Objective Method Decides 'one'
  real<lower=0,upper=1> beta;
  // Rate Surrogate Method Decides 'zero' When Objective Method Decides 'zero'
  real<lower=0,upper=1> gamma;
  
} 

transformed parameters {
  
  simplex[n] pi;
  real xi;
  real psi;
  real kappa;
  
  
  // Probabilities For Each Count
  pi[1] = alpha * beta;
  pi[2] = alpha * (1 - beta);
  pi[3] = (1 - alpha) * (1 - gamma);
  pi[4] = (1 - alpha) * gamma;
    
  // Derived Measures   
  // Rate Surrogate Method Agrees With the Objective Method
  xi = alpha * beta + (1 - alpha) * gamma ;
  
  // Rate of Chance Agreement
  psi = (pi[1] + pi[2]) * (pi[1] + pi[3]) + (pi[2] + pi[4]) * (pi[3] + pi[4]);  
  
  // Chance-Corrected Agreement
  kappa = (xi - psi) / (1 - psi);
  
  
}

model {
  target += beta_lpdf(alpha | 1, 1);  // could be removed
  target += beta_lpdf(beta | 1, 1);  // could be removed
  target += beta_lpdf(gamma | 1, 1);  // could be removed
  // Count Data     
  target += multinomial_lpmf(y | pi);
  
}

generated quantities{
  real<lower=0,upper=1> alphaprior;
  real<lower=0,upper=1> betaprior;
  real<lower=0,upper=1> gammaprior;
  simplex[n] piprior;
  real xiprior;
  real psiprior;
  real kappaprior;
  
  alphaprior = beta_rng(1, 1);  // could be removed
  betaprior = beta_rng(1, 1);  // could be removed
  gammaprior = beta_rng(1, 1);  // could be removed
  
  // Probabilities For Each Count
  piprior[1] = alphaprior * betaprior;
  piprior[2] = alphaprior * (1 - betaprior);
  piprior[3] = (1 - alphaprior) * (1 - gammaprior);
  piprior[4] = (1 - alphaprior) * gammaprior;
    
  // Derived Measures   
  // Rate Surrogate Method Agrees With the Objective Method
  xiprior = alphaprior * betaprior + (1 - alphaprior) * gammaprior ;
  
  // Rate of Chance Agreement
  psiprior = (piprior[1] + piprior[2]) * (piprior[1] + piprior[3]) + (piprior[2] + piprior[4]) * (piprior[3] + piprior[4]);  
  
  // Chance-Corrected Agreement
  kappaprior = (xiprior - psiprior) / (1 - psiprior);
}